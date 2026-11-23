-- | A deliberately simple JSONL completion journal. One process/runtime owns
-- the file at a time; this is a caller obligation, not a lock. Writes within
-- that runtime are serialised. Every unreadable line (including an incomplete
-- final line) fails loading. No repair, cross-process locking, or power-loss
-- guarantee. Acknowledgement follows append, flush and close, before publishing
-- to the in-memory index. After an IO write failure, discard this handle and
-- reopen the file; the failure may have left a readable or partial record.
module Agentic.IO.Journal (openJournal, withJournal) where

import Agentic.Journal
import Agentic.Runtime (Runtime (..), FlowError (..))
import Agentic.Value
import Agentic.Aeson (fromAeson)
import Control.Concurrent.MVar
import Control.Exception (try, throwIO, toException, displayException)
import Control.Monad (foldM, unless)
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Directory (doesFileExist)
import System.IO (withBinaryFile, IOMode (AppendMode), hFlush)

-- | Open or create one run's journal. Changing either identity is an error.
-- Opening a new store object reloads the file; it never shares a process cache.
openJournal :: FilePath -> JournalIdentity -> IO (Journal IO)
openJournal file identity = do
  let header = Object [("journal",Integer 1),("run",String identity.namespace)
                      ,("revision",String identity.revision)]
  exists <- doesFileExist file
  entries <- if exists then load header else append header >> pure Map.empty
  state <- newMVar (Right entries :: Either FlowError (Map.Map Value Value))
  let initialise = pure () -- openJournal already checked the header
      lookupRecord key = do
        k <- canonical key
        withMVar state $ either throwIO (pure . Map.lookup k)
      saveRecord key record = do
        k <- canonical key
        v <- canonical record
        -- Never roll a successful append back in the in-memory index. A write
        -- failure poisons this handle rather than turning future reads into misses.
        result <- modifyMVarMasked state $ \case
          Left e -> pure (Left e,Left (toException e))
          Right records -> case Map.lookup k records of
            Just old | old == v -> pure (Right records,Right ())
                     | otherwise -> do
                         r <- try (raise (JournalConflict k))
                         pure (Right records,r)
            Nothing -> do
              r <- try (append (Object [("key",k),("record",v)]))
              case r of
                Left e -> pure
                  (Left (JournalFailure (JournalWriteFailed file (T.pack (displayException e)))),Left e)
                Right () -> pure (Right (Map.insert k v records),Right ())
        either throwIO pure result
  pure Journal{identity,initialise,lookupRecord,saveRecord}
 where
  raise = throwIO . JournalFailure
  canonical = either (raise . JournalInvalidValue) pure . canonicalValue
  append value = do
    c <- canonical value
    -- canonicalValue has already forced all content, before opening the file.
    let bytes = TE.encodeUtf8 (renderJson c) <> "\n"
    BS.length bytes `seq` withBinaryFile file AppendMode (\h -> BS.hPut h bytes >> hFlush h)
  unreadable n = raise . JournalUnreadable file n
  load header = do
    bytes <- BS.readFile file
    unless (not (BS.null bytes) && BS.last bytes == 10) $
      unreadable 1 "empty journal or incomplete final line"
    values <- traverse parse (zip [1..] (BS8.lines bytes))
    case values of
      h:rows -> do
        expected <- canonical header
        unless (h == expected) (raise (JournalMismatch "journal run, revision or format differs"))
        foldM insert Map.empty (zip [2..] rows)
      [] -> unreadable 1 "missing header"
  parse (n,line) = case J.eitherDecodeStrict' line of
    Left problem -> unreadable n (T.pack problem)
    Right value -> either (unreadable n) pure (canonicalValue (fromAeson value))
  insert records (n,Object fields) = case (lookupField "key" fields,lookupField "record" fields) of
    (Just key,Just record@(Object fs))
      | Just _ <- lookupField "request" fs, Just _ <- lookupField "result" fs ->
        case Map.lookup key records of
          Nothing -> pure (Map.insert key record records)
          Just old | old == record -> pure records
                   | otherwise -> raise (JournalConflict key)
    _ -> unreadable n "missing key or completion fields"
  insert _ (n,_) = unreadable n "expected a journal record"

-- | Install the reference journal. Use 'openJournal' to compose your own runtime.
withJournal :: FilePath -> JournalIdentity -> Runtime IO -> IO (Runtime IO)
withJournal file identity rt = do
  j <- openJournal file identity
  pure rt{journal=Just j}
