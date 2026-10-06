-- | Recording model calls to a file, and replaying them.
--
-- > rt <- pure runtime >>= withSystemOne jev >>= withSystemTwo anthropic >>= withStore ReplayOrRecord "dino.jsonl"
--
-- Every System One and System Two call is a request: a t'Conversation' or a
-- t'JudgeRequest'. The store keys each answer by its whole request, so an answer
-- is replayed exactly when the model would be asked exactly the same thing.
-- Change an instruction or a threshold upstream and only the calls it affects
-- go to the model again.
--
-- Only model calls are stored. @act@ steps and tool bodies run for real, even
-- when replaying.
module Agentic.IO.Store
  ( Mode (..)
  , withStore
  , StoreError (..)
  ) where

import Agentic.Aeson (fromAeson)
import Agentic.Core (Instruction (..), Note (..))
import Agentic.JsonSchema (jsonSchema)
import Agentic.Questions
import Agentic.Runtime
import Agentic.Value (Value (..), lookupField, renderJson)
import Control.Concurrent.MVar
import Control.Exception (Exception (..), throwIO)
import Control.Monad (when)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.IORef
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (doesFileExist)
import System.IO (IOMode (..), hFlush, withFile)

data Mode
  = Record
    -- ^ Call the models and record every answer, starting the file afresh.
  | Replay
    -- ^ Answer only from the file. A request that isn't there is a t'StoreMiss'.
  | ReplayOrRecord
    -- ^ Answer from the file when it can, and call the models (and record the
    -- answer) when it can't.
  deriving (Eq, Show)

data StoreError
  = StoreMiss FilePath Text
    -- ^ A replayed run asked something the recording doesn't have.
  | StoreUnreadable FilePath Text
    -- ^ The recording has an answer that can't be read back.
  deriving (Show)

instance Exception StoreError where
  displayException = \case
    StoreMiss file what ->
      "The recording " <> file <> " has no answer for " <> T.unpack what
        <> ". Something upstream changed; record again, or use ReplayOrRecord."
    StoreUnreadable file what -> "The recording " <> file <> " has " <> T.unpack what <> " that can't be read; record again."

-- | Wrap a runtime's System One and System Two with a store in @file@.
withStore :: Mode -> FilePath -> Runtime IO -> IO (Runtime IO)
withStore mode file rt = do
  existing <- if mode == Record then pure [] else load file
  when (mode == Record) (writeFile file "")
  answers <- newIORef (Map.fromList [(canonical k, a) | (k, a) <- existing])
  lock <- newMVar ()
  let lookupOr key what live = do
        known <- Map.lookup (canonical key) <$> readIORef answers
        case (known, mode) of
          (Just answer, _) | mode /= Record -> pure answer
          (_, Replay) -> throwIO (StoreMiss file what)
          _ -> do
            answer <- live
            atomicModifyIORef' answers (\m -> (Map.insert (canonical key) answer m, ()))
            withMVar lock $ \_ -> append file key answer
            pure answer
      one request =
        lookupOr (judgeKey request) ("a judgement: " <> questionsText request) (encodeStoredAnswers <$> rt.systemOne.ask request)
          >>= decodedAs "answers to a judgement" decodeStoredAnswers
      two conversation =
        lookupOr (turnKey conversation) ("a turn of: " <> conversation.instruction.text) (encodeStoredTurn <$> rt.systemTwo.ask conversation)
          >>= decodedAs "a turn" decodeStoredTurn
  pure rt {systemOne = SystemOne one, systemTwo = SystemTwo two}
  where
    decodedAs :: Text -> (Value -> Maybe a) -> Value -> IO a
    decodedAs what decode' = maybe (throwIO (StoreUnreadable file what)) pure . decode'
    questionsText r = T.intercalate "; " (map question r.questions)
    question = \case
      AskYesNo q -> q
      AskChoice q _ -> q
      AskScore q _ -> q

-- ---------------------------------------------------------------------------
-- The file: one JSON object per line, {"request": …, "answer": …}

load :: FilePath -> IO [(Value, Value)]
load file = do
  exists <- doesFileExist file
  if not exists
    then pure []
    else do
      contents <- LBS.readFile file
      pure [entry | line <- LBS.lines contents, not (LBS.null line), Just entry <- [parse line]]
  where
    parse line = do
      v <- fromAeson <$> J.decode line
      case v of
        Object kvs -> (,) <$> lookupField "request" kvs <*> lookupField "answer" kvs
        _ -> Nothing

append :: FilePath -> Value -> Value -> IO ()
append file key answer = withFile file AppendMode $ \h -> do
  T.hPutStrLn h (renderJson (Object [("request", key), ("answer", answer)]))
  hFlush h

-- ---------------------------------------------------------------------------
-- Keys

-- | Keys are compared in a canonical form: object keys sorted, whole numbers as
-- integers. Reading the file back through aeson reorders keys, and order
-- doesn't change which request it is.
canonical :: Value -> Value
canonical = \case
  Object kvs -> Object (sortOn fst [(k, canonical v) | (k, v) <- kvs])
  Array vs -> Array (map canonical vs)
  Number d | d == fromInteger (round d) -> Integer (round d)
  v -> v

turnKey :: Conversation -> Value
turnKey c =
  Object
    [ ("kind", String "turn")
    , ("path", Array [String n.name | n <- c.path])
    , ("instruction", String c.instruction.text)
    , ("input", c.input)
    , ("inputSchema", jsonSchema c.inputSchema)
    , ("tools", Array [Object [("name", String t.name), ("description", String t.description), ("input", jsonSchema t.input)] | t <- c.tools])
    , ("outputSchema", jsonSchema c.outputSchema)
    , ("history", Array (map exchange c.history))
    ]
  where
    exchange = \case
      Called (Raw r) results -> Object [("called", r), ("results", Array [Object [("id", String i), ("result", toolResult res)] | (i, res) <- results])]
      Rejected (Raw r) problem -> Object [("rejected", r), ("problem", String problem)]
    toolResult = \case
      ToolOk v -> Object [("ok", v)]
      ToolFailed t -> Object [("failed", String t)]

judgeKey :: JudgeRequest -> Value
judgeKey r = Object [("kind", String "judgement"), ("input", r.input), ("questions", Array (map spec r.questions))]
  where
    spec = \case
      AskYesNo q -> Object [("yesNo", String q)]
      AskChoice q opts -> Object [("choice", String q), ("options", labelled opts)]
      AskScore q levels -> Object [("score", String q), ("levels", labelled levels)]
    labelled xs = Array [Object [("label", String l), ("description", maybe Null String d)] | (l, d) <- xs]

-- ---------------------------------------------------------------------------
-- Answers

encodeStoredTurn :: Turn -> Value
encodeStoredTurn (Turn (Raw r) a) = Object [("raw", r), ("action", act a)]
  where
    act = \case
      CallTools calls -> Object [("callTools", Array [Object [("id", String c.callId), ("name", String c.name), ("input", c.input)] | c <- calls])]
      Respond v -> Object [("respond", v)]

decodeStoredTurn :: Value -> Maybe Turn
decodeStoredTurn = \case
  Object kvs -> do
    r <- lookupField "raw" kvs
    a <- lookupField "action" kvs
    Turn (Raw r) <$> act a
  _ -> Nothing
  where
    act = \case
      Object [("respond", v)] -> Just (Respond v)
      Object [("callTools", Array calls)] -> CallTools <$> traverse call calls
      _ -> Nothing
    call = \case
      Object kvs -> ToolCall <$> text "id" kvs <*> text "name" kvs <*> lookupField "input" kvs
      _ -> Nothing
    text k kvs = case lookupField k kvs of
      Just (String t) -> Just t
      _ -> Nothing

encodeStoredAnswers :: [Answer] -> Value
encodeStoredAnswers = Array . map answer
  where
    answer = \case
      YesNoAnswer p -> Object [("yesNo", prob p)]
      ChoiceAnswer l ps c -> Object [("choice", String l), ("probabilities", Array [Array [String x, prob p] | (x, p) <- ps]), ("confidence", prob c)]
      ScoreAnswer pos ps c -> Object [("score", Number pos), ("probabilities", Array [Array [Integer (toInteger i), prob p] | (i, p) <- ps]), ("confidence", prob c)]
    prob = Integer . toInteger . basisPoints

decodeStoredAnswers :: Value -> Maybe [Answer]
decodeStoredAnswers = \case
  Array xs -> traverse answer xs
  _ -> Nothing
  where
    -- Look fields up by name: the file comes back through aeson, which
    -- reorders keys.
    answer = \case
      Object kvs
        | Just p <- lookupField "yesNo" kvs -> YesNoAnswer <$> prob p
        | Just (String l) <- lookupField "choice" kvs ->
            ChoiceAnswer l <$> (pairs label =<< lookupField "probabilities" kvs) <*> (prob =<< lookupField "confidence" kvs)
        | Just pos <- lookupField "score" kvs ->
            ScoreAnswer <$> number pos <*> (pairs index =<< lookupField "probabilities" kvs) <*> (prob =<< lookupField "confidence" kvs)
      _ -> Nothing
    pairs key = \case
      Array ps -> traverse (\case Array [k, p] -> (,) <$> key k <*> prob p; _ -> Nothing) ps
      _ -> Nothing
    label = \case
      String t -> Just t
      _ -> Nothing
    index = \case
      Integer i -> Just (fromInteger i)
      _ -> Nothing
    prob = \case
      Integer bp -> Just (fromBasisPoints (fromInteger bp))
      _ -> Nothing
    number = \case
      Number d -> Just d
      Integer n -> Just (fromInteger n)
      _ -> Nothing
