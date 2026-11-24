-- Tests the real flow and the executable in separate processes, without a model
-- provider or email service. Run with cabal test email-approval-test.
module Main (main) where

import Agentic hiding (key)
import Agentic.IO (withJournal)
import Agentic.Runtime (Conversation (..))
import Agentic.Scripted (modelContract, respond)
import EmailApproval qualified as E
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.IORef
import Data.List (nub, isInfixOf)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.ByteString qualified as BS
import System.Directory
import System.FilePath ((</>))
import System.IO (openTempFile, hClose)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

check :: String -> Bool -> IO ()
check label condition = unless condition (fail label)

withWorkspace :: (FilePath -> IO a) -> IO a
withWorkspace = bracket create removeDirectoryRecursive
 where
  create = do
    temp <- getTemporaryDirectory
    (path,h) <- openTempFile temp "agentic-email-approval"
    hClose h
    removeFile path
    createDirectory path
    pure path

oneWait :: Show a => Progress a -> IO PendingWait
oneWait (RunWaiting (w NE.:| [])) = pure w
oneWait other = fail ("expected one wait: " <> show other)

flowTest :: FilePath -> IO ()
flowTest directory = do
  requests <- newIORef []
  deliveries <- newIORef []
  let brief = "Tell Sam the café opens on Friday."
      mail :: Int -> E.Email
      mail n = E.Email ("Draft " <> T.pack (show n)) ("Body " <> T.pack (show n))
      provider = SystemTwo $ \c -> do
        request <- either (fail . T.unpack) pure ((modelContract @E.DraftRequest).decode c.input)
        seen <- readIORef requests
        writeIORef requests (seen <> [request])
        pure (Turn (Raw Null) (respond (mail (length seen+1))))
      sender stableKey email = modifyIORef' deliveries (<> [(stableKey,email)])
      flow = E.emailApproval sender
      -- Reload the file on every attempt, as a new process would.
      open = withJournal (directory </> "flow.jsonl") (JournalIdentity "email-test" "v1")
        runtime{systemTwo=provider}
      attempt = open >>= \rt -> resumable rt flow brief
      answer w decision = do
        rt <- open
        roundTrip <- either (fail . T.unpack) pure (pendingWaitFromValue (pendingWaitValue w))
        accepted <- fulfil rt flow roundTrip (E.decisionCodec.encode decision)
        check "reply refused" (accepted == Right ())
  firstWait <- attempt >>= oneWait
  answer firstWait (E.RequestChanges "Make it warmer.")
  secondWait <- attempt >>= oneWait
  answer secondWait (E.RequestChanges "Make it shorter.")
  third <- attempt >>= oneWait
  check "revisions reused a wait key" (length (nub [firstWait.key,secondWait.key,third.key]) == 3)
  seen <- readIORef requests
  check "feedback did not carry the exact previous draft and original brief" $
    seen == [E.DraftRequest brief Nothing ""
            ,E.DraftRequest brief (Just (mail 1)) "Make it warmer."
            ,E.DraftRequest brief (Just (mail 2)) "Make it shorter."]
  _ <- attempt >>= oneWait
  afterReplay <- readIORef requests
  check "waiting replay drafted again" (length afterReplay == 3)
  answer third E.Approve
  result <- attempt
  check "did not send the reviewed draft" (result == RunFinished (E.EmailSent (mail 3)))
  answer third E.Approve
  repeated <- attempt
  check "completed replay changed the outcome" (repeated == result)
  sent <- readIORef deliveries
  check "sender did not get an idempotency key or sent more than once" $
    case sent of [(Just _,email)] -> email == mail 3; _ -> False
  afterSend <- readIORef requests
  check "approval regenerated a draft" (length afterSend == 3)

cli :: [String] -> IO String
cli args = do
  (status,out,err) <- readProcessWithExitCode "email-approval" args ""
  check ("command failed: " <> show args <> "\n" <> err) (status == ExitSuccess)
  pure out

refused :: [String] -> String -> IO String
refused args message = do
  (status,out,err) <- readProcessWithExitCode "email-approval" args ""
  check ("expected refusal: " <> show args) (status /= ExitSuccess && message `isInfixOf` err)
  check "refusal printed an exception backtrace" (not ("Uncaught exception" `isInfixOf` err))
  pure out

cliTests :: FilePath -> IO ()
cliTests root = do
  let directory = root </> "approval"
      journal = directory </> "journal.jsonl"
  initial <- cli ["start",directory,"--demo","Tell Sam the café opens on Friday."]
  check "start did not suspend" ("[demo] Drafting email." `isInfixOf` initial
    && "Awaiting approval for draft 0." `isInfixOf` initial
    && not ("FAKE SEND" `isInfixOf` initial))
  originalPending <- BS.readFile (E.pendingFile directory 0)
  originalJournal <- BS.readFile journal
  resumed <- cli ["resume",directory]
  check "resume generated another draft" (not ("[demo]" `isInfixOf` resumed))
  samePending <- BS.readFile (E.pendingFile directory 0)
  check "resume changed the pending descriptor" (samePending == originalPending)
  _ <- refused ["start",directory,"new brief"] "already exists"
  _ <- refused ["start",root </> "empty",""] "provide a nonempty email brief"
  _ <- refused [] "usage: email-approval"
  afterStart <- BS.readFile journal
  check "failed start changed the journal" (afterStart == originalJournal)
  _ <- refused ["reply",directory,"0","revise",""] "Reply refused:"
  afterInvalid <- BS.readFile journal
  check "invalid reply changed the journal" (afterInvalid == originalJournal)
  revision <- cli ["reply",directory,"0","revise","Make it warmer."]
  check "revision did not ask again" ("Awaiting approval for draft 1." `isInfixOf` revision
    && "[demo] Drafting email." `isInfixOf` revision)
  firstWait <- E.readValue (E.pendingFile directory 0) >>= either (fail . T.unpack) pure . pendingWaitFromValue
  secondWait <- E.readValue (E.pendingFile directory 1) >>= either (fail . T.unpack) pure . pendingWaitFromValue
  check "CLI reused the answered wait" (firstWait.key /= secondWait.key)
  staleOut <- refused ["reply",directory,"0","approve"] "is no longer awaiting approval"
  check "old draft approval sent the revision" (not ("FAKE SEND" `isInfixOf` staleOut))
  _ <- refused ["reply",directory,"0","revise","Make it warmer."] "is no longer awaiting approval"
  approved <- cli ["reply",directory,"1","approve"]
  check "approved draft not sent" ("FAKE SEND (no email delivered)" `isInfixOf` approved
    && "Requested changes: Make it warmer." `isInfixOf` approved)
  _ <- refused ["reply",directory,"1","approve"] "is no longer awaiting approval"
  repeated <- cli ["resume",directory]
  check "duplicate approval sent again" (not ("FAKE SEND" `isInfixOf` repeated)
    && not ("[demo]" `isInfixOf` repeated))
  let rejectedDir = root </> "rejection"
  _ <- cli ["start",rejectedDir,"Do not send this."]
  rejected <- cli ["reply",rejectedDir,"0","reject"]
  check "rejection sent an email" ("Done: rejected; nothing sent." `isInfixOf` rejected
    && not ("FAKE SEND" `isInfixOf` rejected))
  rejectedAgain <- cli ["resume",rejectedDir]
  check "rejected run did new work" (rejectedAgain == rejected)

main :: IO ()
main = withWorkspace $ \directory -> do
  flowTest directory
  cliTests directory
  putStrLn "Email approval: revisions, replay, rejection and CLI restarts passed."
