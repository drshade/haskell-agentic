-- | Claude drafts an email. Approve or reject it in a later process.
-- The sender only prints: no email is delivered.
module EmailApproval where

import Agentic
import Agentic.Anthropic (anthropic)
import Agentic.IO
import Control.Exception (catch, displayException, throwIO)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Generics (Generic)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

data Email = Email {subject :: Text, body :: Text}
  deriving (Eq, Show, Generic, Contract, Persist)

data Decision = Approve | Reject deriving (Eq, Show, Generic, Persist)

emailApproval :: (Email -> IO ()) -> Agentic IO Text ()
emailApproval send =
  draft @Email "Draft a short email from this brief."
    >>> (returnA &&& await @Decision `named` "approve this email?")
    >>> arr (\(mail,approved) -> if approved == Approve then Left mail else Right ())
    >>> ((durably (\_ mail -> send mail) `named` "print approved email")
      ||| arr id `named` "finish without sending") `named` "approved?"

fakeSend :: Email -> IO ()
fakeSend mail = T.putStrLn ("FAKE SEND (no email delivered)\n" <> mail.subject <> "\n" <> mail.body)

main :: IO ()
main = runExample `catch` (die . displayException :: RunError -> IO ())

runExample :: IO ()
runExample = do
  args <- getArgs
  case args of
    ["--mermaid"] -> either throwIO (T.putStrLn . mermaid) (describe (emailApproval fakeSend))
    directory:command -> do
      _ <- loadDotEnv
      rt <- withSystemTwo (anthropic & effort Low) runtime
      -- A real host supplies a unique run id and bumps the revision on changes.
      let run = runIn directory (JournalIdentity (T.pack directory) "v3")
            rt (emailApproval fakeSend)
          answer number approved = case readMaybe number of
            Nothing -> die "question number must be an integer"
            Just n -> answerRun run n approved
              >>= either (die . T.unpack . answerRefusalText) pure
      result <- case command of
        ["start",brief] -> startRun run (T.pack brief)
        ["resume"] -> resumeRun run
        ["approve",number] -> answer number Approve
        ["reject",number] -> answer number Reject
        _ -> usage
      case result of
        RunFinished () -> putStrLn "Finished."
        RunWaiting _ -> questions run >>= mapM_ (\q -> do
          mail <- either (die . T.unpack) pure (asked @Email q)
          T.putStrLn (mail.subject <> "\n" <> mail.body)
          putStrLn ("Approve or reject question " <> show q.ticket <> "."))
    _ -> usage
 where
  usage = die "usage: email-approval DIR start BRIEF | DIR resume | DIR approve N | DIR reject N"
