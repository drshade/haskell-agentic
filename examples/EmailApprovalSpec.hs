module Main (main) where

import Agentic
import Agentic.IO
import Agentic.Scripted (replyingWith, respond)
import EmailApproval qualified as E
import Control.Exception (bracket)
import Control.Monad (forM_, unless)
import Data.IORef
import System.Directory
import System.FilePath ((</>))
import System.IO (openTempFile, hClose)

main :: IO ()
main = forM_ [False,True] $ \approved -> bracket fresh removeDirectoryRecursive $ \root -> do
  sent <- newIORef []
  let mail = E.Email "Hello" "The café opens Friday."
      rt = runtime{systemTwo=replyingWith (const (respond mail))}
      run = runIn (root </> "run") (JournalIdentity "test" "v2") rt
        (E.emailApproval (\email -> modifyIORef' sent (<> [email])))
  _ <- startRun run "Tell Sam about the café."
  [q] <- questions run
  unless (asked @E.Email q == Right mail) (fail "wrong draft shown for approval")
  result <- answerRun run q.ticket (if approved then E.Approve else E.Reject)
  unless (result == Right (RunFinished ())) (fail "did not finish")
  _ <- resumeRun run
  delivered <- readIORef sent
  unless (delivered == if approved then [mail] else []) (fail "incorrect or repeated send")
 where
  fresh = do
    temp <- getTemporaryDirectory
    (path,h) <- openTempFile temp "email-approval-test"
    hClose h
    removeFile path
    createDirectory path
    pure path
