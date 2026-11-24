module RunIO (spec) where

import Agentic
import Agentic.IO
import Agentic.Scripted (replyingWith, respond)
import Control.Exception (bracket, IOException, try)
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Text (Text)
import System.Directory
import System.FilePath ((</>))
import System.IO (openTempFile, hClose)
import Test.Hspec hiding (describe)
import Test.Hspec qualified as H

workspace :: (FilePath -> IO a) -> IO a
workspace = bracket create removeDirectoryRecursive
 where
  create = do
    dir <- getTemporaryDirectory
    (file,h) <- openTempFile dir "agentic-runs"
    hClose h
    removeFile file
    createDirectory file
    pure file

identity :: JournalIdentity
identity = JournalIdentity "run-test" "v1"

spec :: Spec
spec = H.describe "file-backed runs" $ do
  it "restores model work, typed questions and completed effects across new handles" $ workspace $ \dir -> do
    turns <- newIORef (0 :: Int)
    sends <- newIORef (0 :: Int)
    let SystemTwo provider = replyingWith (const (respond ("Draft" :: Text)))
        rt = runtime{systemTwo=SystemTwo (\c -> modifyIORef' turns (+1) >> provider c)}
        flow = draft @Text "email" >>> await @Bool
          >>> durably (\_ b -> modifyIORef' sends (+1) >> pure b)
        open = runIn (dir </> "run") identity rt flow
    _ <- startRun open ("brief" :: Text)
    [q] <- questions open
    asked @Text q `shouldBe` Right "Draft"
    _ <- resumeRun open
    questions open `shouldReturn` [q]
    answerRun open q.ticket (7 :: Int) `shouldReturn`
      Left (AnswerRejected (ReplyInvalid "expected a boolean, got 7"))
    answerRun open q.ticket True `shouldReturn` Right (RunFinished True)
    questions open `shouldReturn` []
    answerRun open q.ticket True `shouldReturn` Left (QuestionClosed q.ticket)
    resumeRun open `shouldReturn` RunFinished True
    readIORef turns `shouldReturn` 1
    readIORef sends `shouldReturn` 1
  it "allocates stable tickets for parallel waits and new tickets for later waits" $ workspace $ \dir -> do
    let flow = (await @Int &&& await @Int) >>> arr (uncurry (+)) >>> await @Int
        run = runIn (dir </> "run") identity runtime flow
    _ <- startRun run (1 :: Int)
    [a,b] <- questions run
    map (.ticket) [a,b] `shouldBe` [0,1]
    _ <- answerRun run a.ticket (2 :: Int)
    questions run `shouldReturn` [b]
    _ <- answerRun run b.ticket (3 :: Int)
    [c] <- questions run
    c.ticket `shouldBe` 2
    asked @Int c `shouldBe` Right 5
    answerRun run a.ticket (9 :: Int) `shouldReturn` Left (QuestionClosed 0)
    answerRun run c.ticket (6 :: Int) `shouldReturn` Right (RunFinished 6)
  it "recovers when a reply was committed but resume never ran" $ workspace $ \dir -> do
    let path = dir </> "run"
        flow = await @Bool :: Agentic IO Text Bool
        run = runIn path identity runtime flow
    _ <- startRun run "question"
    [q] <- questions run
    rt <- withJournal (path </> "journal.jsonl") identity runtime
    fulfil rt flow q.pending (Bool True) `shouldReturn` Right ()
    answerRun run q.ticket True `shouldReturn` Right (RunFinished True)
  it "preserves frontier and tickets on failure, then recovers without repeating committed work" $ workspace $ \dir -> do
    failNow <- newIORef True
    effects <- newIORef (0 :: Int)
    let path = dir </> "run"
        flow = await @Bool >>> durably (\_ b -> modifyIORef' effects (+1) >> pure b)
          >>> act (\b -> readIORef failNow >>= \bad -> if bad then fail "interrupted" else pure b)
        run = runIn path identity runtime flow
    _ <- startRun run ("question" :: Text)
    [q] <- questions run
    snapshot <- BS.readFile (path </> "run.json")
    result <- try (answerRun run q.ticket True) :: IO (Either IOException (Either AnswerRefused (Progress Bool)))
    either (const (pure ())) (const (expectationFailure "expected failure")) result
    BS.readFile (path </> "run.json") `shouldReturn` snapshot
    writeIORef failNow False
    resumeRun run `shouldReturn` RunFinished True
    readIORef effects `shouldReturn` 1
  it "recovers if the successful outcome's frontier was never published" $ workspace $ \dir -> do
    let path = dir </> "run"
        run = runIn path identity runtime (await @Bool :: Agentic IO Text Bool)
    _ <- startRun run "question"
    snapshot <- BS.readFile (path </> "run.json")
    [q] <- questions run
    _ <- answerRun run q.ticket True
    -- Simulate the crash window between completing work and atomic rename.
    BS.writeFile (path </> "run.json") snapshot
    resumeRun run `shouldReturn` RunFinished True
    questions run `shouldReturn` []
  it "refuses accidental reuse, wrong revisions, absent runs and preinstalled journals" $ workspace $ \dir -> do
    let path = dir </> "run"
        flow = await @Bool :: Agentic IO Text Bool
        run = runIn path identity runtime flow
    resumeRun run `shouldThrow` (== RunNotStarted path)
    _ <- startRun run "question"
    startRun run "question" `shouldThrow` (== RunAlreadyExists path)
    startRun run "different" `shouldThrow` (== RunAlreadyExists path)
    resumeRun (runIn path (JournalIdentity "run-test" "v2") runtime flow)
      `shouldThrow` (== RunIdentityMismatch path)
    rt <- withJournal (dir </> "other.jsonl") identity runtime
    void (startRun (runIn (dir </> "other") identity rt flow) "q") `shouldThrow` (== RunHasJournal)
  it "fails on corrupt state without treating it as an absent run" $ workspace $ \dir -> do
    let path = dir </> "run"
        run = runIn path identity runtime (await @Bool :: Agentic IO Text Bool)
    _ <- startRun run "question"
    BS.writeFile (path </> "run.json") "{"
    resumeRun run `shouldThrow` (\case RunInvalidStore _ _ -> True; _ -> False)
  it "restarts initialization after directory creation or an interrupted initial save" $ workspace $ \dir -> do
    let path = dir </> "run"
        run = runIn path identity runtime (await @Bool :: Agentic IO Text Bool)
    createDirectory path
    BS.writeFile (path </> "run.json.tmp") "incomplete"
    _ <- startRun run "question"
    [q] <- questions run
    q.ticket `shouldBe` 0
    asked @Text q `shouldBe` Right "question"
  it "keeps both siblings' tickets when the middle parallel question is answered" $ workspace $ \dir -> do
    let flow = each (await @Int) >>> arr sum >>> await @Int
        run = runIn (dir </> "run") identity runtime flow
    _ <- startRun run [10,20,30 :: Int]
    [a,b,c] <- questions run
    map (.ticket) [a,b,c] `shouldBe` [0,1,2]
    _ <- answerRun run b.ticket (2 :: Int)
    questions run `shouldReturn` [a,c]
    _ <- answerRun run a.ticket (1 :: Int)
    _ <- answerRun run c.ticket (3 :: Int)
    [d] <- questions run
    d.ticket `shouldBe` 3
    asked @Int d `shouldBe` Right 6
