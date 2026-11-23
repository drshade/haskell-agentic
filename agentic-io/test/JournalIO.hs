module JournalIO (spec, childMode) where

import Agentic hiding (record, key, model, right)
import Agentic.Codec qualified as C
import Agentic.Schema (Shape (..))
import Agentic.IO
import Agentic.Runtime (Conversation (..))
import Agentic.Scripted (scripted, respond, fixedAnswers)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, readMVar)
import Control.Exception
import Control.Monad (when)
import Data.IORef
import Data.List (isInfixOf)
import Data.Text qualified as T
import GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding, latin1)
import System.Directory
import System.Environment (getArgs, getExecutablePath)
import System.Exit
import System.IO (openTempFile, hClose)
import System.Process (readProcessWithExitCode)
import System.Timeout qualified as Timeout
import Test.Hspec hiding (describe)
import Test.Hspec qualified as H

intCodec :: C.Codec Int
intCodec = C.primitive SInteger (Integer . toInteger) $ \case
  Integer n | n >= toInteger (minBound :: Int), n <= toInteger (maxBound :: Int) -> Right (fromInteger n)
  _ -> Left "expected Int"

identity :: JournalIdentity
identity = JournalIdentity "test-run" "test-revision"

withFilePath :: (FilePath -> IO a) -> IO a
withFilePath = bracket fresh cleanup
 where
  fresh = do
    dir <- getTemporaryDirectory
    (file,h) <- openTempFile dir "agentic-journal-test"
    hClose h
    removeFile file
    pure file
  cleanup file = mapM_ (\p -> doesFileExist p >>= \exists -> when exists (removeFile p))
    [file,file <> ".effects"]

counted :: IORef Int -> Agentic IO Int Int
counted count = durably intCodec intCodec $ \key n -> do
  case key of Nothing -> fail "expected a run-scoped key"; Just _ -> pure ()
  atomicModifyIORef' count (\c -> (c+1,()))
  pure (n+1)

recording :: FilePath -> Runtime IO -> IO (Runtime IO)
recording file = withJournal file identity

capture :: Runtime IO -> IO (Runtime IO, IO [Summary])
capture rt = do
  ref <- newIORef []
  pure (observing (\e -> atomicModifyIORef' ref (\es -> (summarise e:es,()))) rt, reverse <$> readIORef ref)

isJournal :: FlowError -> Bool
isJournal (JournalFailure _) = True
isJournal _ = False

childMode :: IO Bool
childMode = getArgs >>= \case
  ["--journal-child",mode,file,scenario] -> do
    let checkpoint = durably intCodec intCodec (\key n -> do
          appendFile (file <> ".effects") (maybe "missing key" (T.unpack . durableKeyText) key <> "\n")
          pure n)
        self = recursiveTool $ \t -> tool @Int @Int "self" "" $
          checkpoint >>> draftWith [t] "recurse"
        flow = case scenario of
          "plain" -> checkpoint >>> act (pure . (+2))
          "each" -> arr (\n -> [n,n]) >>> each checkpoint >>> arr sum
          "repeat" -> repeatUntil (>=43) (checkpoint >>> arr (+1))
          "tools" -> draftWith [tool "inc" "" checkpoint] "two"
          "recursive" -> draftWith [self] "recurse"
          _ -> error "unknown child scenario"
        provider = SystemTwo $ \c -> pure $ Turn (Raw (String "child turn")) $
          case (scenario,c.input,c.history) of
            ("tools",Integer n,[]) -> CallTools
              [ToolCall "same-id" "inc" (Integer n),ToolCall "same-id" "inc" (Integer n)]
            ("tools",Integer n,_) -> Respond (Integer (2*n))
            ("recursive",Integer n,[]) | n > 0 ->
              CallTools [ToolCall "same-id" "self" (Integer (n-1))]
            _ -> Respond c.input
        stop e = when (mode == "stop" && e.nodeType == EffectNode && e.happened == Returned Completed)
          (exitWith (ExitFailure 23))
    rt <- recording file runtime{systemTwo=provider}
    value <- interpret (observing stop rt) flow (if scenario == "recursive" then 3 else 41)
    print value
    pure True
  _ -> pure False

spec :: Spec
spec = H.describe "durable journal" $ do
  it "restarts a new process against the file and continues without repeating the effect" $ withFilePath $ \file -> do
    exe <- getExecutablePath
    (code,_,_) <- readProcessWithExitCode exe ["--journal-child","stop",file,"plain"] ""
    code `shouldBe` ExitFailure 23
    (again,out,err) <- readProcessWithExitCode exe ["--journal-child","resume",file,"plain"] ""
    (again,out,err) `shouldBe` (ExitSuccess,"43\n","")
    effects <- readFile (file <> ".effects")
    length (lines effects) `shouldBe` 1
    effects `shouldSatisfy` isInfixOf "test-run"

  mapM_ (\(scenario,expected,count) ->
    it ("resumes " <> scenario <> " in a new process with each effect executed once") $ withFilePath $ \file -> do
      exe <- getExecutablePath
      (stopped,_,_) <- readProcessWithExitCode exe ["--journal-child","stop",file,scenario] ""
      stopped `shouldBe` ExitFailure 23
      (code,out,err) <- readProcessWithExitCode exe ["--journal-child","resume",file,scenario] ""
      (code,out,err) `shouldBe` (ExitSuccess,expected,"")
      contents <- readFile (file <> ".effects")
      length (lines contents) `shouldBe` count)
    [("each","82\n",2),("repeat","43\n",2),("tools","82\n",2),("recursive","3\n",3)]

  it "restores repeated identical inputs at distinct each indices, including a longer list" $ withFilePath $ \file -> do
    n <- newIORef 0
    let flow = each (counted n)
    rt <- recording file runtime
    interpret rt flow [1,1] `shouldReturn` [2,2]
    again <- recording file runtime
    interpret again flow [1,1,1] `shouldReturn` [2,2,2]
    readIORef n `shouldReturn` 3
    interpret again flow [2,1,1] `shouldThrow` \case
      JournalFailure (JournalDiverged _) -> True
      _ -> False
    readIORef n `shouldReturn` 3

  it "restores repeat passes independently" $ withFilePath $ \file -> do
    n <- newIORef 0
    let flow = repeatUntil (>=3) (counted n)
    rt <- recording file runtime
    interpret rt flow 0 `shouldReturn` 3
    again <- recording file runtime
    interpret again flow 0 `shouldReturn` 3
    readIORef n `shouldReturn` 3

  it "retains a completion when its Returned observer throws" $ withFilePath $ \file -> do
    n <- newIORef 0
    rt <- recording file runtime
    let stop e = when (e.nodeType == EffectNode && e.happened == Returned Completed) (throwIO (ErrorCall "stop"))
    interpret (observing stop rt) (counted n) 1 `shouldThrow` anyErrorCall
    again <- recording file runtime
    (observed,events) <- capture again
    interpret observed (counted n) 1 `shouldReturn` 2
    readIORef n `shouldReturn` 1
    es <- events
    [e.happened | e <- es, e.nodeType == EffectNode]
      `shouldBe` [Entered Nothing,Restored DurableEffect,Returned Completed]
    renderJson (summaryValue (es !! 2)) `shouldSatisfy` T.isInfixOf "restored"

  it "rejects another run or revision before executing anything" $ withFilePath $ \file -> do
    _ <- recording file runtime
    withJournal file (JournalIdentity "test-run" "new") runtime `shouldThrow` isJournal
    withJournal file (JournalIdentity "other-run" "test-revision") runtime `shouldThrow` isJournal

  it "does not rerun a result that fails its persistence decoder" $ withFilePath $ \file -> do
    n <- newIORef 0
    rt <- recording file runtime
    interpret rt (counted n) 1 `shouldReturn` 2
    let bad = C.Codec intCodec.schema intCodec.encode (const (Left "old result cannot decode"))
        flow = durably intCodec bad (\_ _ -> modifyIORef' n (+1) >> pure (9 :: Int))
    again <- recording file runtime
    interpret again flow 1 `shouldThrow` \case
      JournalFailure (JournalDecodeError _ _) -> True
      _ -> False
    readIORef n `shouldReturn` 1

  it "propagates save failure without a Returned or a live retry" $ withFilePath $ \file -> do
    n <- newIORef 0
    j <- openJournal file identity
    let broken = j{saveRecord = \_ _ -> throwIO (ErrorCall "save failed")}
    (rt,events) <- capture runtime{journal=Just broken}
    interpret rt (counted n) 1 `shouldThrow` anyErrorCall
    readIORef n `shouldReturn` 1
    es <- events
    [e.happened | e <- es, e.nodeType == EffectNode] `shouldBe` [Entered Nothing,Failed]
    again <- recording file runtime
    interpret again (counted n) 1 `shouldReturn` 2
    readIORef n `shouldReturn` 2 -- the agreed uncommitted-effect window

  it "writes journal and cache text as UTF-8 regardless of locale encoding" $
    bracket getLocaleEncoding setLocaleEncoding $ \_ -> do
      setLocaleEncoding latin1
      let textCodec = C.primitive (SString Nothing) String $ \case
            String t -> Right t
            _ -> Left "expected text"
          unicode = "café 🦖"
      withFilePath $ \file -> do
        n <- newIORef (0 :: Int)
        let flow = durably textCodec textCodec (\_ t -> modifyIORef' n (+1) >> pure t)
        rt <- recording file runtime
        interpret rt flow unicode `shouldReturn` unicode
        again <- recording file runtime
        interpret again flow unicode `shouldReturn` unicode
        readIORef n `shouldReturn` 1
      withFilePath $ \file -> do
        model <- scripted [respond unicode]
        rt <- withStore Record file runtime{systemTwo=model}
        interpret rt (draft @T.Text @T.Text "unicode") "input" `shouldReturn` unicode
        again <- withStore Replay file runtime
        interpret again (draft @T.Text @T.Text "unicode") "input" `shouldReturn` unicode

  it "fails on unreadable interior, blank and truncated final lines" $ do
    mapM_ (\tailText -> withFilePath $ \file -> do
      _ <- recording file runtime
      appendFile file tailText
      recording file runtime `shouldThrow` isJournal)
      ["not JSON\n{}\n","\n","{\"key\":" ,"{}\n"]

  it "accepts identical duplicate records and rejects conflicting completions" $ withFilePath $ \file -> do
    j <- openJournal file identity
    let key = String "test-key"
        a = Object [("request",Integer 1),("result",Integer 2)]
        b = Object [("request",Integer 1),("result",Integer 3)]
    j.saveRecord key a
    j.saveRecord key a
    j.lookupRecord key `shouldReturn` Just a
    j.saveRecord key b `shouldThrow` isJournal
    j.lookupRecord key `shouldReturn` Just a
    appendFile file (T.unpack (renderJson (Object [("key",key),("record",a)])) <> "\n")
    same <- openJournal file identity
    same.lookupRecord key `shouldReturn` Just a
    appendFile file (T.unpack (renderJson (Object [("key",key),("record",b)])) <> "\n")
    openJournal file identity `shouldThrow` isJournal

  it "replays model turns, duplicate provider call ids and completed tool bodies" $ withFilePath $ \file -> do
    n <- newIORef 0
    let t = tool @Int @Int "inc" "" (counted n)
        flow = draftWith @Int @Int [t] "go"
    model <- scripted [CallTools [ToolCall "same" "inc" (Integer 1),ToolCall "same" "inc" (Integer 1)],respond (9 :: Int)]
    rt <- recording file runtime{systemTwo=model}
    interpret rt flow 0 `shouldReturn` 9
    readIORef n `shouldReturn` 2
    again <- recording file runtime -- no provider installed
    (observed,events) <- capture again
    interpret observed flow 0 `shouldReturn` 9
    readIORef n `shouldReturn` 2
    es <- events
    [op | e <- es, Restored op <- [e.happened]]
      `shouldBe` [DraftTurn 0,ToolExecution,ToolExecution,DraftTurn 1]
    [e | e <- es, e.nodeType == EffectNode] `shouldSatisfy` null
    length [() | e <- es, CallResolved _ _ <- [e.happened]] `shouldBe` 2

  it "saves a turn before dispatch and runs its unsaved tool live on resume" $ withFilePath $ \file -> do
    n <- newIORef 0
    let flow = draftWith @Int @Int [tool "inc" "" (counted n)] "go"
        stop e = when (e.happened == Turned 0 ToolBatch) (throwIO (ErrorCall "stop after turn"))
    model <- scripted [CallTools [ToolCall "id" "inc" (Integer 1)]]
    rt <- recording file runtime{systemTwo=model}
    interpret (observing stop rt) flow 0 `shouldThrow` anyErrorCall
    readIORef n `shouldReturn` 0
    live <- scripted [respond (8 :: Int)]
    again <- recording file runtime{systemTwo=live}
    (observed,events) <- capture again
    interpret observed flow 0 `shouldReturn` 8
    readIORef n `shouldReturn` 1
    es <- events
    [op | e <- es, Restored op <- [e.happened]] `shouldBe` [DraftTurn 0]

  it "restores committed effects inside an incomplete tool" $ withFilePath $ \file -> do
    n <- newIORef 0
    stopped <- newIORef True
    let body = counted n >>> act (\x -> readIORef stopped >>= \stop -> if stop then throwIO (ErrorCall "later failure") else pure x)
        flow = draftWith @Int @Int [tool "inc" "" body] "go"
    model <- scripted [CallTools [ToolCall "id" "inc" (Integer 1)]]
    rt <- recording file runtime{systemTwo=model}
    interpret rt flow 0 `shouldThrow` anyErrorCall
    writeIORef stopped False
    live <- scripted [respond (8 :: Int)]
    again <- recording file runtime{systemTwo=live}
    interpret again flow 0 `shouldReturn` 8
    readIORef n `shouldReturn` 1

  it "restores judge answers and rejected turns, preserving Raw history" $ withFilePath $ \file -> do
    calls <- newIORef (0 :: Int)
    let flow = judge (yesNo "ok?") >>> draft @Int "number"
        provider = SystemTwo $ \c -> do
          modifyIORef' calls (+1)
          pure $ if null c.history then Turn (Raw (String "rejected raw")) (Respond (String "bad"))
            else case c.history of
              [Rejected (Raw (String "rejected raw")) _] ->
                Turn (Raw (String "accepted raw")) (respond (7 :: Int))
              _ -> error "raw rejection history was lost"
    rt <- recording file runtime{systemOne=fixedAnswers 0.9,systemTwo=provider}
    interpret rt flow (1 :: Int) `shouldReturn` 7
    readIORef calls `shouldReturn` 2
    again <- recording file runtime
    (observed,events) <- capture again
    interpret observed flow (1 :: Int) `shouldReturn` 7
    es <- events
    [op | e <- es, Restored op <- [e.happened]] `shouldBe` [Judgement,DraftTurn 0,DraftTurn 1]
    [e.happened | e <- es, e.happened == Judged || e.happened == OutputRejected 0]
      `shouldBe` [Judged,OutputRejected 0]

  it "retains a completed sibling while cancellation leaves the other live" $ withFilePath $ \file -> do
    n <- newIORef 0
    ready <- newEmptyMVar
    blocked <- newEmptyMVar
    done <- newEmptyMVar
    let right = act (\x -> readMVar ready >> takeMVar blocked >> pure x) >>> counted n
        flow = counted n &&& right
        observeCommit e = when (e.nodeType == EffectNode && e.happened == Returned Completed) $
          putMVar ready ()
    rt <- recording file (concurrently runtime)
    tid <- forkIO $ do
      r <- try (interpret (observing observeCommit rt) flow 1)
      putMVar done (r :: Either SomeException (Int,Int))
    takeMVar ready
    putMVar ready ()
    throwTo tid ThreadKilled
    Timeout.timeout 1000000 (takeMVar done) `shouldReturnSatisfy` \case
      Just (Left _) -> True; _ -> False
    putMVar blocked ()
    again <- recording file (concurrently runtime)
    interpret again flow 1 `shouldReturn` (2,2)
    readIORef n `shouldReturn` 2

  it "restores an inner recursive tool under an unfinished outer invocation" $ withFilePath $ \file -> do
    n <- newIORef (0 :: Int)
    stopped <- newIORef True
    let mark = durably intCodec intCodec (\_ x -> modifyIORef' n (+1) >> pure x)
        self = recursiveTool $ \t -> tool @Int @Int "self" "" $
          mark >>> draftWith [t] "recurse" >>> act (\x -> do
            stop <- readIORef stopped
            when (stop && x == 1) (throwIO (ErrorCall "outer stopped"))
            pure x)
        flow = draftWith @Int @Int [self] "start"
        provider = SystemTwo $ \c -> pure $ Turn (Raw (String "raw")) $ case (c.input,c.history) of
          (Integer x,[]) | x > 0 -> CallTools [ToolCall "same-id" "self" (Integer (x-1))]
          _ -> Respond c.input
    rt <- recording file runtime{systemTwo=provider}
    interpret rt flow 3 `shouldThrow` anyErrorCall
    readIORef n `shouldReturn` 3
    writeIORef stopped False
    again <- recording file runtime{systemTwo=provider}
    (observed,events) <- capture again
    interpret observed flow 3 `shouldReturn` 3
    readIORef n `shouldReturn` 3
    es <- events
    length [() | e <- es, e.happened == Restored ToolExecution] `shouldBe` 1
    length [() | e <- es, e.happened == Restored DurableEffect] `shouldBe` 2
    final <- recording file runtime -- now no model or body needs to run
    interpret final flow 3 `shouldReturn` 3
    readIORef n `shouldReturn` 3

  it "retains unknown-tool and invalid-input outcomes on replay" $ withFilePath $ \file -> do
    n <- newIORef 0
    let flow = draftWith @Int @Int [tool "inc" "" (counted n)] "go"
    model <- scripted [CallTools [ToolCall "missing" "missing" Null,ToolCall "invalid" "inc" Null], respond (3 :: Int)]
    rt <- recording file runtime{systemTwo=model}
    interpret rt flow 0 `shouldReturn` 3
    again <- recording file runtime
    (observed,events) <- capture again
    interpret observed flow 0 `shouldReturn` 3
    readIORef n `shouldReturn` 0
    es <- events
    [o | e <- es, e.nodeType == CallNode, Returned o <- [e.happened]]
      `shouldBe` [UnknownTool,InvalidInput]
    length [() | e <- es, CallResolved _ _ <- [e.happened]] `shouldBe` 1

  it "checks complete model requests, including instruction and judge specs" $ do
    withFilePath $ \file -> do
      model <- scripted [respond (7 :: Int)]
      rt <- recording file runtime{systemTwo=model}
      interpret rt (draft @Int @Int "first instruction") 1 `shouldReturn` 7
      again <- recording file runtime
      interpret again (draft @Int @Int "different instruction") 1 `shouldThrow` isJournal
    withFilePath $ \file -> do
      rt <- recording file runtime{systemOne=fixedAnswers 0.9}
      _ <- interpret rt (judge (yesNo "first question")) (1 :: Int)
      again <- recording file runtime
      interpret again (judge (yesNo "different question")) (1 :: Int) `shouldThrow` isJournal

  it "validates a restored tool result against its output codec before reporting Restored" $ withFilePath $ \file -> do
    n <- newIORef 0
    let flow = draftWith @Int @Int [tool "inc" "" (counted n)] "go"
    model <- scripted [CallTools [ToolCall "id" "inc" (Integer 1)],respond (5 :: Int)]
    rt <- recording file runtime{systemTwo=model}
    interpret rt flow 0 `shouldReturn` 5
    j <- openJournal file identity
    let edit name f (Object fs) = Object [(k,if k == name then f v else v) | (k,v) <- fs]
        edit _ _ v = v
        corrupt = edit "result" (edit "completion" (edit "result" (const (Object [("ok",String "bad int")]))))
        broken = j{lookupRecord = \k -> do
          saved <- j.lookupRecord k
          pure $ case k of
            Object fs | lookupField "operation" fs == Just (Array [String "tool"]) -> fmap corrupt saved
            _ -> saved}
    (observed,events) <- capture runtime{journal=Just broken}
    interpret observed flow 0 `shouldThrow` isJournal
    readIORef n `shouldReturn` 1
    es <- events
    [() | e <- es, e.happened == Restored ToolExecution] `shouldBe` []

  it "poisons a handle after an actual file write failure instead of serving a miss" $ withFilePath $ \file -> do
    j <- openJournal file identity
    removeFile file
    createDirectory file
    let record = Object [("request",Null),("result",Integer 1)]
    (j.saveRecord (String "key") record `shouldThrow` anyIOException)
      `finally` removeDirectory file
    j.lookupRecord (String "key") `shouldThrow` isJournal


shouldReturnSatisfy :: Show a => IO a -> (a -> Bool) -> Expectation
shouldReturnSatisfy action predicate = action >>= (`shouldSatisfy` predicate)
