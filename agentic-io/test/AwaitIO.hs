module AwaitIO (spec, childMode) where

import Agentic hiding (key, record)
import Agentic.Codec qualified as C
import Agentic.Schema (Shape (..))
import Agentic.Schema qualified as S
import Agentic.Runtime (Conversation (..))
import Agentic.Runtime qualified as R
import Agentic.IO
import Agentic.Aeson (fromAeson)
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as TE
import Data.Text qualified as T
import Data.List.NonEmpty qualified as NE
import Data.IORef
import Control.Concurrent (newEmptyMVar, putMVar, readMVar)
import Control.Exception
import Control.Monad (unless, when, forM_)
import System.Directory
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode (..))
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
identity = JournalIdentity "await-run" "r1"

withPath :: (FilePath -> IO a) -> IO a
withPath = bracket fresh cleanup
 where
  fresh = do
    dir <- getTemporaryDirectory
    (file,h) <- openTempFile dir "agentic-await-test"
    hClose h
    removeFile file
    pure file
  cleanup file = forM_ [file,file<>".pending",file<>".effects",file<>".after"] $ \p ->
    doesFileExist p >>= \exists -> when exists (removeFile p)

open :: FilePath -> IO (Runtime IO)
open file = withJournal file identity runtime

oneWait :: Show a => Progress a -> IO PendingWait
oneWait (Waiting (w NE.:| [])) = pure w
oneWait other = expectationFailure ("expected one wait: " <> show other) >> fail "missing wait"

childFlow :: FilePath -> Agentic IO Int Int
childFlow file = durably intCodec intCodec (\_ n -> appendFile (file<>".effects") "once\n" >> pure (n+1))
  >>> await intCodec intCodec
  >>> act (\n -> appendFile (file<>".after") (show n<>"\n") >> pure (n+1))

childMode :: IO Bool
childMode = getArgs >>= \case
  ["--await-child",mode,file] -> do
    rt <- open file
    case mode of
      "ask" -> do
        w <- resumable rt (childFlow file) 0 >>= oneWait
        BS.writeFile (file<>".pending") (TE.encodeUtf8 (renderJson (pendingWaitValue w)))
      "fulfil" -> do
        bytes <- BS.readFile (file<>".pending")
        v <- either fail (pure . fromAeson) (J.eitherDecodeStrict' bytes)
        w <- either (fail . T.unpack) pure (pendingWaitFromValue v)
        accepted <- fulfil rt (childFlow file) w (Integer 42)
        unless (accepted == Right ()) (fail (show accepted))
      "resume" -> resumable rt (childFlow file) 0 >>= \case
        Finished 43 -> pure ()
        result -> fail (show result)
      _ -> fail "unknown child mode"
    pure True
  _ -> pure False

spec :: Spec
spec = H.describe "external replies with an IO journal" $ do
  it "asks, fulfils in another process and resumes with the typed reply" $ withPath $ \file -> do
    exe <- getExecutablePath
    forM_ ["ask","fulfil","resume"] $ \mode -> do
      child <- Timeout.timeout 10000000 (readProcessWithExitCode exe ["--await-child",mode,file] "")
      child `shouldBe` Just (ExitSuccess,"","")
    readFile (file<>".effects") `shouldReturn` "once\n"
    readFile (file<>".after") `shouldReturn` "42\n"

  it "distinguishes a descriptor mismatch from a backing journal revision mismatch" $ withPath $ \file -> do
    rt <- open file
    let flow = await intCodec intCodec
    w <- resumable rt flow 1 >>= oneWait
    let journal = case rt.journal of Just j -> j; Nothing -> error "missing journal"
        wrongRevision = JournalIdentity "await-run" "r2"
        lazyJournal = journal{initialise=openJournal file wrongRevision >> pure ()}
    fulfil rt flow w{R.identity=wrongRevision} (Integer 2) `shouldReturn` Left WaitWrongIdentity
    eager <- try (openJournal file wrongRevision)
    case eager of
      Left (JournalFailure (JournalMismatch _)) -> pure ()
      Left e -> expectationFailure (show e)
      Right _ -> expectationFailure "opened the wrong revision"
    lazyResult <- try (fulfil rt{journal=Just lazyJournal} flow w (Integer 2))
    lazyResult `shouldSatisfy` \case Left (JournalFailure (JournalMismatch _)) -> True; _ -> False

  it "normalises and validates the exact reply saved, with idempotent duplicate delivery" $ withPath $ \file -> do
    rt <- open file
    let output = C.primitive (SObject []) (\(a,b) -> Object [("b",Integer (toInteger b)),("a",Integer (toInteger a))]) $ \case
          Object [("a",Integer a),("b",Integer b)] -> Right (fromInteger a::Int,fromInteger b::Int)
          _ -> Left "expected canonical a,b"
        flow = await intCodec output
    w <- resumable rt flow 1 >>= oneWait
    fulfil rt flow w (Object [("b",Number 2.0),("a",Number 1.0)]) `shouldReturn` Right ()
    fulfil rt flow w (Object [("a",Integer 1),("b",Integer 2)]) `shouldReturn` Right ()
    rt' <- open file
    resumable rt' flow 1 `shouldReturn` Finished (1,2)
    fulfil rt' flow w (Object [("a",Integer 3),("b",Integer 2)]) `shouldReturn` Left ReplyAlreadyAnswered

  it "returns invalid replies as data without changing the journal" $ withPath $ \file -> do
    rt <- open file
    let flow = await intCodec intCodec
    w <- resumable rt flow 1 >>= oneWait
    originalBytes <- BS.readFile file
    fulfil rt flow w (String "no") `shouldReturn` Left (ReplyInvalid "expected Int")
    result <- fulfil rt flow w (Number (0/0))
    result `shouldSatisfy` \case Left (ReplyNonCanonical _) -> True; _ -> False
    badRequest <- fulfil rt flow w{request=Object [("x",Null),("x",Null)]} (Integer 2)
    badRequest `shouldSatisfy` \case Left (WaitInvalidRequest _) -> True; _ -> False
    BS.readFile file `shouldReturn` originalBytes
    resumable rt flow 1 `shouldReturn` Waiting (w NE.:| [])

  it "reports a wait payload and restores with Entered, Restored Wait, Returned" $ withPath $ \file -> do
    rt <- open file
    events <- newIORef []
    let flow = await intCodec intCodec `named` "approval"
        observed = observing (\e -> modifyIORef' events (e:)) rt
    w <- resumable observed flow 1 >>= oneWait
    firstEvents <- reverse <$> readIORef events
    [p | Event{nodeType=AwaitNode,happened=Suspended,payload=Just (WaitPayload p)} <- firstEvents] `shouldBe` [w]
    w.path `shouldBe` [Note "approval" Nothing]
    fulfil observed flow w (Integer 2) `shouldReturn` Right ()
    (map summarise <$> readIORef events) `shouldReturn` map summarise (reverse firstEvents) -- fulfil emits no events
    writeIORef events []
    resumable observed flow 1 `shouldReturn` Finished 2
    restored <- reverse <$> readIORef events
    [e.happened | e <- restored,e.nodeType == AwaitNode] `shouldBe`
      [Entered Nothing,Restored Wait,Returned Completed]

  it "lets an independent durable sibling finish and saves it before returning Waiting" $ withPath $ \file -> do
    rt <- open file
    waiting <- newEmptyMVar
    count <- newIORef (0::Int)
    let observed = observing (\e -> when (e.nodeType == AwaitNode && e.happened == Suspended) (putMVar waiting ())) (concurrently rt)
        effect = durably intCodec intCodec (\_ n -> readMVar waiting >> modifyIORef' count (+1) >> pure (n+1))
        flow = await intCodec intCodec &&& effect
    outcome <- Timeout.timeout 2000000 (resumable observed flow 1)
    w <- case outcome of Just r -> oneWait r; _ -> fail "parallel wait deadlocked"
    readIORef count `shouldReturn` 1
    fulfil rt flow w (Integer 5) `shouldReturn` Right ()
    rt' <- open file
    resumable (concurrently rt') flow 1 `shouldReturn` Finished (5,2)
    readIORef count `shouldReturn` 1

  it "fails the parent when a sibling fails after a wait" $ withPath $ \file -> do
    rt <- open file
    waiting <- newEmptyMVar
    events <- newIORef []
    let observed = observing (\e -> do
          atomicModifyIORef' events (\es -> (e:es,()))
          when (e.nodeType == AwaitNode && e.happened == Suspended) (putMVar waiting ())) (concurrently rt)
        flow = await intCodec intCodec &&& act (\_ -> readMVar waiting >> throwIO (ErrorCall "sibling") :: IO Int)
    result <- Timeout.timeout 2000000 (try (resumable observed flow 1))
    result `shouldSatisfy` \case Just (Left (ErrorCall "sibling")) -> True; _ -> False
    es <- readIORef events
    [e.happened | e <- es,e.nodeType == RunNode] `shouldBe` [Failed,Entered Nothing]

  forM_ [False,True] $ \cancel ->
    it (if cancel then "preserves cancellation during a suspension report" else "preserves an exception during a suspension report") $ withPath $ \file -> do
      rt <- open file
      events <- newIORef []
      let observed = observing (\e -> do
            modifyIORef' events (e:)
            when (e.nodeType == AwaitNode && e.happened == Suspended) $
              if cancel then throwIO ThreadKilled else throwIO (ErrorCall "observer")) rt
      result <- try (resumable observed (await intCodec intCodec) 1)
      case result of
        Left (e::SomeException) ->
          if cancel then (fromException e :: Maybe AsyncException) `shouldBe` Just ThreadKilled
          else (fromException e :: Maybe ErrorCall) `shouldBe` Just (ErrorCall "observer")
        Right _ -> expectationFailure "observer failure became waiting"
      es <- readIORef events
      [e.happened | e <- es,e.nodeType == AwaitNode] `shouldBe` [Suspended,Entered Nothing]
      [e.happened | e <- es,e.nodeType == RunNode] `shouldBe` [if cancel then Cancelled else Failed,Entered Nothing]

  it "resolves codecs through shared recursive bindings without running any action or model" $ withPath $ \file -> do
    rt <- open file
    calls <- newIORef (0::Int)
    let requestCodec = C.reschema (S.titled "request") intCodec
        replyCodec = C.primitive SInteger (Integer . toInteger) $ \case
          Integer n | odd n -> Right (fromInteger n::Int)
          _ -> Left "reply must be odd"
        -- Both group members can reach b: directly from the model and through
        -- a recursive reference. The codecs belong to b's finite definition.
        (a,b) = recursiveTools $ \(_,b') ->
          (tool @Int @Int "a" "" (draftWith [b'] "call b")
          ,tool @Int @Int "b" "" (act (\n -> modifyIORef' calls (+1) >> pure n) >>> await requestCodec replyCodec))
        flow = draftWith @Int @Int [a,b] "both"
        provider = SystemTwo $ \c -> pure $ case (c.instruction.text,c.history) of
          ("both",[]) -> Turn (Raw Null) (CallTools [ToolCall "a" "a" (Integer 1),ToolCall "b" "b" (Integer 1)])
          ("call b",[]) -> Turn (Raw Null) (CallTools [ToolCall "b" "b" (Integer 1)])
          _ -> Turn (Raw Null) (Respond (Integer 1))
    progress <- resumable rt{systemTwo=provider} flow 0
    ws <- case progress of Waiting waits -> pure (NE.toList waits); _ -> fail "expected waits"
    length ws `shouldBe` 2
    readIORef calls `shouldReturn` 2
    let isolated = rt{observe=const (error "fulfil observed"),systemTwo=SystemTwo (const (error "fulfil called model"))}
    forM_ ws $ \w -> do
      w.requestSchema `shouldBe` requestCodec.schema
      w.replySchema `shouldBe` replyCodec.schema
      pendingWaitFromValue (pendingWaitValue w) `shouldBe` Right w
      fulfil isolated flow w (Integer 2) `shouldReturn` Left (ReplyInvalid "reply must be odd")
      fulfil isolated flow w (Integer 3) `shouldReturn` Right ()
    readIORef calls `shouldReturn` 2
