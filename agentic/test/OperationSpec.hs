module OperationSpec (spec) where

import Agentic
import Agentic.Runtime (Conversation (..))
import Agentic.Scripted (respond, callTools, fixedAnswers)
import Control.Exception (throwIO, ErrorCall (..), AsyncException (..))
import Control.Monad (when, forM_)
import Data.IORef
import Data.List (nub, inits)
import Data.List.NonEmpty qualified as NE
import ObservationModel (operationOrigins, OperationOrigin (..))
import Test.Hspec hiding (describe)
import Test.Hspec qualified as H
import Test.QuickCheck (ioProperty, property, conjoin, counterexample, (===))

-- Captured traces are metadata-only and each read resets the recording. The
-- completion store is shared by attempts, as a file-backed store would be.
fixture :: IO (Runtime IO, IO [Summary])
fixture = do
  records <- newIORef []
  events <- newIORef []
  let journal = Journal (JournalIdentity "operation-tests" "v1") (pure ())
        (\k -> lookup k <$> readIORef records)
        (\k v -> modifyIORef' records ((k,v):))
      rt = observing (\e -> modifyIORef' events (summarise e:)) runtime{journal=Just journal}
  pure (rt,reverse <$> atomicModifyIORef' events (\es -> ([],es)))

classified :: [Summary] -> [Summary]
classified = filter (\e -> case e.happened of Executing _ -> True; Restored _ -> True; _ -> False)

phases :: [Summary] -> [Happened]
phases = map (.happened) . classified

singleWait :: Progress a -> IO PendingWait
singleWait (RunWaiting (w NE.:| [])) = pure w
singleWait _ = fail "expected one wait"

spec :: Spec
spec = H.describe "live operation observations" $ do
  it "classifies live callbacks before invocation, and restores under the original key" $ do
    (rt,readEvents) <- fixture
    seen <- newIORef []
    let observe e = do
          modifyIORef' seen (e.happened:)
          case e.happened of
            Executing _ -> case e.payload of
              Nothing -> pure ()
              Just _ -> expectationFailure "execution classification carried a rich payload"
            _ -> pure ()
        action _ (n :: Int) = do
          readIORef seen >>= \es -> take 1 es `shouldBe` [Executing (KeyedOperation DurableEffect)]
          pure (n+1)
        flow = durably action
    interpret (observing observe rt) flow 1 `shouldReturn` 2
    first <- readEvents
    interpret rt flow 1 `shouldReturn` 2
    second <- readEvents
    phases first `shouldBe` [Executing (KeyedOperation DurableEffect)]
    phases second `shouldBe` [Restored DurableEffect]
    [live] <- pure (classified first)
    [restored] <- pure (classified second)
    operationOrigins first `shouldBe` Right [(live.occurrence,KeyedOperation DurableEffect,Live)]
    operationOrigins second `shouldBe` Right [(restored.occurrence,KeyedOperation DurableEffect,FromJournal)]
    live.occurrence `shouldBe` restored.occurrence
    journalKey DurableEffect live.occurrence `shouldBe` journalKey DurableEffect restored.occurrence

  it "maps restoration to the original journal key even when Each totals change" $ do
    (rt,readEvents) <- fixture
    let flow = each (durably (\_ (n :: Int) -> pure (n+1)))
    interpret rt flow [1] `shouldReturn` [2]
    first <- readEvents
    interpret rt flow [1,2] `shouldReturn` [2,3]
    second <- readEvents
    [original] <- pure (classified first)
    [restored] <- pure [e | e <- second, e.happened == Restored DurableEffect]
    original.occurrence `shouldNotBe` restored.occurrence
    journalKey DurableEffect original.occurrence `shouldBe` journalKey DurableEffect restored.occurrence
    phases second `shouldBe` [Restored DurableEffect,Executing (KeyedOperation DurableEffect)]

  it "reports plain actions as live on every attempt, with or without a journal" $ do
    forM_ [False,True] $ \journaled -> do
      (rt,readEvents) <- fixture
      count <- newIORef (0 :: Int)
      let configured = if journaled then rt else rt{journal=Nothing}
          flow = act (\() -> modifyIORef' count (+1))
      interpret configured flow ()
      first <- readEvents
      interpret configured flow ()
      second <- readEvents
      phases first `shouldBe` [Executing PlainAction]
      phases second `shouldBe` [Executing PlainAction]
      readIORef count `shouldReturn` 2

  it "suspends without execution, then restores a newly accepted reply at the same wait" $ do
    (rt,readEvents) <- fixture
    let flow = await @Bool :: Agentic IO Int Bool
    w <- resumable rt flow 7 >>= singleWait
    first <- readEvents
    phases first `shouldBe` []
    [e.happened | e <- first, e.nodeType == AwaitNode] `shouldBe` [Entered Nothing,Suspended]
    fulfil rt flow w (Bool True) `shouldReturn` Right ()
    resumable rt flow 7 `shouldReturn` RunFinished True
    second <- readEvents
    phases second `shouldBe` [Restored AwaitedReply]
    [restored] <- pure (classified second)
    journalKey AwaitedReply restored.occurrence `shouldBe` w.key.key
    resumable rt flow 7 `shouldReturn` RunFinished True
    third <- readEvents
    map (.occurrence) (classified third) `shouldBe` [restored.occurrence]

  it "re-enters an incomplete tool live while restoring its completed work and first model turn" $ ioProperty $ do
    (rt,readEvents) <- fixture
    count <- newIORef (0 :: Int)
    providerTraces <- newIORef []
    let body = durably (\_ (n :: Int) -> modifyIORef' count (+1) >> pure n) >>> await @Int
        flow = draftWith @Int @Int [tool "review" "" body] "go"
        provider = SystemTwo $ \c -> do
          es <- readEvents
          -- Keep the trace for assertions as well as checking call order.
          modifyIORef' providerTraces (<> es)
          phases es `shouldSatisfy` (not . null)
          last (phases es) `shouldBe` Executing (KeyedOperation (DraftTurn (length c.history)))
          pure (Turn (Raw Null) (if null c.history then callTools [("review",Integer 3)] else respond (9 :: Int)))
    let configured = rt{systemTwo=provider}
    w <- resumable configured flow 0 >>= singleWait
    tailFirst <- readEvents
    first <- (<> tailFirst) <$> readIORef providerTraces
    writeIORef providerTraces []
    fulfil rt flow w (Integer 4) `shouldReturn` Right ()
    resumable configured flow 0 `shouldReturn` RunFinished 9
    tailSecond <- readEvents
    second <- (<> tailSecond) <$> readIORef providerTraces
    phases first `shouldBe` [Executing (KeyedOperation (DraftTurn 0)),Executing (KeyedOperation ToolExecution)
      ,Executing (KeyedOperation DurableEffect)]
    phases second `shouldBe` [Restored (DraftTurn 0),Executing (KeyedOperation ToolExecution)
      ,Restored DurableEffect,Restored AwaitedReply,Executing (KeyedOperation (DraftTurn 1))]
    readIORef count `shouldReturn` 1
    let toolKeys es = [e.occurrence | e <- classified es, e.nodeType == CallNode]
    toolKeys first `shouldBe` toolKeys second
    -- The model rejects any double classification and accepts every prefix.
    pure $ property $ conjoin [counterexample (show prefix) (valid prefix === True)
      | trace <- [first,second], prefix <- inits trace]

  it "leaves pre-classification prefixes unknown and prevents callbacks when classification delivery fails" $ do
    forM_ [Drafting,Executing (KeyedOperation (DraftTurn 0))] $ \stop -> do
      forM_ [(cancel,recordFirst) | cancel <- [False,True], recordFirst <- [False,True]] $ \(cancel,recordFirst) -> do
        (rt,readEvents) <- fixture
        calls <- newIORef (0 :: Int)
        let abort e = when (e.happened == stop) $
              if cancel then throwIO ThreadKilled else throwIO (ErrorCall "observer stopped")
            provider = SystemTwo (\_ -> modifyIORef' calls (+1) >> pure (Turn (Raw Null) (respond (1 :: Int))))
            configured = if recordFirst then observing abort rt
              else rt{observe = \e -> abort e >> rt.observe e}
            run = interpret configured{systemTwo=provider} (draft @Int @Int "go") 0
        if cancel then run `shouldThrow` (== ThreadKilled) else run `shouldThrow` anyErrorCall
        events <- readEvents
        readIORef calls `shouldReturn` 0
        if stop == Drafting || not recordFirst then operationOrigins events `shouldBe` Right []
          else phases events `shouldBe` [Executing (KeyedOperation (DraftTurn 0))]

  it "retries a failed operation as a fresh execution episode at the same key" $ do
    (rt,readEvents) <- fixture
    failed <- newIORef True
    let flow = durably (\_ (n :: Int) -> readIORef failed >>= \bad ->
          if bad then throwIO (ErrorCall "retry") else pure n)
    interpret rt flow 1 `shouldThrow` anyErrorCall
    first <- readEvents
    writeIORef failed False
    interpret rt flow 1 `shouldReturn` 1
    second <- readEvents
    phases first `shouldBe` [Executing (KeyedOperation DurableEffect)]
    phases second `shouldBe` phases first
    map (.occurrence) (classified first) `shouldBe` map (.occurrence) (classified second)

  it "does not invent execution for an empty judge batch" $ do
    (rt,readEvents) <- fixture
    interpret rt (judge (pure ()) :: Agentic IO Int ()) 0 `shouldReturn` ()
    events <- readEvents
    phases events `shouldBe` []
    [e.happened | e <- events, e.nodeType == JudgeNode] `shouldBe` [Entered Nothing,Judged,Returned Completed]

  it "classifies non-journaled model turns and judgements too" $ do
    (rt,readEvents) <- fixture
    let configured = rt{journal=Nothing,systemOne=fixedAnswers 1
                       ,systemTwo=SystemTwo (\_ -> pure (Turn (Raw Null) (respond (5 :: Int))))}
    _ <- interpret configured (judge (yesNo "yes?") >>> draft @Int "go") (0 :: Int)
    events <- readEvents
    phases events `shouldBe` [Executing (KeyedOperation Judgement),Executing (KeyedOperation (DraftTurn 0))]

  it "keeps distinct loop iterations and call positions despite reused call ids" $ do
    (rt,readEvents) <- fixture
    let loop = repeatUntil (>=3) (durably (\_ n -> pure (n+1))) :: Agentic IO Int Int
    interpret rt loop 0 `shouldReturn` 3
    events <- readEvents
    length (nub (map (journalKey DurableEffect . (.occurrence)) (classified events))) `shouldBe` 3
    let flow = draftWith @Int @Int [tool "id" "" (act pure :: Agentic IO Int Int)] "go"
        provider = SystemTwo $ \c -> pure (Turn (Raw Null) (if null c.history
          then CallTools [ToolCall "same" "id" (Integer 1),ToolCall "same" "id" (Integer 1)] else respond (2 :: Int)))
    interpret rt{systemTwo=provider} flow 0 `shouldReturn` 2
    calls <- readEvents
    let keys = [journalKey ToolExecution e.occurrence | e <- classified calls, e.nodeType == CallNode]
    length keys `shouldBe` 2
    length (nub keys) `shouldBe` 2

  it "reports unknown and invalid tool evaluation without claiming entry into the body" $ do
    (rt,readEvents) <- fixture
    touched <- newIORef False
    let flow = draftWith @Int @Int [tool "known" "" (act (\(n :: Int) -> writeIORef touched True >> pure n))] "go"
        provider = SystemTwo $ \c -> pure (Turn (Raw Null) (if null c.history
          then callTools [("missing",Integer 1),("known",String "bad input")] else respond (2 :: Int)))
    interpret rt{systemTwo=provider} flow 0 `shouldReturn` 2
    readIORef touched `shouldReturn` False
    events <- readEvents
    let calls = [e | e <- events, e.nodeType == CallNode]
        keys = nub (map (.occurrence) calls)
    case keys of
      [unknown,invalid] -> do
        [e.happened | e <- calls, e.occurrence == unknown] `shouldBe`
          [Entered Nothing,Executing (KeyedOperation ToolExecution),Returned UnknownTool]
        case [e.happened | e <- calls, e.occurrence == invalid] of
          [Entered Nothing,CallResolved _ _,Executing (KeyedOperation ToolExecution),Returned InvalidInput] -> pure ()
          other -> expectationFailure (show other)
      _ -> expectationFailure "expected two calls"

  it "encodes classifications as metadata and rejects invalid operation grammars" $ do
    (rt,readEvents) <- fixture
    interpret rt (act pure :: Agentic IO () ()) ()
    events <- readEvents
    [e] <- pure (classified events)
    let sample happened kind = Summary e.occurrence e.parent kind e.path happened
        encoded happened = case summaryValue (sample happened e.nodeType) of
          Object fs -> lookupField "event" fs
          _ -> Nothing
    encoded (Executing PlainAction) `shouldBe` Just (Object
      [("kind",String "executing"),("operation",Object [("kind",String "action")])])
    encoded (Executing (KeyedOperation (DraftTurn 3))) `shouldBe` Just (Object
      [("kind",String "executing"),("operation",Object
        [("kind",String "keyed"),("operation",Object [("turn",Integer 3)])])])
    encoded (Restored (DraftTurn 3)) `shouldBe` Just (Object
      [("kind",String "restored"),("operation",Object [("turn",Integer 3)])])
    valid [e,e] `shouldBe` False
    valid [sample (Returned Completed) EffectNode] `shouldBe` False
    valid [sample (Executing PlainAction) EffectNode
          ,sample (Executing (KeyedOperation DurableEffect)) EffectNode] `shouldBe` False
    valid [sample (Executing (KeyedOperation AwaitedReply)) AwaitNode] `shouldBe` False
    valid [sample (Turned 0 Response) DraftNode] `shouldBe` False
    valid [sample (Executing (KeyedOperation DurableEffect)) EffectNode,sample (Restored DurableEffect) EffectNode]
      `shouldBe` False

valid :: [Summary] -> Bool
valid = either (const False) (const True) . operationOrigins
