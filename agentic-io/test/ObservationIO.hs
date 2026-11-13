module ObservationIO (spec) where

import Agentic hiding (record, timeout, left, right, gate)
import Agentic.IO.Concurrent (concurrently)
import Control.Concurrent (forkIO, newEmptyMVar, newMVar, modifyMVar_, readMVar, putMVar, takeMVar)
import Control.Exception
import Data.IORef
import Data.List (isInfixOf)
import System.Timeout (timeout)
import Test.Hspec hiding (describe)
import Test.Hspec qualified as H

data BodyFailure = BodyFailure deriving (Eq, Show)
instance Exception BodyFailure

record :: IO (Runtime IO, IO [Summary])
record = do
  ref <- newIORef []
  pure (observing (\e -> atomicModifyIORef' ref (\xs -> (summarise e:xs,()))) runtime, reverse <$> readIORef ref)

checkPrefixes :: Agentic IO a b -> [Summary] -> Expectation
checkPrefixes flow es = go [] [] es
 where
  graph = flowGraph (either (error . show) id (describe flow))
  go _ _ [] = pure ()
  go entered closed (e:rest) = do
    e.occurrence.address `shouldSatisfy` (`elem` map fst graph.addressItems)
    case e.happened of
      Entered _ -> do
        e.occurrence `shouldSatisfy` (`notElem` entered)
        case e.parent of
          Nothing -> e.nodeType `shouldBe` RunNode
          Just p -> do
            p `shouldSatisfy` (`elem` entered)
            p `shouldSatisfy` (`notElem` closed)
        go (e.occurrence:entered) closed rest
      phase -> do
        e.occurrence `shouldSatisfy` (`elem` entered)
        e.occurrence `shouldSatisfy` (`notElem` closed)
        let terminal = case phase of Returned _ -> True; Failed -> True; Cancelled -> True; _ -> False
        go entered (if terminal then e.occurrence:closed else closed) rest

spec :: Spec
spec = H.describe "IO observation scopes" $ do
  it "reports a failed sibling and cancels the other before the parent exits" $ do
    ready <- newEmptyMVar
    blocked <- newEmptyMVar
    (rt,readEvents) <- record
    let left = act (\() -> takeMVar ready >> throwIO BodyFailure) :: Agentic IO () ()
        right = act (\() -> putMVar ready () >> takeMVar blocked) :: Agentic IO () ()
        flow = left &&& right
    interpret (concurrently rt) flow () `shouldThrow` (== BodyFailure)
    es <- readEvents
    checkPrefixes flow es
    [e.happened | e <- es, e.nodeType == EffectNode, e.happened /= Entered Nothing] `shouldMatchList` [Failed,Cancelled]
    (last es).happened `shouldBe` Failed

  it "records two concurrent failures without claiming a causal origin" $ do
    (rt,readEvents) <- record
    failures <- newMVar (0 :: Int)
    bothFailed <- newEmptyMVar
    let waitForBoth e = if e.nodeType == EffectNode && e.happened == Failed
          then do
            modifyMVar_ failures $ \n -> do
              if n == 1 then putMVar bothFailed () else pure ()
              pure (n + 1)
            readMVar bothFailed
          else pure ()
        flow = each (act (\() -> throwIO BodyFailure) :: Agentic IO () ())
    interpret (concurrently (observing waitForBoth rt)) flow [(),()] `shouldThrow` (== BodyFailure)
    es <- readEvents
    checkPrefixes flow es
    [e.happened | e <- es, e.nodeType == EffectNode, e.happened /= Entered Nothing] `shouldBe` [Failed,Failed]
    (last es).happened `shouldBe` Failed

  it "joins children before reporting parent cancellation" $ do
    ready <- newEmptyMVar
    blocked <- newEmptyMVar
    done <- newEmptyMVar
    (rt,readEvents) <- record
    let flow = each (act (\() -> putMVar ready () >> takeMVar blocked)) :: Agentic IO [()] [()]
    thread <- forkIO $ (try (interpret (concurrently rt) flow [(),()]) :: IO (Either SomeException [()])) >>= putMVar done
    takeMVar ready
    takeMVar ready
    throwTo thread ThreadKilled
    result <- takeMVar done
    case result of
      Left e -> (fromException e :: Maybe AsyncException) `shouldBe` Just ThreadKilled
      Right _ -> expectationFailure "cancelled run returned"
    es <- readEvents
    checkPrefixes flow es
    [e.happened | e <- es, e.nodeType == EffectNode, e.happened /= Entered Nothing] `shouldBe` [Cancelled,Cancelled]
    (last es).happened `shouldBe` Cancelled

  it "classifies timeout as cancellation and resource exceptions as failure" $ do
    (rt,readEvents) <- record
    blocked <- newEmptyMVar
    timeout 10000 (interpret rt (act (\() -> takeMVar blocked) :: Agentic IO () ()) ()) `shouldReturn` Nothing
    es <- readEvents
    [e.happened | e <- es, e.nodeType == EffectNode] `shouldBe` [Entered Nothing,Cancelled]
    mapM_ (\exception -> do
      (rt',readEvents') <- record
      interpret rt' (act (\() -> throwIO exception) :: Agentic IO () ()) () `shouldThrow` (== exception)
      es' <- readEvents'
      [e.happened | e <- es', e.nodeType == EffectNode] `shouldBe` [Entered Nothing,Failed]) [StackOverflow,HeapOverflow]

  it "preserves the original exception type through custom parallel" $ do
    caught <- newIORef False
    let run actions = sequence actions `catch` \(e :: BodyFailure) -> writeIORef caught True >> throwIO e
        rt = runtime {parallel=run}
    interpret rt (each (act (\() -> throwIO BodyFailure) :: Agentic IO () ())) [()] `shouldThrow` (== BodyFailure)
    readIORef caught `shouldReturn` True

  it "leaves entry or return delivery incomplete when cancellation interrupts the observer" $
    mapM_ (\(phase,recordFirst) -> do
      gate <- newEmptyMVar
      blocked <- newEmptyMVar
      done <- newEmptyMVar
      (rt,readEvents) <- record
      let flow = arr id :: Agentic IO Int Int
          observer e = if e.nodeType == PureNode && e.happened == phase
            then putMVar gate () >> takeMVar blocked else pure ()
      let observed = if recordFirst then observing observer rt else rt{observe = \e -> observer e >> rt.observe e}
      thread <- forkIO $ (try (interpret observed flow 1) :: IO (Either SomeException Int)) >>= putMVar done
      takeMVar gate
      throwTo thread ThreadKilled
      _ <- takeMVar done
      es <- readEvents
      checkPrefixes flow es
      [e.happened | e <- es, e.nodeType == PureNode] `shouldBe`
        (if phase == Entered Nothing then [Entered Nothing | recordFirst] else [Entered Nothing] <> [Returned Completed | recordFirst])
      (last es).happened `shouldBe` Cancelled) [(phase,recordFirst) | phase <- [Entered Nothing, Returned Completed], recordFirst <- [False,True]]

  it "preserves a body failure if cancellation interrupts its failure report" $ do
    gate <- newEmptyMVar
    blocked <- newEmptyMVar
    done <- newEmptyMVar
    (rt,readEvents) <- record
    let flow = act (\() -> throwIO BodyFailure) :: Agentic IO () ()
        observer e = if e.nodeType == EffectNode && e.happened == Failed
          then putMVar gate () >> takeMVar blocked else pure ()
    thread <- forkIO $ (try (interpret (observing observer rt) flow ()) :: IO (Either SomeException ())) >>= putMVar done
    takeMVar gate
    throwTo thread ThreadKilled
    result <- takeMVar done
    case result of
      Left e -> (fromException e :: Maybe BodyFailure) `shouldBe` Just BodyFailure
      Right _ -> expectationFailure "failure swallowed"
    es <- readEvents
    checkPrefixes flow es
    (last es).happened `shouldBe` Failed

  it "preserves all concurrent occurrences despite out-of-order sibling completion" $ do
    gate <- newEmptyMVar
    (rt,readEvents) <- record
    let flow = each (act (\i -> if i == (0::Int) then takeMVar gate >> pure i else putMVar gate () >> pure i))
    interpret (concurrently rt) flow [0,1] `shouldReturn` [0,1]
    es <- readEvents
    checkPrefixes flow es
    length [() | e <- es, e.nodeType == EffectNode, Returned _ <- [e.happened]] `shouldBe` 2

  it "allows a parent failure after its child has returned" $ do
    (rt,readEvents) <- record
    let condition n = if n == (0::Int) then False else error "predicate"
        flow = repeatUntil condition (arr (+1))
    interpret rt flow 0 `shouldThrow` (\(e :: ErrorCall) -> "predicate" `isInfixOf` show e)
    es <- readEvents
    checkPrefixes flow es
    [e.happened | e <- es, e.nodeType == PureNode] `shouldBe` [Entered Nothing,Returned Completed]
    [e.happened | e <- es, e.nodeType == RepeatNode] `shouldBe` [Entered Nothing,Failed]
