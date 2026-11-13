module ExceptScopeSpec (spec) where

import Agentic hiding (timeout)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, throwTo)
import Control.Exception (Exception, AsyncException (..), SomeException, throwIO, try, getMaskingState, MaskingState (..))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Data.IORef
import Test.Hspec hiding (describe)
import Test.Hspec qualified as H
import System.Timeout (timeout)

data Problem = Primary | Observer deriving (Eq, Show)
data BodyException = BodyException deriving (Eq, Show)
instance Exception BodyException

wrapped :: Monad m => Scope m -> (Happened -> m ()) -> m a -> m a
wrapped hook report body = scopeWith hook
  (Right () <$ report (Entered Nothing))
  (\a -> report (Returned Completed) >> pure a)
  (\h _ -> report h)
  (Right <$> body)

recorded :: (Happened -> ExceptT Problem IO ()) -> ExceptT Problem IO a -> IO (Either Problem a, [Happened])
recorded notify body = do
  ref <- newIORef []
  let report h = liftIO (modifyIORef' ref (h:)) >> notify h
  result <- runExceptT (wrapped (exceptTScope ioScope) report body)
  events <- reverse <$> readIORef ref
  pure (result,events)

flowRuntime :: IO (Runtime (ExceptT FlowError IO), IO [Summary])
flowRuntime = do
  ref <- newIORef []
  pure ((runtimeWith throwE)
    {scope = exceptTScope ioScope
    ,observe = \e -> liftIO (modifyIORef' ref (summarise e:))}
    ,reverse <$> readIORef ref)

spec :: Spec
spec = H.describe "ExceptT scopes" $ do
  it "returns Right without forcing the value, and requires no Show for errors" $ do
    (result,events) <- recorded (const (pure ())) (pure (error "lazy result" :: Int))
    case result of Right _ -> pure (); Left _ -> expectationFailure "failed"
    events `shouldBe` [Entered Nothing,Returned Completed]
    -- A function error type has no Show instance.
    noShow <- runExceptT (wrapped (exceptTScope ioScope) (const (pure ()))
      (throwE (const True) :: ExceptT (Int -> Bool) IO ()))
    case noShow of Left f -> f 1 `shouldBe` True; Right _ -> expectationFailure "lost error"

  it "does not force an error value while reporting failure" $ do
    (result,events) <- recorded (const (pure ()))
      (throwE (error "lazy error") :: ExceptT Problem IO ())
    case result of Left _ -> pure (); Right _ -> expectationFailure "lost error"
    events `shouldBe` [Entered Nothing,Failed]

  it "keeps notifications masked and restores only the body" $ do
    ref <- newIORef []
    let note = liftIO (getMaskingState >>= \state -> modifyIORef' ref (state:))
    runExceptT (wrapped (exceptTScope ioScope) (const note) note :: ExceptT Problem IO ()) `shouldReturn` Right ()
    reverse <$> readIORef ref `shouldReturn` [MaskedInterruptible,Unmasked,MaskedInterruptible]

  it "reports nested Left exits in order and preserves the original error" $ do
    (rt,events) <- flowRuntime
    let flow = act (\() -> throwE NoSystemOne) `named` "body"
    runExceptT (interpret rt flow ()) `shouldReturn` (Left NoSystemOne :: Either FlowError ())
    es <- events
    map (.happened) es `shouldBe` replicate 3 (Entered Nothing) <> replicate 3 Failed
    map (.nodeType) es `shouldBe` [RunNode,NoteNode,EffectNode,EffectNode,NoteNode,RunNode]
    map (.occurrence) (drop 3 es) `shouldBe` reverse (map (.occurrence) (take 3 es))

  it "observes FlowError from the runtime failure function" $ do
    (rt,events) <- flowRuntime
    runExceptT (interpret rt (draft @Int @Int "no provider") 1) `shouldReturn` Left NoSystemTwo
    es <- events
    [e.happened | e <- es, e.nodeType == RunNode] `shouldBe` [Entered Nothing,Failed]
    [e.happened | e <- es, e.nodeType == DraftNode, e.happened == Failed] `shouldBe` [Failed]

  it "entry short circuits prevent the body and any terminal" $ do
    (result,events) <- recorded (\h -> when (h == Entered Nothing) (throwE Observer))
      (error "body must not run" :: ExceptT Problem IO ())
    result `shouldBe` Left Observer
    events `shouldBe` [Entered Nothing]

  it "normal-return short circuits do not produce a second terminal" $ do
    (result,events) <- recorded (\h -> when (h == Returned Completed) (throwE Observer)) (pure ())
    result `shouldBe` Left Observer
    events `shouldBe` [Entered Nothing,Returned Completed]

  it "failure-report short circuits preserve the original Left" $ do
    (result,events) <- recorded (\h -> when (h == Failed) (throwE Observer)) (throwE Primary :: ExceptT Problem IO ())
    result `shouldBe` Left Primary
    events `shouldBe` [Entered Nothing,Failed]

  it "failure-report IO exceptions also preserve the original Left" $ do
    (result,events) <- recorded (\h -> when (h == Failed) (liftIO (throwIO BodyException))) (throwE Primary :: ExceptT Problem IO ())
    result `shouldBe` Left Primary
    events `shouldBe` [Entered Nothing,Failed]

  it "entry and return IO exceptions propagate without a second terminal" $ do
    mapM_ (\phase -> do
      ref <- newIORef []
      let notify h = liftIO (modifyIORef' ref (h:)) >> when (h == phase) (liftIO (throwIO BodyException))
          body = if phase == Entered Nothing then error "body must not run" else pure ()
      runExceptT (wrapped (exceptTScope ioScope) notify body :: ExceptT Problem IO ()) `shouldThrow` (== BodyException)
      reverse <$> readIORef ref `shouldReturn` (if phase == Entered Nothing then [Entered Nothing] else [Entered Nothing,Returned Completed]))
      [Entered Nothing,Returned Completed]

  it "preserves base exception and cancellation types despite observer Left" $ do
    ref <- newIORef []
    let report h = liftIO (modifyIORef' ref (h:)) >> when (h == Failed || h == Cancelled) (throwE Observer)
        run action = runExceptT (wrapped (exceptTScope ioScope) report (liftIO action) :: ExceptT Problem IO ())
    run (throwIO BodyException) `shouldThrow` (== BodyException)
    reverse <$> readIORef ref `shouldReturn` [Entered Nothing,Failed]
    writeIORef ref []
    run (throwIO ThreadKilled) `shouldThrow` (== ThreadKilled)
    reverse <$> readIORef ref `shouldReturn` [Entered Nothing,Cancelled]

  it "lifts every ExceptT layer, preserving either layer's body error" $ do
    let hook = exceptTScope (exceptTScope ioScope) :: Scope (ExceptT Problem (ExceptT Int IO))
    mapM_ (\outer -> do
      ref <- newIORef []
      let report h = liftIO (modifyIORef' ref (h:))
          body = if outer then throwE Primary else lift (throwE 42)
      result <- runExceptT (runExceptT (wrapped hook report (wrapped hook report body) :: ExceptT Problem (ExceptT Int IO) ()))
      result `shouldBe` if outer then Right (Left Primary) else Left 42
      reverse <$> readIORef ref `shouldReturn` [Entered Nothing,Entered Nothing,Failed,Failed]) [True,False]

  it "honours explicit entry aborts and body failures through nested layers" $ do
    let hook = exceptTScope (exceptTScope ioScope) :: Scope (ExceptT Problem (ExceptT Int IO))
        run = runExceptT . runExceptT
    ref <- newIORef []
    let returned :: () -> ExceptT Problem (ExceptT Int IO) String
        returned () = error "must not return"
        exited h _ = liftIO (modifyIORef' ref (h:)) >> lift (throwE 99)
    run (scopeWith hook (pure (Left "entry")) returned exited (error "must not run")) `shouldReturn` Right (Right "entry")
    readIORef ref `shouldReturn` []
    run (scopeWith hook (pure (Right ())) returned exited (pure (Left "body"))) `shouldReturn` Right (Right "body")
    readIORef ref `shouldReturn` [Failed]

  it "propagates nested entry and return callback errors without extra terminals" $ do
    let hook = exceptTScope (exceptTScope ioScope) :: Scope (ExceptT Problem (ExceptT Int IO))
    mapM_ (\(phase,outer) -> do
      ref <- newIORef []
      let report h = do
            liftIO (modifyIORef' ref (h:))
            when (h == phase) (if outer then throwE Observer else lift (throwE 42))
          body = if phase == Entered Nothing then error "must not run" else pure ()
      result <- runExceptT (runExceptT (wrapped hook report body))
      result `shouldBe` if outer then Right (Left Observer) else Left 42
      reverse <$> readIORef ref `shouldReturn` (if phase == Entered Nothing then [Entered Nothing] else [Entered Nothing,Returned Completed]))
      [(phase,outer) | phase <- [Entered Nothing,Returned Completed], outer <- [False,True]]

  it "keeps either layer's primary error when failure reporting short circuits in either layer" $ do
    let hook = exceptTScope (exceptTScope ioScope) :: Scope (ExceptT Problem (ExceptT Int IO))
    mapM_ (\(bodyOuter,reportOuter) -> do
      ref <- newIORef []
      let report h = do
            liftIO (modifyIORef' ref (h:))
            when (h == Failed) (if reportOuter then throwE Observer else lift (throwE 99))
          body = if bodyOuter then throwE Primary else lift (throwE 42)
      result <- runExceptT (runExceptT (wrapped hook report body :: ExceptT Problem (ExceptT Int IO) ()))
      result `shouldBe` if bodyOuter then Right (Left Primary) else Left 42
      reverse <$> readIORef ref `shouldReturn` [Entered Nothing,Failed])
      [(b,r) | b <- [False,True], r <- [False,True]]

  it "preserves Left when cancellation interrupts its report, and unwinds ancestors as Failed" $ do
    (rt,events) <- flowRuntime
    ready <- newEmptyMVar
    blocked <- newEmptyMVar
    done <- newEmptyMVar
    let report e = when (e.nodeType == EffectNode && e.happened == Failed) $
          liftIO (putMVar ready () >> takeMVar blocked)
        flow = act (\() -> throwE NoSystemOne) :: Agentic (ExceptT FlowError IO) () ()
    tid <- forkIO $ do
      result <- try (runExceptT (interpret (observing report rt) flow ()))
      putMVar done (result :: Either SomeException (Either FlowError ()))
    takeMVar ready
    throwTo tid ThreadKilled
    result <- timeout 1000000 (takeMVar done)
    case result of
      Just (Right value) -> value `shouldBe` Left NoSystemOne
      _ -> expectationFailure "original Left lost or worker did not unwind"
    es <- events
    map (.happened) es `shouldBe` [Entered Nothing,Entered Nothing,Failed,Failed]

  it "documents the real timeout consequence at the failure-report boundary" $ do
    blocked <- newEmptyMVar
    let report h = when (h == Failed) (liftIO (takeMVar blocked))
    timeout 20000 (recorded report (throwE Primary :: ExceptT Problem IO ()))
      `shouldReturn` Just (Left Primary,[Entered Nothing,Failed])
