module JournalSpec (spec) where

import Agentic
import Agentic.Codec qualified as C
import Agentic.Schema (Shape (..))
import Control.Exception (ErrorCall (..), throwIO)
import Data.Functor.Identity
import Data.IORef
import Data.Text qualified as T
import Test.Hspec hiding (describe)
import Test.Hspec qualified as H

intCodec :: C.Codec Int
intCodec = C.primitive SInteger (Integer . toInteger) $ \case
  Integer n -> Right (fromInteger n)
  _ -> Left "expected integer"

-- A small pure store demonstrates that core does not require IO or exceptions.
newtype Memory a = Memory {runMemory :: [(Value,Value)] -> Either FlowError (a,[(Value,Value)])}
instance Functor Memory where
  fmap f (Memory g) = Memory (fmap (\(a,s) -> (f a,s)) . g)
instance Applicative Memory where
  pure a = Memory (\s -> Right (a,s))
  f <*> a = f >>= \g -> fmap g a
instance Monad Memory where
  Memory a >>= f = Memory $ \s -> do
    (x,s') <- a s
    (f x).runMemory s'

spec :: Spec
spec = H.describe "journal core" $ do
  it "runs without a journal without evaluating codecs or inventing a key" $ do
    let flow = durably (error "input codec") (error "output codec") $ \key (x :: Int) ->
          key `seq` pure (key,x)
    let (key,x) = runIdentity (interpret (runtimeWith (error . show)) flow 7)
    key `shouldBe` Nothing
    x `shouldBe` 7

  it "describes durable effects without running them or demanding their codecs" $ do
    let flow = durably (error "input codec") (error "output codec")
          (\_ _ -> error "must not run") :: Agentic IO () ()
    case describe flow of
      Left e -> expectationFailure (show e)
      Right d -> do
        renderTree d `shouldSatisfy` T.isInfixOf "durably"
        mermaid d `shouldSatisfy` T.isInfixOf "durably"

  it "replays in a pure monad and guards hits, without requiring IO" $ do
    let raise e = Memory (const (Left e))
        journal = Journal (JournalIdentity "pure-run" "r1") (pure ())
          (\key -> Memory (\s -> Right (lookup key s,s)))
          (\key value -> Memory (\s -> Right ((),(key,value):s)))
        rt = (runtimeWith raise){journal=Just journal}
        f = durably intCodec intCodec (\_ x -> pure (x+1))
    case (interpret rt f 4).runMemory [] of
      Left e -> expectationFailure (show e)
      Right (a,records) -> do
        a `shouldBe` 5
        (interpret rt f 4).runMemory records `shouldBe` Right (5,records)
        (interpret rt f 6).runMemory records `shouldSatisfy` \case
          Left (JournalFailure (JournalDiverged _)) -> True
          _ -> False

  it "drops totals from every Each frame, leaving observation identity intact" $ do
    ref <- newIORef []
    let flow = each (each (act pure)) :: Agentic IO [[Int]] [[Int]]
        rt = observing (\e -> modifyIORef' ref (e:)) runtime
    _ <- interpret rt flow [[1]]
    first <- readIORef ref
    writeIORef ref []
    _ <- interpret rt flow [[1,2],[3]]
    second <- readIORef ref
    let effects es = reverse [e.occurrence | e <- es, e.nodeType == EffectNode, e.happened == Entered Nothing]
    case (effects first,effects second) of
      ([a],b:_) -> do
        a `shouldNotBe` b
        journalKey DurableEffect a `shouldBe` journalKey DurableEffect b
        journalKeyValue (journalKey DurableEffect a) `shouldNotBe` occurrenceValue a
        journalKey DurableEffect a `shouldNotBe` journalKey ToolExecution a
        journalKey (DraftTurn 0) a `shouldNotBe` journalKey (DraftTurn 1) a
      _ -> expectationFailure "missing effect observations"

  it "does no journal lookups for pure plumbing or plain act" $ do
    let journal = Journal (JournalIdentity "run" "r1") (pure ())
          (\_ -> error "pure step looked up a key") (\_ _ -> error "pure step saved a record")
        flow = each (arr (+1) >>> act pure) :: Agentic IO [Int] [Int]
    interpret runtime{journal=Just journal} flow [1,2] `shouldReturn` [2,3]

  it "rejects ambiguous JSON values and compares semantic object/number forms" $ do
    canonicalValue (Object [("x",Integer 1),("x",Integer 2)]) `shouldSatisfy` isLeft
    canonicalValue (Number (0/0)) `shouldSatisfy` isLeft
    canonicalValue (Object [("b",Number 2),("a",Array [Number 1])])
      `shouldBe` Right (Object [("a",Array [Integer 1]),("b",Integer 2)])

  it "forces encoded values before invoking save" $ do
    saved <- newIORef False
    let journal = Journal (JournalIdentity "run" "r1") (pure ())
          (const (pure Nothing)) (\_ _ -> writeIORef saved True)
        codec = C.Codec intCodec.schema (\_ -> Array [Integer 1,error "bad encoded tail"]) intCodec.decode
        f = durably intCodec codec (\_ x -> pure x)
    interpret runtime{journal=Just journal} f 1 `shouldThrow` anyErrorCall
    readIORef saved `shouldReturn` False

  it "fails lookup/init errors rather than invoking the effect" $ do
    effect <- newIORef False
    let f = durably intCodec intCodec (\_ x -> writeIORef effect True >> pure x)
        journal = Journal (JournalIdentity "run" "r1") (pure ())
          (const (throwIO (ErrorCall "lookup failed"))) (\_ _ -> pure ())
    interpret runtime{journal=Just journal} f 1 `shouldThrow` anyErrorCall
    readIORef effect `shouldReturn` False
    let bad = journal{initialise = throwIO (ErrorCall "wrong revision")}
    seen <- newIORef []
    let observed = observing (\e -> modifyIORef' seen (e.happened:)) runtime{journal=Just bad}
    interpret observed (act (\() -> writeIORef effect True)) () `shouldThrow` anyErrorCall
    readIORef effect `shouldReturn` False
    reverse <$> readIORef seen `shouldReturn` [Entered Nothing,Failed]
 where
  isLeft (Left _) = True
  isLeft _ = False
