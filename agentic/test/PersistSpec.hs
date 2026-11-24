module PersistSpec (spec) where

import Agentic
import Control.Exception (evaluate)
import Data.IORef
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Test.Hspec hiding (describe)
import Test.Hspec qualified as H
import Test.QuickCheck

data Packet = Packet {tag :: Text, hidden :: Int, fields :: Maybe (Maybe ())}
  deriving (Eq, Show, Generic, Persist)
-- The model intentionally cannot see hidden. Persistence must still save it.
instance Contract Packet where
  contract = record "model view" (Packet <$> required "tag" "" (.tag) <*> pure 0 <*> pure Nothing)
data Reply = Yes | No | Revise Text (Maybe Int)
  deriving (Eq, Show, Generic, Persist)
data Recursive = Recur (Maybe Recursive) deriving (Generic, Persist)
data MutualA = MutualA MutualB deriving (Generic, Persist)
data MutualB = MutualB MutualA deriving (Generic, Persist)

newtype Wrapped = Wrapped (Maybe (Maybe Int))
  deriving stock (Eq, Show, Generic)
  deriving anyclass Persist

roundTrip :: forall a. (Eq a, Show a, Persist a) => a -> Property
roundTrip x = counterexample (show encoded) $
  (canonicalValue encoded >>= (persistence @a).decode) === Right x
 where encoded = (persistence @a).encode x

spec :: Spec
spec = H.describe "derived full-value persistence" $ do
  it "preserves every field, including one omitted from Contract and a field named tag" $
    property $ \text n (m :: Maybe (Maybe Bool)) ->
      roundTrip (Packet (T.pack text) n (fmap (fmap (const ())) m))
  it "preserves nested optional values and positional wrappers" $
    property $ \(x :: Maybe (Maybe Int)) -> roundTrip (Wrapped x)
  it "preserves sums with nullary and positional constructors" $
    property $ \text (n :: Maybe Int) -> conjoin (map roundTrip [Yes,No,Revise (T.pack text) n])
  it "preserves lists, tuples, either and arbitrary precision integers" $
    property $ \(xs :: [Int]) (n :: Integer) (choice :: Either Bool (Maybe Int)) -> roundTrip (xs,n,choice)
  it "preserves both Int bounds and rejects overflow" $ do
    (persistence @Int).decode ((persistence @Int).encode minBound) `shouldBe` Right minBound
    (persistence @Int).decode ((persistence @Int).encode maxBound) `shouldBe` Right maxBound
    (persistence @Int).decode (Integer (toInteger (maxBound :: Int)+1)) `shouldSatisfy` isLeft
  it "rejects changed field sets and constructors without defaulting" $ do
    let c = persistence @Packet
        payload fs = Object [("tag",String "Packet"),("fields",Object fs)]
    c.decode (payload [("tag",String "x")]) `shouldSatisfy` isLeft
    case c.encode (Packet "x" 3 Nothing) of
      Object [("tag",t),("fields",Object fs)] -> do
        c.decode (payload (("extra",Null):fs)) `shouldSatisfy` isLeft
        c.decode (payload (("hidden",Integer 9):fs)) `shouldSatisfy` isLeft
        c.decode (Object [("tag",String "Renamed"),("fields",Object fs)]) `shouldSatisfy` isLeft
        c.decode (Object [("tag",t),("fields",Object fs),("extra",Null)]) `shouldSatisfy` isLeft
      _ -> expectationFailure "unexpected wire form"

  it "rejects recursive schemas by name before yielding a codec" $ do
    evaluate (persistence @Recursive) `shouldThrow` (\(RecursivePersistence name) -> "Recursive" `T.isInfixOf` name)
    evaluate (persistence @MutualA) `shouldThrow` (\(RecursivePersistence name) -> "MutualA" `T.isInfixOf` name)
  it "allows repeated types on independent paths" $
    roundTrip (Packet "x" 1 Nothing, Packet "y" 2 (Just Nothing))
  it "rejects recursion when inferred flow steps are constructed" $ do
    let recursive (RecursivePersistence name) = "Recursive" `T.isInfixOf` name
    evaluate (await @Recursive :: Agentic IO () Recursive) `shouldThrow` recursive
    evaluate (durably (\_ _ -> pure ()) :: Agentic IO Recursive ()) `shouldThrow` recursive
  it "does not round large integers through floating point" $
    roundTrip (10^(100 :: Int) + 1 :: Integer)
  it "rejects recursive steps through describe, preflight and execution before workflow work" $ do
    calls <- newIORef (0 :: Int)
    let touched = modifyIORef' calls (+1)
        recursive (RecursivePersistence name) = "Recursive" `T.isInfixOf` name
        prefix = act (\() -> touched) >>> draft @Bool "never called" >>> arr (const ())
        waitFlow = prefix >>> await @Recursive :: Agentic IO () Recursive
        effectFlow = prefix >>> durably (\_ () -> touched >> pure (Recur Nothing))
        journal = Journal (JournalIdentity "test" "v1") (pure ())
          (\_ -> pure Nothing) (\_ _ -> touched)
        rt = runtime{journal=Just journal,systemTwo=SystemTwo (\_ -> touched >> fail "model called")}
    evaluate (describe waitFlow) `shouldThrow` recursive
    evaluate (preflight waitFlow) `shouldThrow` recursive
    resumable rt waitFlow () `shouldThrow` recursive
    evaluate (describe effectFlow) `shouldThrow` recursive
    evaluate (preflight effectFlow) `shouldThrow` recursive
    resumable rt effectFlow () `shouldThrow` recursive
    readIORef calls `shouldReturn` 0
