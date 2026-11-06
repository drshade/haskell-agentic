{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Agentic hiding (model)
import Agentic.Codec qualified as C
import Agentic.Contract (Case (..), Fields (..), Structure (..))
import Agentic.Schema (Schema (..), Shape (..), Variant (..))
import Agentic.Schema qualified as Schema
import Agentic.Scripted (modelContract)
import Control.Exception (evaluate, try)
import Data.Either (isLeft)
import Data.Fixed (E2, Fixed (..))
import Data.IORef
import Data.Text (Text)
import GHC.Generics (Generic)
import ProjectionExample
import Test.Hspec hiding (describe)
import Test.Hspec qualified as H
import Type.Reflection

newtype Hidden = Hidden Text deriving (Eq, Show)
instance Contract Hidden where
  contract = scalar (ScalarInfo "example.Hidden" []) Nothing

data VariantChoice = Safe Int | Unsafe Hidden deriving (Generic, Contract)
newtype Wrapper = Wrapper (Maybe Int)
  deriving stock (Generic)
  deriving anyclass (Contract)
data EmptyRoot = EmptyRoot deriving (Generic, Contract, Eq, Show)

newtype Age = Age Int
  deriving stock (Generic, Eq, Show)
  deriving anyclass (Contract)
type Alias = Age

good :: Either ProjectionError a -> a
good = either (error . show) id
roundTrip :: (Eq a, Show a) => Codec a -> a -> Expectation
roundTrip c x = c.decode (c.encode x) `shouldBe` Right x

main :: IO ()
main = hspec $ do
  H.describe "independent projections" $ do
    it "compiles one derived Sample to storage and model codecs" $ do
      let stored = good (storage (contract @Sample))
          model = modelContract @Sample
      stored.encode sample `shouldBe` Object [("key", String "sample-1"), ("amount", String "12.34"), ("confidence", String "8500")]
      model.encode sample `shouldBe` Object [("key", String "sample-1"), ("amount", String "12.34"), ("confidence", Number 0.85)]
      roundTrip stored sample
      roundTrip model sample
    it "preserves negative and huge exact decimals" $ do
      let c = good (storage (contract @(Fixed E2)))
      mapM_ (roundTrip c . MkFixed) [-1234, 123456789012345678901234567890123456789, 0]
      c.encode (MkFixed (-1234)) `shouldBe` String "-12.34"
    it "rejects malformed decimal and identifier values" $ do
      mapM_
        (\v -> decimalCodec.decode v `shouldSatisfy` isLeft)
        [Number 12.34, String "12.345", String "01.00", String "NaN", String "-0.00", String "1e2"]
      identifierCodec.decode (String "") `shouldSatisfy` isLeft
    it "validates storage probability before clamping, independently of model behavior" $ do
      let model = modelContract @Probability
      probabilityStorage.decode (String "8500") `shouldBe` Right (fromBasisPoints 8500)
      mapM_
        (\v -> probabilityStorage.decode v `shouldSatisfy` isLeft)
        [String "10001", String "-1", String "85.5", String "08500", Number 8500]
      model.decode (Number 1.2) `shouldBe` Right 1
      model.decode (Number 1e308) `shouldBe` Right 1
      model.decode (Number (0 / 0)) `shouldSatisfy` isLeft
      model.decode (Number (1 / 0)) `shouldSatisfy` isLeft
    it "shares contract checks across projections" $ do
      let c = good (storage (checked "positive" (> (0 :: Integer)) contract))
      c.decode (String "-1") `shouldBe` Left "must be positive"
    it "composes through lists, optionals, and payload sums" $ do
      roundTrip
        (good (storage (contract @[Maybe Payload])))
        [Nothing, Just Empty, Just (Amount (MkFixed (-1234))), Just (Confidence 0.85)]
    it "retains all nested optional distinctions" $ do
      let c = good (storage (contract @(Maybe (Maybe Int))))
          values = [Nothing, Just Nothing, Just (Just 7)]
      mapM_ (roundTrip c) values
      map c.encode values
        `shouldBe` [ Object [("tag", String "None")]
                   , Object [("tag", String "Some"), ("value", Object [("tag", String "None")])]
                   , Object [("tag", String "Some"), ("value", Object [("tag", String "Some"), ("value", String "7")])]
                   ]
    it "rejects lossy model optionals even through nominal and documentation wrappers" $ do
      isLeft (compileModel (contract @(Maybe (Maybe Int)))) `shouldBe` True
      isLeft (compileModel (Optional (documented "wrapper" (contract @Wrapper)))) `shouldBe` True
      isLeft (compileModel (contract @(Maybe ()))) `shouldBe` True
      isLeft (compileModel (Optional (checked "anything" (const True) (mapStructure id id (documented "nullable" (contract @(Maybe Int))))))) `shouldBe` True
    it "lets storage choose unit as an empty record" $
      (good (storage (contract @()))).encode () `shouldBe` Object []
  H.describe "structural overrides" $ do
    it "stores positional wrappers with tags while models remain transparent" $ do
      let stored = good (storage (contract @Age))
          model = modelContract @Age
      stored.encode (Age 42) `shouldBe` Object [("tag", String "Age"), ("_1", String "42")]
      model.encode (Age 42) `shouldBe` Integer 42
      stored.decode (Object [("tag", String "Other"), ("_1", String "42")]) `shouldSatisfy` isLeft
    it "projects a singleton nullary root to an empty object" $ do
      let stored = good (storage (contract @EmptyRoot))
      stored.encode EmptyRoot `shouldBe` Object []
      roundTrip stored EmptyRoot
      (modelContract @EmptyRoot).encode EmptyRoot `shouldBe` String "EmptyRoot"
    it "moves sum payloads under a payload object independently of the model layout" $ do
      let stored = good (storage (contract @Payload))
          model = modelContract @Payload
      stored.encode (Amount (MkFixed 1234)) `shouldBe` Object [("tag", String "Amount"), ("payload", Object [("_1", String "12.34")])]
      model.encode (Amount (MkFixed 1234)) `shouldBe` Object [("tag", String "Amount"), ("_1", String "12.34")]
      roundTrip stored (Amount (MkFixed 1234))
    it "applies field documentation before a projection changes the sum layout" $ do
      let stored = good (storage (field "_1" "quantity" (contract @Payload)))
      roundTrip stored (Amount (MkFixed 1234))
      case stored.schema.shape of
        SSum (_ : Variant "Amount" _ [Schema.Field "payload" (Schema _ _ _ (SObject [Schema.Field "_1" amount _])) _] : _) ->
          amount.doc `shouldBe` Just "quantity"
        other -> expectationFailure (show other)
    it "rejects a missing field as soon as the annotation is evaluated" $
      evaluate (field "missing" "quantity" (contract @Sample))
        `shouldThrow` errorCall "Agentic.Contract.field: no field named missing"
    it "applies repeated field annotations immediately, with the last one winning" $
      case outline (field "key" "second" (field "key" "first" (contract @Sample))) of
        ONamed _ (ORecord _ (keyField : _)) -> do
          keyField.name `shouldBe` "key"
          keyField.doc `shouldBe` Just "second"
          keyField.structure `shouldBe` outline (contract @FactId)
        other -> expectationFailure (show other)
    it "compiles existential cases without exposing nonmatching payload getters" $ do
      let c :: Case (Either Int Text)
          c =
            Case
              { constructor = Constructor "Left" PositionalFields
              , doc = Nothing
              , match = \case Left n -> Just n; Right _ -> Nothing
              , inject = Left
              , fields = Field "_1" Nothing (contract @Int) id
              }
          compiled :: C.CaseCodec (Either Int Text)
          compiled = good (compileCaseWith compileModel c)
      compiled.encode (Right "not this case") `shouldBe` Nothing
      compiled.encode (Left 3) `shouldBe` Just [("_1", Integer 3)]
      compiled.decode [("_1", Integer 3)] `shouldBe` Right (Left 3)
    it "accepts whole in-range model Numbers for Int and rejects overflow" $ do
      let c = modelContract @Int
      c.decode (Number 3.0) `shouldBe` Right 3
      c.decode (Number 3.5) `shouldSatisfy` isLeft
      c.decode (Integer (toInteger (maxBound :: Int) + 1)) `shouldSatisfy` isLeft
  H.describe "discovery" $ do
    it "omits representation mappings from outlines" $ do
      let original = contract @Sample
          mapped = mapStructure id id (mapStructure id id original)
      outline mapped `shouldBe` outline original
      outline (nominal original) `shouldBe` ONamed (SomeTypeRep (typeRep @Sample)) (outline original)
    it "retains applied identity and scalar parameters" $ do
      outline (contract @(Box (Fixed E2))) `shouldNotBe` outline (contract @(Box Probability))
      leaves (outline (contract @Sample))
        `shouldBe` [ ScalarInfo "example.FactId" []
                   , ScalarInfo "Data.Fixed.Fixed" [("resolution", "100")]
                   , ScalarInfo "Agentic.Questions.Probability" []
                   ]
    it "preserves nominal wrappers and aliases without flattening them to scalars" $ do
      outline (contract @Alias) `shouldBe` outline (contract @Age)
      case outline (contract @Age) of
        ONamed rep tree
          | ORecord (Constructor "Age" PositionalFields) [OutlineField "_1" _ _] <- tree ->
              rep `shouldBe` SomeTypeRep (typeRep @Age)
        other -> expectationFailure (show other)
    it "describes a singleton nullary type as a product" $
      case outline (contract @EmptyRoot) of
        ONamed _ tree -> tree `shouldBe` ORecord (Constructor "EmptyRoot" PositionalFields) []
        other -> expectationFailure (show other)
    it "keeps constructor declaration order and positional payload metadata" $ do
      case outline (contract @Payload) of
        ONamed _ tree
          | OSum cs <- tree ->
              [con | OutlineCase con _ _ <- cs]
                `shouldBe` [Constructor "Empty" PositionalFields, Constructor "Amount" PositionalFields, Constructor "Confidence" PositionalFields]
        other -> expectationFailure (show other)
  H.describe "preflight" $ do
    it "checks empty-list elements, absent optionals, and unused sum arms" $ do
      isLeft (compileModel (contract @[Hidden])) `shouldBe` True
      isLeft (compileModel (contract @(Maybe Hidden))) `shouldBe` True
      isLeft (compileModel (contract @VariantChoice)) `shouldBe` True
    it "describes a flow even when its model projection is unsupported" $
      renderTree (describe (draft @Hidden @() @IO "hidden")) `shouldSatisfy` (not . null . show)
    it "rejects unsupported tool results before any effect or provider call" $ do
      effects <- newIORef (0 :: Int)
      calls <- newIORef (0 :: Int)
      let rt = runtime{systemTwo = SystemTwo (\_ -> modifyIORef' calls (+ 1) >> pure (Turn (Raw Null) (Respond (String "done"))))}
          hiddenTool = tool @() @Hidden "hidden" "hidden output" (arr (const (Hidden "secret")))
          flow = act (\() -> modifyIORef' effects (+ 1)) >>> draftWith @Text [hiddenTool] "test"
      result <- try @FlowError (interpret rt flow ())
      result `shouldSatisfy` (\case Left (UnsupportedContract _ _) -> True; _ -> False)
      readIORef calls `shouldReturn` 0
      readIORef effects `shouldReturn` 0
    it "retains named flow context and structured projection paths" $
      case preflight (draft @Hidden @() @IO "hidden" `named` "make hidden") of
        Left (UnsupportedContract notes (ProjectionError path _)) -> do
          notes `shouldBe` [Note "make hidden" Nothing]
          path `shouldBe` ["draft.output"]
        other -> expectationFailure (show other)
    it "rejects unsupported tool inputs and nested tool body contracts" $ do
      let inputTool = tool @Hidden @Text "input" "" (arr (const "ok"))
          bodyTool = tool @() @Text "body" "" (draft @Hidden "hidden" >>> arr (const "ok"))
      preflight (draftWith @Text @() @IO [inputTool] "test") `shouldSatisfy` isLeft
      preflight (draftWith @Text @() @IO [bodyTool] "test") `shouldSatisfy` isLeft

leaves :: Outline -> [ScalarInfo]
leaves = \case
  OScalar _ info -> [info]
  ONamed _ c -> leaves c
  ORecord _ fs -> concat [leaves c | OutlineField _ _ c <- fs]
  OSum cs -> concat [leaves c | OutlineCase _ _ fs <- cs, OutlineField _ _ c <- fs]
  OList c -> leaves c
  OOptional c -> leaves c
  OUnit -> []
  OChecked _ c -> leaves c
  ODocumented _ c -> leaves c
