module Protocol.DhallSchema.MarshalSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Data.Text (Text, isInfixOf, unpack)
import Dhall (FromDhall, ToDhall)
import GHC.Generics (Generic)
import Protocol.DhallSchema.Marshal (parseDhall, Dhall)
import Protocol.Class (SchemaFormat(schemaOf))
import Data.Data (Proxy(..))

data SimpleRecord = SimpleRecord { name :: Text, age :: Int }
    deriving (Generic, Show, Eq, FromDhall, ToDhall)

data SumType = VariantA { x :: Int } | VariantB { y :: Text }
    deriving (Generic, Show, Eq, FromDhall, ToDhall)

tests :: TestTree
tests = testGroup "Protocol.DhallSchema.Marshal"
    [ testCase "schemaOf record contains field names" $ do
        let schema = schemaOf @Dhall (Proxy @SimpleRecord)
        assertBool ("schema contains 'name': " <> unpack schema) ("name" `isInfixOf` schema)
        assertBool ("schema contains 'age': " <> unpack schema) ("age" `isInfixOf` schema)

    , testCase "schemaOf sum type contains variant names" $ do
        let schema = schemaOf @Dhall (Proxy @SumType)
        assertBool ("schema contains 'VariantA': " <> unpack schema) ("VariantA" `isInfixOf` schema)
        assertBool ("schema contains 'VariantB': " <> unpack schema) ("VariantB" `isInfixOf` schema)

    , testCase "parseDhall round-trips a record" $ do
        -- Note: Dhall Integer literals require an explicit sign ('+30', not '30')
        let input = "{ name = \"Alice\", age = +30 }"
        result <- parseDhall @SimpleRecord input
        result @?= Right SimpleRecord { name = "Alice", age = 30 }

    , testCase "parseDhall returns Left on invalid input" $ do
        let input = "not valid dhall !!!"
        result <- parseDhall @SimpleRecord input
        case result of
            Left msg -> assertBool ("error should mention 'Dhall parse error', got: " <> unpack msg)
                                   ("Dhall parse error" `isInfixOf` msg)
            Right _  -> fail "Expected parse failure"

    , testCase "parseDhall round-trips a sum type" $ do
        -- Dhall union type encoding: use let to define the type alias
        let input = "let T = < VariantA : { x : Integer } | VariantB : { y : Text } > in T.VariantA { x = +5 }"
        result <- parseDhall @SumType input
        result @?= Right (VariantA { x = 5 })
    ]
