module Protocol.JSONSchema.MarshalSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import Data.Text (Text, isInfixOf, unpack)
import Autodocodec (HasCodec(..), object, requiredField', (.=))
import Protocol.Class (SchemaFormat(schemaOf, parseWithSchema))
import Protocol.JSONSchema.Marshal (Json)
import Data.Data (Proxy(..))
import GHC.Generics (Generic)

data JRecord = JRecord { jname :: Text, jage :: Int }
    deriving (Generic, Show, Eq)

instance HasCodec JRecord where
    codec = object "JRecord" $
        JRecord
          <$> requiredField' "jname" .= (.jname)
          <*> requiredField' "jage" .= (.jage)

tests :: TestTree
tests = testGroup "Protocol.JSONSchema.Marshal"
    [ testCase "schemaOf JSON record contains field names" $ do
        let schema = schemaOf @Json (Proxy @JRecord)
        assertBool ("schema contains 'jname': " <> unpack schema) ("jname" `isInfixOf` schema)
        assertBool ("schema contains 'jage': " <> unpack schema) ("jage" `isInfixOf` schema)

    , testCase "parseWithSchema JSON round-trips a record" $ do
        let json = "{\"jname\":\"Bob\",\"jage\":25}"
        result <- parseWithSchema @Json (Proxy @JRecord) json
        result @?= Right JRecord { jname = "Bob", jage = 25 }

    , testCase "parseWithSchema JSON returns Left on invalid JSON" $ do
        let json = "not valid json"
        result <- parseWithSchema @Json (Proxy @JRecord) json
        case result of
            Left _  -> pure ()
            Right _ -> fail "Expected parse failure"
    ]
