module Main (main) where

import Agentic
import Agentic.Scripted (modelContract)
import Agentic.Runtime (Conversation (..))
import Agentic.Schema (Schema(..), Field(..), Shape (..))
import Agentic.JsonSchema (objectSchema)
import Agentic.Aeson (toAeson)
import Agentic.OpenAI
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromJust)
import Data.Text (Text)
import GHC.Generics (Generic)
import Test.Hspec hiding (describe)
import qualified Test.Hspec

data Square = Blank | X | O
  deriving (Generic, Show, Contract)

data Row = Row {left :: Square, centre :: Square, right :: Square}
  deriving (Generic, Show, Contract)

data Board = Board {top :: Row, middle :: Row, bottom :: Row}
  deriving (Generic, Show, Contract)

data Box a = Box { value :: a } deriving (Generic, Contract)
data Repeated = Repeated { one :: Box Int, two :: Box Int } deriving (Generic, Contract)

conversation :: Conversation
conversation =
  Conversation
    { path = []
    , instruction = "Suggest 3 dinosaurs"
    , input = Null
    , inputSchema = schemaOf SNull
    , tools = [ToolSpec "search" "Search the fossil database" ((modelContract @Text).schema)]
    , outputSchema = (modelContract @[Text]).schema
    , history = []
    }

at :: J.Key -> J.Value -> J.Value
at k = \case
  J.Object o -> fromJust (KeyMap.lookup k o)
  other -> error ("not an object: " <> show other)

body :: Conversation -> J.Value
body = toAeson . requestBody openai

decoded :: J.Value -> Either String Turn
decoded = either (Left . show) Right . decodeTurn conversation

main :: IO ()
main = hspec $ Test.Hspec.describe "Agentic.OpenAI" $ do
  it "asks for strict structured output, wrapped when it isn't an object" $
    at "format" (at "text" (body conversation))
      `shouldBe` fromJust
        ( J.decode
            "{\"type\":\"json_schema\",\"name\":\"output\",\"strict\":true,\"schema\":{\"type\":\"object\",\
            \\"properties\":{\"value\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}},\
            \\"required\":[\"value\"],\"additionalProperties\":false}}"
        )

  it "names the schema after its type, and shares repeated types through $defs" $ do
    let format = at "format" (at "text" (body conversation {outputSchema = (modelContract @Board).schema}))
        schema = at "schema" format
    at "name" format `shouldBe` J.String "Board"
    at "top" (at "properties" schema) `shouldBe` fromJust (J.decode "{\"$ref\":\"#/$defs/Row\"}")
    KeyMap.keys (case at "$defs" schema of J.Object o -> o; _ -> mempty) `shouldMatchList` ["Row", "Square"]

  it "uses safe shared-definition keys for applied type names" $ do
    let schema = toAeson (objectSchema (modelContract @Repeated).schema)
    at "$ref" (at "one" (at "properties" schema)) `shouldBe` J.String "#/$defs/Box_Int"
    at "$ref" (at "two" (at "properties" schema)) `shouldBe` J.String "#/$defs/Box_Int"
    at "type" (at "Box_Int" (at "$defs" schema)) `shouldBe` J.String "object"

  it "keeps colliding sanitized names and pointer punctuation distinct" $ do
    let a = Schema (Just "Box Int") Nothing [] SInteger
        b = Schema (Just "Box_Int") Nothing [] (SString Nothing)
        c = Schema (Just "A/B~C") Nothing [] SBool
        root = schemaOf (SObject [Field n ty True | (n, ty) <- [("a1",a),("a2",a),("b1",b),("b2",b),("c1",c),("c2",c)]])
        schema = toAeson (objectSchema root)
        defs = at "$defs" schema
        props = at "properties" schema
    at "$ref" (at "a1" props) `shouldBe` J.String "#/$defs/Box_Int"
    at "$ref" (at "b1" props) `shouldBe` J.String "#/$defs/Box_Int_2"
    at "$ref" (at "c1" props) `shouldBe` J.String "#/$defs/A_B_C"
    at "type" (at "Box_Int" defs) `shouldBe` J.String "integer"
    at "type" (at "Box_Int_2" defs) `shouldBe` J.String "string"
    at "type" (at "A_B_C" defs) `shouldBe` J.String "boolean"

  it "stores nothing, and asks for reasoning it can send back" $ do
    at "store" (body conversation) `shouldBe` J.Bool False
    at "include" (body conversation) `shouldBe` J.toJSON ["reasoning.encrypted_content" :: Text]

  it "sends tools as strict functions" $
    at "tools" (body conversation)
      `shouldBe` fromJust
        ( J.decode
            "[{\"type\":\"function\",\"name\":\"search\",\"description\":\"Search the fossil database\",\"strict\":true,\
            \\"parameters\":{\"type\":\"object\",\"properties\":{\"value\":{\"type\":\"string\"}},\
            \\"required\":[\"value\"],\"additionalProperties\":false}}]"
        )

  it "sends earlier output items back as input, followed by tool outputs" $ do
    let reasoning = Object [("type", String "reasoning"), ("encrypted_content", String "abc")]
        call = Object [("type", String "function_call"), ("call_id", String "c1"), ("name", String "search"), ("arguments", String "{\"value\":\"rex\"}")]
        c = conversation {history = [Called (Raw (Array [reasoning, call])) [("c1", ToolOk (String "T. rex"))]]}
    at "input" (body c)
      `shouldBe` fromJust
        ( J.decode
            "[{\"role\":\"user\",\"content\":\"Suggest 3 dinosaurs\"},\
            \{\"type\":\"reasoning\",\"encrypted_content\":\"abc\"},\
            \{\"type\":\"function_call\",\"call_id\":\"c1\",\"name\":\"search\",\"arguments\":\"{\\\"value\\\":\\\"rex\\\"}\"},\
            \{\"type\":\"function_call_output\",\"call_id\":\"c1\",\"output\":\"T. rex\"}]"
        )

  it "reads function calls, parsing and unwrapping their arguments" $
    fmap (.action) (decoded (fromJust (J.decode "{\"status\":\"completed\",\"output\":[{\"type\":\"reasoning\"},{\"type\":\"function_call\",\"call_id\":\"c1\",\"name\":\"search\",\"arguments\":\"{\\\"value\\\":\\\"rex\\\"}\"}]}")))
      `shouldBe` Right (CallTools [ToolCall "c1" "search" (String "rex")])

  it "reads a final answer from the message text" $
    fmap (.action) (decoded (fromJust (J.decode "{\"status\":\"completed\",\"output\":[{\"type\":\"message\",\"content\":[{\"type\":\"output_text\",\"text\":\"{\\\"value\\\":[\\\"Stegosaurus\\\"]}\"}]}]}")))
      `shouldBe` Right (Respond (Array [String "Stegosaurus"]))

  it "reports refusals and truncation as errors" $ do
    let refusal = decodeTurn conversation (fromJust (J.decode "{\"status\":\"completed\",\"output\":[{\"type\":\"message\",\"content\":[{\"type\":\"refusal\",\"refusal\":\"No.\"}]}]}"))
        truncated = decodeTurn conversation (fromJust (J.decode "{\"status\":\"incomplete\",\"incomplete_details\":{\"reason\":\"max_output_tokens\"},\"output\":[]}"))
    either (\case Refused "No." -> True; _ -> False) (const False) refusal `shouldBe` True
    either (\case Truncated -> True; _ -> False) (const False) truncated `shouldBe` True
