module Main (main) where

import Agentic
import Agentic.Runtime (Conversation (..))
import Agentic.Schema (Shape (..))
import Agentic.Aeson (toAeson)
import Agentic.Anthropic
import qualified Data.Text as T
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromJust)
import Data.Text (Text)
import Test.Hspec hiding (describe)
import qualified Test.Hspec

conversation :: Conversation
conversation =
  Conversation
    { path = []
    , instruction = "Suggest 3 dinosaurs"
    , input = Null
    , inputSchema = schemaOf SNull
    , tools = [ToolSpec "search" "Search the fossil database" ((contract @Text).schema)]
    , outputSchema = (contract @[Text]).schema
    , history = []
    }

at :: J.Key -> J.Value -> J.Value
at k = \case
  J.Object o -> fromJust (KeyMap.lookup k o)
  other -> error ("not an object: " <> show other)

decoded :: J.Value -> Either String Turn
decoded = either (Left . show) Right . decodeTurn conversation

json :: J.Value -> J.Value
json = id

main :: IO ()
main = hspec $ Test.Hspec.describe "Agentic.Anthropic" $ do
  it "wraps a non-object output in a value field for structured outputs" $
    at "format" (at "output_config" (toAeson (requestBody anthropic conversation)))
      `shouldBe` fromJust
        ( J.decode
            "{\"type\":\"json_schema\",\"schema\":{\"type\":\"object\",\
            \\"properties\":{\"value\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}},\
            \\"required\":[\"value\"],\"additionalProperties\":false}}"
        )

  it "keeps a record's field order in its schema" $ do
    let body = renderJson (requestBody anthropic conversation {outputSchema = (contract @(Text, Text, Text)).schema})
        at' k = T.length (fst (T.breakOn k body))
    (at' "\"_1\"" < at' "\"_2\"", at' "\"_2\"" < at' "\"_3\"") `shouldBe` (True, True)

  it "sends tools as strict, with wrapped inputs" $
    at "tools" (toAeson (requestBody anthropic conversation))
      `shouldBe` fromJust
        ( J.decode
            "[{\"name\":\"search\",\"description\":\"Search the fossil database\",\"strict\":true,\
            \\"input_schema\":{\"type\":\"object\",\"properties\":{\"value\":{\"type\":\"string\"}},\
            \\"required\":[\"value\"],\"additionalProperties\":false}}]"
        )

  it "sends earlier turns back unchanged, with tool results" $ do
    let raw = Object [("type", String "tool_use"), ("id", String "t1"), ("name", String "search"), ("input", Object [("value", String "rex")])]
        c = conversation {history = [Called (Raw (Array [raw])) [("t1", ToolOk (Array [String "T. rex"]))]]}
    at "messages" (toAeson (requestBody anthropic c))
      `shouldBe` fromJust
        ( J.decode
            "[{\"role\":\"user\",\"content\":\"Suggest 3 dinosaurs\"},\
            \{\"role\":\"assistant\",\"content\":[{\"type\":\"tool_use\",\"id\":\"t1\",\"name\":\"search\",\"input\":{\"value\":\"rex\"}}]},\
            \{\"role\":\"user\",\"content\":[{\"type\":\"tool_result\",\"tool_use_id\":\"t1\",\"content\":\"[\\\"T. rex\\\"]\"}]}]"
        )

  it "reads tool calls, unwrapping their input" $
    fmap (.action) (decoded (fromJust (J.decode "{\"stop_reason\":\"tool_use\",\"content\":[{\"type\":\"thinking\",\"thinking\":\"\",\"signature\":\"x\"},{\"type\":\"tool_use\",\"id\":\"t1\",\"name\":\"search\",\"input\":{\"value\":\"rex\"}}]}")))
      `shouldBe` Right (CallTools [ToolCall "t1" "search" (String "rex")])

  it "reads a final answer, unwrapping it" $
    fmap (.action) (decoded (fromJust (J.decode "{\"stop_reason\":\"end_turn\",\"content\":[{\"type\":\"text\",\"text\":\"{\\\"value\\\":[\\\"Stegosaurus\\\"]}\"}]}")))
      `shouldBe` Right (Respond (Array [String "Stegosaurus"]))

  it "keeps the whole content, thinking included, as the raw turn" $
    fmap (.raw) (decoded (fromJust (J.decode "{\"stop_reason\":\"end_turn\",\"content\":[{\"type\":\"thinking\",\"thinking\":\"\",\"signature\":\"x\"},{\"type\":\"text\",\"text\":\"{}\"}]}")))
      -- aeson orders object keys, which JSON ignores; the blocks themselves are unchanged.
      `shouldBe` Right (Raw (Array [Object [("signature", String "x"), ("thinking", String ""), ("type", String "thinking")], Object [("text", String "{}"), ("type", String "text")]]))

  it "reports refusals as errors, not answers" $
    either (\case Refused (Just "cyber") -> True; _ -> False) (const False)
      (decodeTurn conversation (fromJust (J.decode "{\"stop_reason\":\"refusal\",\"stop_details\":{\"type\":\"refusal\",\"category\":\"cyber\"},\"content\":[]}")))
      `shouldBe` True
  where
    _unused = json
