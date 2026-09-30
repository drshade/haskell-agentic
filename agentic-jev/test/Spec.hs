module Main (main) where

import Agentic
import Agentic.Aeson (toAeson)
import Agentic.Jev
import qualified Data.Aeson as J
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Test.Hspec hiding (describe)
import qualified Test.Hspec

data Groan = Mild | Solid | Unbearable
  deriving (Generic, Show, Eq)

instance Options Groan where
  options = described "" [option Mild "A polite smile", option Solid "An audible groan", option Unbearable "People leave"]

request :: JudgeRequest
request =
  JudgeRequest
    (Object [("joke", String "Why was the scarecrow promoted?")])
    (specs ((,,) <$> yesNo "Is it funny?" <*> choice @Groan "Which reaction?" <*> score @Groan "How much groaning?"))


main :: IO ()
main = hspec $ Test.Hspec.describe "Agentic.Jev" $ do
  it "builds Jev's request body" $
    toAeson (requestBody "jev-latest" request)
      `shouldBe` fromJust
        ( J.decode
            "{\"model\":\"jev-latest\",\
            \\"state\":{\"joke\":\"Why was the scarecrow promoted?\"},\
            \\"questions\":{\
            \\"q0\":{\"type\":\"noul\",\"instructions\":\"Is it funny?\"},\
            \\"q1\":{\"type\":\"choice\",\"instructions\":\"Which reaction?\",\
            \\"criteria\":{\"Mild\":\"A polite smile\",\"Solid\":\"An audible groan\",\"Unbearable\":\"People leave\"}},\
            \\"q2\":{\"type\":\"score\",\"instructions\":\"How much groaning?\",\
            \\"criteria\":[\"A polite smile\",\"An audible groan\",\"People leave\"]}}}"
        )

  it "decodes Jev's answers in question order" $
    decodeResponse
      request
      ( fromJust
          ( J.decode
              "{\"model\":\"jev-latest\",\"answers\":{\
              \\"q2\":{\"type\":\"score\",\"score\":1.25,\"legend\":{\"0\":\"A polite smile\",\"1\":\"An audible groan\",\"2\":\"People leave\"},\
              \\"probabilities\":{\"0\":0.1,\"1\":0.55,\"2\":0.35},\"confidence\":0.4},\
              \\"q0\":{\"type\":\"noul\",\"noul\":0.8132},\
              \\"q1\":{\"type\":\"choice\",\"choice\":\"Solid\",\"probabilities\":{\"Mild\":0.2,\"Solid\":0.7,\"Unbearable\":0.1},\"confidence\":0.5}},\
              \\"usage\":{\"input_tokens\":120,\"output_tokens\":3}}"
          )
      )
      `shouldBe` Right
        [ YesNoAnswer 0.8132
        , ChoiceAnswer "Solid" [("Mild", 0.2), ("Solid", 0.7), ("Unbearable", 0.1)] 0.5
        , ScoreAnswer 1.25 [(0, 0.1), (1, 0.55), (2, 0.35)] 0.4
        ]

  it "reports a response it can't read" $
    decodeResponse request (J.object []) `shouldSatisfy` either (const True) (const False)
