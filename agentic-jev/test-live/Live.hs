module Main (main) where

import Agentic
import Agentic.IO.DotEnv (loadDotEnv)
import Agentic.Jev (jev)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import Test.Hspec hiding (describe)
import qualified Test.Hspec

data Joke = Joke {setup :: Text, punchline :: Text}
  deriving (Generic, Show, Contract)

data Groan = Mild | Solid | Unbearable
  deriving (Generic, Show, Eq)

instance Options Groan where
  options =
    described
      "How much the audience groans"
      [ option Mild "A polite smile; most people didn't notice"
      , option Solid "An audible groan from most of the room"
      , option Unbearable "People get up and leave"
      ]

joke :: Joke
joke = Joke "Why was the scarecrow promoted?" "He was outstanding in his field."

main :: IO ()
main = do
  _ <- loadDotEnv
  token <- lookupEnv "JEV_TOKEN"
  hspec $ Test.Hspec.describe "Jev, live" $ case token of
    Nothing -> it "needs JEV_TOKEN" (pendingWith "set JEV_TOKEN in .env to run the live tests")
    Just _ -> do
      it "answers a yes/no, a choice and a score in one request" $ do
        rt <- pure runtime >>= withSystemOne jev
        (funny, reaction, groan) <-
          interpret rt
            ( judge $
                (,,)
                  <$> yesNo "Would a 10-year-old laugh at this joke?"
                  <*> choice @Groan "How will the audience react?"
                  <*> score @Groan "How much will the audience groan?"
            )
            joke
        probability (yes funny) `shouldSatisfy` (\p -> p >= 0 && p <= 1)
        map fst (choiceProbabilities reaction) `shouldBe` [Mild, Solid, Unbearable]
        position groan `shouldSatisfy` (\p -> p >= 0 && p <= 2)

      it "filters with keep" $ do
        rt <- pure runtime >>= withSystemOne jev
        -- Plain text, not Joke: a record with "setup" and "punchline" fields
        -- tells Jev it's a joke before it reads a word.
        let texts = ["Why was the scarecrow promoted? He was outstanding in his field.", "The meeting is at 3pm in room 4." :: Text]
        kept <- interpret rt (keep 0.5 (yesNo "Is this text a joke?")) texts
        kept `shouldBe` take 1 texts
