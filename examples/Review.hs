-- | Jev judges a joke, for real. Needs JEV_TOKEN, in the environment or .env.
module Main (main) where

import Agentic
import Agentic.IO.DotEnv (loadDotEnv)
import Agentic.Jev (jev)
import Data.Text (Text)
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

data Joke = Joke {setup :: Text, punchline :: Text}
  deriving (Generic, Show, Contract)

data Groan = Mild | Solid | Unbearable
  deriving (Generic, Show, Eq)

instance Options Groan where
  options =
    documentedOptions
      "How much the audience groans"
      [ option Mild "A polite smile; most people didn't notice"
      , option Solid "An audible groan from most of the room"
      , option Unbearable "People get up and leave"
      ]

data Review = Review {funny :: YesNo, groan :: Score Groan}
  deriving (Show)

review :: Agentic IO Joke Review
review =
  judge $
    Review
      <$> yesNo "Would a 10-year-old laugh at this joke?"
      <*> score "How much will the audience groan?"

main :: IO ()
main = do
  _ <- loadDotEnv
  print $ describe review
  T.putStrLn $ "\n" <> mermaid (describe review)
  T.putStrLn $ dot (describe review)
  rt <- pure runtime >>= withSystemOne jev
  result <- interpret rt review (Joke "Why was the scarecrow promoted?" "He was outstanding in his field.")
  print result
