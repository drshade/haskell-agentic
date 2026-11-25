-- | Jev checks a joke before each revision; Claude revises it until it passes.
-- Needs JEV_TOKEN and ANTHROPIC_API_KEY, in the environment or .env.
module Revision where

import Agentic
import Agentic.Anthropic (anthropic)
import Agentic.IO.DotEnv (loadDotEnv)
import Agentic.Jev (jev)
import Control.Exception (throwIO)
import Data.Text (Text)
import Data.Text.IO qualified as T
import GHC.Generics (Generic)

data Joke = Joke {setup :: Text, punchline :: Text}
  deriving (Eq, Show, Generic, Contract)

revise :: Agentic IO Joke Joke
revise =
  repeatUntil
    ((judge (yesNo "Would a 10-year-old laugh at this joke?")
      >>> purely ((>= 0.8) . (.yes))) `named` "Funny enough?")
    (draft @Joke "Improve this joke for a 10-year-old." `named` "Revise joke")

main :: IO ()
main = do
  _ <- loadDotEnv
  description <- either throwIO pure (describe revise)
  T.putStrLn (mermaid description)
  rt <- pure runtime >>= withSystemOne jev >>= withSystemTwo anthropic
  result <- interpret rt revise
    (Joke "Why did the chicken cross the road?" "To get to the other side.")
  print result
