module Jokes where

import           Agentic
import           Autodocodec            (Autodocodec (Autodocodec),
                                         HasCodec (codec), object,
                                         requiredField, (.=))
import           Control.Arrow
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Text
import           GHC.Generics           (Generic)

data Joke = Joke { genre :: Text, setup :: Text, punchline :: Text }
            deriving stock (Show, Eq, Generic)
            deriving (FromJSON, ToJSON) via (Autodocodec Joke)

instance HasCodec Joke where
  codec =
    object "The structure of a Joke" $
      Joke <$> requiredField "genre" "The category of joke" .= genre
           <*> requiredField "setup" "The setup of the joke" .= setup
           <*> requiredField "punchline" "The punchline" .= punchline

generateJoke :: Arrow a => a () Text
generateJoke = arr $ const "Give me some dad jokes"

showJoke :: Arrow a => a Joke Text
showJoke = arr $ \joke -> setup joke <> "\n\n" <> punchline joke

agent :: MonadIO m => Kleisli m () Text
agent = generateJoke >...> showJoke
