module Jokes where

import           Agentic
import           Autodocodec            (Autodocodec (Autodocodec),
                                         HasCodec (codec), object,
                                         requiredField, (.=))
import           Control.Arrow
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Text
import           Dhall                  (FromDhall, ToDhall)
import           GHC.Generics           (Generic)

data Joke = Joke { genre :: Text, setup :: Text, punchline :: Text }
    deriving stock (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via (Autodocodec Joke)

instance HasCodec Joke where
    codec =
        object "The structure of a Joke" $
            Joke <$> requiredField "genre" "The category of joke" .= (.genre)
                 <*> requiredField "setup" "The setup of the joke" .= (.setup)
                 <*> requiredField "punchline" "The punchline" .= (.punchline)

newtype Dog = Dog { name :: String }
    deriving stock (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via (Autodocodec Dog)

instance HasCodec Dog where
    codec =
        object "The structure of a Dog" $
            Dog <$> requiredField "name" "The name of the dog" .= (.name)

data BetterJoke
    = DadJoke { setup :: Text, punchline :: Text }
    | OneLiner { line :: Text }
    | Story { paragraphs :: [Text] }
    deriving (Generic, Show, FromDhall, ToDhall)

generateJoke :: Arrow a => a () Text
generateJoke = arr $ const "Give me some dad jokes"

showJoke :: Arrow a => a Joke Text
showJoke = arr $ \joke -> joke.setup <> "\n\n" <> joke.punchline

agent :: MonadIO m => Kleisli m () Text
agent = generateJoke >...> showJoke


