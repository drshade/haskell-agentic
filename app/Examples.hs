module Examples where

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

data MaritalStatus = Unmarried | Married | Widowed
    deriving (Generic, Show, FromDhall, ToDhall)

data ApplicationForm = ApplicationForm
    { name          :: Text
    , age           :: Int
    , maritalStatus :: MaritalStatus
    }
    deriving (Generic, Show, FromDhall, ToDhall)


