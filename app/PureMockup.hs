module PureMockup where

import           Autodocodec          (Autodocodec (Autodocodec),
                                       HasCodec (codec), object, requiredField,
                                       (.=))
import           Autodocodec.Schema   (jsonSchemaViaCodec)
import           Control.Arrow        (Arrow, arr, (>>>))
import           Data.Aeson           (FromJSON, ToJSON, eitherDecode, encode)
import           Data.Bifunctor       (first)
import qualified Data.ByteString.Lazy as LBS
import           Data.Text
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import           GHC.Generics         (Generic)

data Joke = Joke { genre :: Text, setup :: Text, punchline :: Text }
            deriving stock (Show, Eq, Generic)
            deriving (FromJSON, ToJSON) via (Autodocodec Joke)

instance HasCodec Joke where
  codec =
    object "The structure of a Joke" $
      Joke <$> requiredField "genre" "The category of joke" .= (.genre)
           <*> requiredField "setup" "The setup of the joke" .= (.setup)
           <*> requiredField "punchline" "The punchline" .= (.punchline)

schema :: forall a. HasCodec a => Text
schema = decodeUtf8 $ LBS.toStrict $ encode $ jsonSchemaViaCodec @a

parsed :: forall b a. Arrow a => FromJSON b => a Text (Either Text b)
parsed = arr $ first pack . eitherDecode . LBS.fromStrict . encodeUtf8

orFail :: Arrow a => a (Either Text c) c
orFail = arr $ either (error . unpack) id

-- Glue two arrows together, injecting the schema of the type required as input to second arrow
-- to the prompt given by the first (including parsing or failing of that type)
(>...>) :: forall a b c ar. (Arrow ar, FromJSON b, HasCodec b) => ar a Text -> ar b c -> ar a c
(>...>) l r = l >>> injectSchema @b >>> runLLM >>> parsed @b >>> orFail >>> r

runLLM :: Arrow a => a Text Text
runLLM = arr $ \_prompt ->
    -- Run the prompt here... mockup for now to retain clarity (free from IO)
    "{\"genre\":\"dad joke\",\"setup\":\"When does a joke become a dad joke?\",\"punchline\":\"When the punchline becomes apparent\"}"

injectSchema :: forall s a. HasCodec s => Arrow a => a Text Text
injectSchema = arr (\prompt -> prompt <> "\n\nRespond with this schema:\n" <> schema @s)

generateJoke :: Arrow a => a () Text
generateJoke = arr $ const "Give me a dad joke"

showJoke :: Arrow a => a Joke Text
showJoke = arr $ \joke -> joke.setup <> "\n\n" <> joke.punchline

agent :: Arrow a => a () Text
agent = generateJoke >...> showJoke

