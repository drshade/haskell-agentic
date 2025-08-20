module Agentic where

import           Autodocodec            (HasCodec)
import           Autodocodec.Schema     (jsonSchemaViaCodec)
import           Control.Arrow          (Arrow, Kleisli (..), arr, (>>>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON, eitherDecode, encode)
import           Data.Bifunctor         (first)
import qualified Data.ByteString.Lazy   as LBS
import           Data.Text
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import qualified LLM                    (chat)

schema :: forall a. HasCodec a => Text
schema = decodeUtf8 $ LBS.toStrict $ encode $ jsonSchemaViaCodec @a

parsed :: forall b a. Arrow a => FromJSON b => a Text (Either Text b)
parsed = arr $ first pack . eitherDecode . LBS.fromStrict . encodeUtf8

orFail :: Arrow a => a (Either Text c) c
orFail = arr $ either (error . unpack) id

ka :: MonadIO m => Kleisli m () ()
ka = Kleisli $ \_ -> pure ()

-- Glue two arrows together, injecting the schema of the type required as input to second arrow
-- to the prompt given by the first (including parsing or failing of that type)
(>...>) :: forall a b c m. (MonadIO m, FromJSON b, HasCodec b) => Kleisli m a Text -> Kleisli m b c -> Kleisli m a c
(>...>) l r = l >>> as @b >>> r

as :: forall s m. (MonadIO m, FromJSON s, HasCodec s) => Kleisli m Text s
as = injectSchema @s >>> runLLM >>> parsed @s >>> orFail

runLLM :: MonadIO m => Kleisli m Text Text
runLLM = Kleisli $ \prompt -> do
    r <- liftIO $ LLM.chat prompt
    -- liftIO $ putStrLn $ unpack r
    pure r

injectSchema :: forall s a. HasCodec s => Arrow a => a Text Text
injectSchema = arr $ \prompt -> prompt <> "\n\n" <> instructions <> "\n" <> schema @s <> "\n\n" <> examples
    where instructions :: Text
          instructions = "\
            \Return response as JSON and using the following schema:\n\
            \"
          examples :: Text
          examples = "\
            \Example schema: {\"type\":\"string\"}\n\
            \Valid response: \"Hello, how are you?\"\n\
            \Invalid response: \"{\"string\":\"Hello, how are you?\"}\n\
            \\n\
            \Example schema: {\"$comment\":\"A dog record\",\"properties\":{\"breed\":{\"$comment\":\"Type of dog breed\",\"type\":\"string\"},\"age\":{\"$comment\":\"Age of the dog\",\"type\":\"number\"},\"name\":{\"$comment\":\"The dogs name\",\"type\":\"string\"}},\"required\":[\"age\",\"name\"],\"type\":\"object\"}\"\n\
            \Valid response: {\"breed\":\"schnauser\",\"age\":7,\"name\":\"Rexxie\"}\n\
            \Invalid response: [{\"breed\":\"schnauser\",\"age\":7,\"name\":\"Rexxie\"}] (in an array)\n\
            \Invalid response: {\"age\":7,\"name\":\"Rexxie\"} (missing required field)\n\
            \\n\
            \Additionally - try to be as uncreative as possible when abiding by the schema above - e.g. if there is only field and the prompt has asked for many, don't try to squash all the results into this single field. Just insert one. I.e. do not break the contract of the schema as it will not be interpretted by a human or other agent, but rather a structured parser.\n\
            \"

prompt :: Arrow a => a Text Text
prompt = arr id



