-- | JSON Schema backend for the 'Protocol.Class.SchemaFormat' typeclass,
-- using @autodocodec@ for codec derivation.
module Protocol.JSONSchema.Marshal where

import           Agentic.Error           (AgenticError (..))
import           Autodocodec             (HasCodec, eitherDecodeJSONViaCodec,
                                          encodeJSONViaCodec)
import           Autodocodec.Schema      (jsonSchemaViaCodec)
import           Control.Monad.IO.Class  (MonadIO)
import           Data.Aeson              (encode)
import qualified Data.ByteString.Lazy    as LB (fromStrict)
import           Data.Proxy              (Proxy)
import           Data.Text               hiding (show)
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Protocol.Class          (SchemaFormat (..))

-- | Phantom type tag selecting the JSON Schema format.
-- Use with @extractWith \@Json@ or @injectWith \@Json@.
data Json

instance SchemaFormat Json where
  type SchemaConstraint Json a = HasCodec a

  schemaOf :: forall a. SchemaConstraint Json a => Proxy a -> Text
  schemaOf = jsonSchemaOf

  parseWithSchema :: forall a m. MonadIO m => SchemaConstraint Json a => Proxy a -> Text -> m (Either AgenticError a)
  parseWithSchema _ input =
    pure $ either
        (\errMsg -> Left $ ParseError { message = "JSON parse error: " <> pack errMsg, rawInput = input })
        Right
        (eitherDecodeJSONViaCodec (LB.fromStrict $ encodeUtf8 input))

  injectSchema :: forall a. SchemaConstraint Json a => Proxy a -> Text -> Text
  injectSchema proxy prompt =
    prompt <> "\n\nPlease respond using this JSON schema:\n" <> jsonSchemaOf proxy

  systemPrompt :: forall a. SchemaConstraint Json a => Proxy a -> Text
  systemPrompt _ = "You reply to all responses in JSON format only, do not include any markdown or any other lead-in syntax. Just output pure JSON as a bare string."

  injectObject :: SchemaConstraint Json a => a -> Text -> Text
  injectObject obj prompt =
    let jsonValue = TL.toStrict $ TLE.decodeUtf8 $ encodeJSONViaCodec obj
    in prompt <> "\n\nHere's the object:\n" <> jsonValue

-- | Render the JSON Schema for type @a@ as text, using its 'HasCodec' instance.
jsonSchemaOf :: forall a. HasCodec a => Proxy a -> Text
jsonSchemaOf _ = TL.toStrict $ TLE.decodeUtf8 $ encode $ jsonSchemaViaCodec @a
