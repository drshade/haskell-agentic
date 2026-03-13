module Agentic.Schema.Json
  ( jsonSchemaOf
  , parseJson
  , injectJsonSchema
  , injectJsonObject
  ) where

import Agentic.Error (SchemaError(..))
import Autodocodec (HasCodec)
import Autodocodec.Schema (jsonSchemaViaCodec)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

-- | Render the JSON Schema for @a@ as Text.
jsonSchemaOf :: forall a. HasCodec a => Text
jsonSchemaOf = decodeUtf8 $ toStrict $ encodePretty $ jsonSchemaViaCodec @a

-- | Parse a JSON-encoded value, returning a SchemaError on failure.
parseJson :: forall a. FromJSON a => Text -> Either SchemaError a
parseJson input = case eitherDecodeStrict (encodeUtf8 input) of
  Right value -> Right value
  Left err    -> Left $ JsonParseError "" (pack err)

-- | Inject the JSON Schema for @a@ into a user prompt.
injectJsonSchema :: forall a. HasCodec a => Text -> Text
injectJsonSchema prompt =
  prompt <> "\n\nRespond with valid JSON matching this schema:\n" <> jsonSchemaOf @a

-- | Inject a JSON-serialised value into a prompt.
injectJsonObject :: forall a. ToJSON a => a -> Text -> Text
injectJsonObject obj prompt =
  prompt <> "\n\nInput:\n" <> decodeUtf8 (toStrict (encodePretty obj))
