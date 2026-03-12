module Protocol.JSONSchema.Marshal where

import           Autodocodec             (HasCodec, eitherDecodeJSONViaCodec,
                                          encodeJSONViaCodec)
import           Autodocodec.Schema      (jsonSchemaViaCodec)
import           Control.Monad.IO.Class  (MonadIO)
import           Data.Aeson              (encode)
import qualified Data.ByteString.Lazy    as LB (fromStrict)
import           Data.Either.Combinators (mapLeft)
import           Data.Proxy              (Proxy)
import           Data.Text               hiding (show)
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Protocol.Class          (SchemaFormat (..))

data Json

instance SchemaFormat Json where
  type SchemaConstraint Json a = HasCodec a

  schemaOf :: forall a. SchemaConstraint Json a => Proxy a -> Text
  schemaOf = jsonSchemaOf

  parseWithSchema :: forall a m. MonadIO m => SchemaConstraint Json a => Proxy a -> Text -> m (Either Text a)
  parseWithSchema _ input =
    pure $ mapLeft pack $ eitherDecodeJSONViaCodec (LB.fromStrict $ encodeUtf8 input)

  injectSchema :: forall a. SchemaConstraint Json a => Proxy a -> Text -> Text
  injectSchema proxy prompt =
    prompt <> "\n\nPlease respond using this JSON schema:\n" <> jsonSchemaOf proxy

  systemPrompt :: forall a. SchemaConstraint Json a => Proxy a -> Text
  systemPrompt _ = "You reply to all responses in JSON format only, do not include any markdown or any other lead-in syntax. Just output pure JSON as a bare string."

  injectObject :: SchemaConstraint Json a => a -> Text -> Text
  injectObject obj prompt =
    let jsonValue = TL.toStrict $ TLE.decodeUtf8 $ encodeJSONViaCodec obj
    in prompt <> "\n\nHere's the object:\n" <> jsonValue

jsonSchemaOf :: forall a. HasCodec a => Proxy a -> Text
jsonSchemaOf _ = TL.toStrict $ TLE.decodeUtf8 $ encode $ jsonSchemaViaCodec @a
