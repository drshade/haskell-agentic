
module Protocol.Class where

import           Agentic                      (Agentic, AgenticRWS, Prompt (..),
                                               orFail, pattern Agentic, runLLM)
import           Autodocodec                  (HasCodec,
                                               eitherDecodeJSONViaCodec,
                                               encodeJSONViaCodec)
import           Autodocodec.Schema           (jsonSchemaViaCodec)
import           Control.Arrow                ((>>>))
import           Control.Exception            (SomeException, try)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Data.Aeson                   (encode)
import qualified Data.ByteString.Lazy         as LB (fromStrict)
import           Data.Data                    (Proxy (Proxy))
import           Data.Either.Combinators      (mapLeft)
import           Data.Either.Validation       (Validation (Failure, Success))
import           Data.Kind                    (Constraint)
import           Data.Text                    hiding (show)
import           Data.Text.Encoding           (encodeUtf8)
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Encoding      as TLE
import           Dhall                        (FromDhall, ToDhall)
import qualified Dhall
import qualified Dhall.Core
import qualified Protocol.DhallSchema.Prompts

class SchemaFormat fmt where
    type SchemaConstraint fmt a :: Constraint

    schemaOf :: forall a. SchemaConstraint fmt a => Proxy a -> Text

    -- Parse text using schema
    parseWithSchema :: forall a m. MonadIO m => SchemaConstraint fmt a => Proxy a -> Text -> m (Either Text a)

    -- Inject schema into prompt
    injectSchema :: forall a. SchemaConstraint fmt a => Proxy a -> Text -> Text
    schemaSystemPrompt :: forall a. SchemaConstraint fmt a => Proxy a -> Text

    -- Inject object into prompt
    injectObject :: SchemaConstraint fmt a => a -> Text -> Text

data DhallSchema
data JsonSchema

instance SchemaFormat DhallSchema where
  type SchemaConstraint DhallSchema a = (ToDhall a, FromDhall a)

  schemaOf :: forall a. SchemaConstraint DhallSchema a => Proxy a -> Text
  schemaOf = dhallSchemaOf

  parseWithSchema :: forall a m. MonadIO m => SchemaConstraint DhallSchema a => Proxy a -> Text -> m (Either Text a)
  parseWithSchema _ input = do
    result <- liftIO $ try $ Dhall.input Dhall.auto input
    case result of
        Right value -> pure $ Right value
        Left (err :: SomeException) -> pure $ Left $ "Dhall parse error: " <> pack (show err) <> "\nInput was: " <> input

  injectSchema :: forall a. SchemaConstraint DhallSchema a => Proxy a -> Text -> Text
  injectSchema proxy prompt =
    prompt <> "\n\nPlease respond using this Dhall schema:\n" <> dhallSchemaOf proxy

  schemaSystemPrompt :: forall a. SchemaConstraint DhallSchema a => Proxy a -> Text
  schemaSystemPrompt _ = Protocol.DhallSchema.Prompts.languageReference1

  injectObject :: SchemaConstraint DhallSchema a => a -> Text -> Text
  injectObject obj prompt =
    let dhallValue = Dhall.Core.pretty $ Dhall.embed Dhall.inject obj
    in prompt <> "\n\nHere's the object:\n" <> dhallValue

dhallSchemaOf :: forall a. (ToDhall a, FromDhall a) => Proxy a -> Text
dhallSchemaOf _ = case Dhall.expected (Dhall.auto @a) of
    Success result -> Dhall.Core.pretty result
    Failure err    -> error $ show err

instance SchemaFormat JsonSchema where
  type SchemaConstraint JsonSchema a = HasCodec a

  schemaOf :: forall a. SchemaConstraint JsonSchema a => Proxy a -> Text
  schemaOf = jsonSchemaOf

  parseWithSchema :: forall a m. MonadIO m => SchemaConstraint JsonSchema a => Proxy a -> Text -> m (Either Text a)
  parseWithSchema _ input =
    pure $ mapLeft pack $ eitherDecodeJSONViaCodec (LB.fromStrict $ encodeUtf8 input)

  injectSchema :: forall a. SchemaConstraint JsonSchema a => Proxy a -> Text -> Text
  injectSchema proxy prompt =
    prompt <> "\n\nPlease respond using this JSON schema:\n" <> jsonSchemaOf proxy

  schemaSystemPrompt :: forall a. SchemaConstraint JsonSchema a => Proxy a -> Text
  schemaSystemPrompt _ = "You reply to all responses in JSON format only, do not include any markdown or any other lead-in syntax. Just output pure JSON as a bare string."

  injectObject :: SchemaConstraint JsonSchema a => a -> Text -> Text
  injectObject obj prompt =
    let jsonValue = TL.toStrict $ TLE.decodeUtf8 $ encodeJSONViaCodec obj
    in prompt <> "\n\nHere's the object:\n" <> jsonValue

jsonSchemaOf :: forall a. HasCodec a => Proxy a -> Text
jsonSchemaOf _ = TL.toStrict $ TLE.decodeUtf8 $ encode $ jsonSchemaViaCodec @a

extractWithProxy :: forall fmt s m. (SchemaFormat fmt, SchemaConstraint fmt s, AgenticRWS m) => Proxy fmt -> Agentic m Prompt s
extractWithProxy _ = injectSchemaStep >>> runLLM >>> parseStep >>> orFail
  where
    injectSchemaStep :: Agentic m Prompt Prompt
    injectSchemaStep = Agentic $ \(Prompt _system user) ->
      pure $ Prompt (schemaSystemPrompt @fmt (Proxy @s)) $ injectSchema @fmt (Proxy @s) user

    parseStep :: Agentic m Text (Either Text s)
    parseStep = Agentic $ \input -> parseWithSchema @fmt (Proxy @s) input

injectWithProxy :: forall fmt s m. (SchemaFormat fmt, SchemaConstraint fmt s) => Proxy fmt -> s -> Agentic m Prompt Prompt
injectWithProxy _ obj = Agentic $ \(Prompt _system user) ->
  pure $ Prompt _system (injectObject @fmt obj user)

extractWith :: forall fmt s m. (SchemaFormat fmt, SchemaConstraint fmt s, AgenticRWS m) => Agentic m Prompt s
extractWith = extractWithProxy (Proxy @fmt)

injectWith :: forall fmt s m. (SchemaFormat fmt, SchemaConstraint fmt s) => s -> Agentic m Prompt Prompt
injectWith = injectWithProxy (Proxy @fmt)
