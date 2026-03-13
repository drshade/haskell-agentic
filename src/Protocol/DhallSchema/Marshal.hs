-- | Dhall schema backend for the 'Protocol.Class.SchemaFormat' typeclass.
module Protocol.DhallSchema.Marshal where

import           Agentic.Error                (AgenticError (..))
import           Control.Exception            (SomeException, try)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Data.Data                    (Proxy)
import           Data.Either.Validation       (Validation (Failure, Success))
import           Data.Text                    hiding (show)
import           Dhall                        (FromDhall, ToDhall)
import qualified Dhall
import qualified Dhall.Core
import           Protocol.Class               (SchemaFormat (..))
import qualified Protocol.DhallSchema.Prompts

-- | Phantom type tag selecting the Dhall schema format.
-- Use with @extractWith \@Dhall@ or @injectWith \@Dhall@.
data Dhall

instance SchemaFormat Dhall where
  type SchemaConstraint Dhall a = (ToDhall a, FromDhall a)

  schemaOf :: forall a. SchemaConstraint Dhall a => Proxy a -> Text
  schemaOf = dhallSchemaOf

  parseWithSchema :: forall a m. MonadIO m => SchemaConstraint Dhall a => Proxy a -> Text -> m (Either AgenticError a)
  parseWithSchema _ input = parseDhall input

  injectSchema :: forall a. SchemaConstraint Dhall a => Proxy a -> Text -> Text
  injectSchema proxy prompt =
    prompt <> "\n\nPlease respond using this Dhall schema:\n" <> dhallSchemaOf proxy

  systemPrompt :: forall a. SchemaConstraint Dhall a => Proxy a -> Text
  systemPrompt _ = Protocol.DhallSchema.Prompts.languageReference1_no_functions

  injectObject :: SchemaConstraint Dhall a => a -> Text -> Text
  injectObject obj prompt =
    let dhallValue = Dhall.Core.pretty $ Dhall.embed Dhall.inject obj
    in prompt <> "\n\nHere's the object:\n" <> dhallValue

-- | Render the Dhall schema for type @a@ as text.
dhallSchemaOf :: forall a. (ToDhall a, FromDhall a) => Proxy a -> Text
dhallSchemaOf _ = case Dhall.expected (Dhall.auto @a) of
    Success result -> Dhall.Core.pretty result
    -- Note: Failure here indicates a malformed ToDhall instance, which is a
    -- programming error. We use 'error' since 'SchemaFormat.schemaOf' returns
    -- Text, not Either. This will be addressed in a future API revision.
    Failure err    -> error $ show err

-- | Parse Dhall text into a Haskell value.
parseDhall :: forall b m. (FromDhall b, MonadIO m) => Text -> m (Either AgenticError b)
parseDhall input = do
    result <- liftIO $ try $ Dhall.input Dhall.auto input
    case result of
        Right value -> pure $ Right value
        Left (err :: SomeException) ->
            pure $ Left $ ParseError
                { message  = "Dhall parse error: " <> pack (show err)
                , rawInput = input
                }

