module Protocol.DhallSchema.Marshal where

import           Agentic                      (Agentic, AgenticRWS, Prompt (..),
                                               pattern Agentic, run, runLLM)
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

data Dhall

instance SchemaFormat Dhall where
  type SchemaConstraint Dhall a = (ToDhall a, FromDhall a)

  schemaOf :: forall a. SchemaConstraint Dhall a => Proxy a -> Text
  schemaOf = dhallSchemaOf

  parseWithSchema :: forall a m. MonadIO m => SchemaConstraint Dhall a => Proxy a -> Text -> m (Either Text a)
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

dhallSchemaOf :: forall a. (ToDhall a, FromDhall a) => Proxy a -> Text
dhallSchemaOf _ = case Dhall.expected (Dhall.auto @a) of
    Success result -> Dhall.Core.pretty result
    Failure err    -> error $ show err

-- | Parse Dhall text into a Haskell value.
parseDhall :: forall b m. (FromDhall b, MonadIO m) => Text -> m (Either Text b)
parseDhall input = do
    result <- liftIO $ try $ Dhall.input Dhall.auto input
    case result of
        Right value -> pure $ Right value
        Left (err :: SomeException) -> pure $ Left $ "Dhall parse error: " <> pack (show err) <> "\nInput was: " <> input

-- | Like 'extractWith @Dhall' but retries once on parse failure.
extractWithRetryDhall :: forall s m. (FromDhall s, ToDhall s, AgenticRWS m) => Agentic m Prompt (Either Text s)
extractWithRetryDhall = Agentic $ \prompt'@(Prompt system user) -> do
    let attemptRaw input' = do
            reply <- run runLLM input'
            parsed <- parseDhall @s reply
            pure (reply, parsed)
    (reply, parsed) <- attemptRaw prompt'
    case parsed of
        Left err -> do
            let instruction = Protocol.DhallSchema.Prompts.retryError err reply user
            (_reply', parsed') <- attemptRaw $ Prompt system instruction
            pure parsed'
        Right result -> pure $ Right result
