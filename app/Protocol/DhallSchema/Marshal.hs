{-# LANGUAGE TypeFamilies #-}

module Protocol.DhallSchema.Marshal where

import           Agentic
import           Control.Exception      (SomeException, try)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Either.Validation (Validation (Failure, Success))
import           Data.Text              hiding (show)
import           Dhall                  (FromDhall, ToDhall)
import qualified Dhall
import qualified Dhall.Core
import           Protocol.Class



-- schemaOf :: forall a. FromDhall a => Text
-- schemaOf = case Dhall.expected (Dhall.auto @a) of
--     Success result -> Dhall.Core.pretty result
--     Failure err    -> error $ show err

-- parse :: forall b m. (FromDhall b) => Agentic m Text (Either Text b)
-- parse = Agentic $ \input -> do
--     result <- liftIO $ try $ Dhall.input Dhall.auto input
--     case result of
--         Right value -> pure $ Right value
--         Left (err :: SomeException) -> pure $ Left $ "Dhall parse error: " <> pack (show err) <> "\nInput was: " <> input

-- injectSchema :: forall s m. (FromDhall s, ToDhall s) => Agentic m Prompt Prompt
-- injectSchema = Agentic $ \(Prompt _system user) ->
--     pure $ Prompt Prompts.languageReference1 (Prompts.injectDhallSchema user (schemaOf @s))

-- inject :: forall s m. (ToDhall s) => s -> Agentic m Prompt Prompt
-- inject obj = Agentic $ \(Prompt system user) -> do
--     let dhall = Dhall.Core.pretty $ Dhall.embed Dhall.inject obj
--     pure $ Prompt system (Prompts.injectObject user dhall)
