module Agentic where

import           Control.Arrow           (Arrow, Kleisli (..), arr, (>>>))
import           Control.Exception       (SomeException, try)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.RWS       (RWST, runRWST, tell)
import           Control.Monad.RWS.Class (MonadRWS)
import           Data.Either.Validation  (Validation (Failure, Success))
import           Data.Text               hiding (show)
import           Dhall                   (FromDhall, ToDhall)
import qualified Dhall
import qualified Dhall.Core
import qualified LLM                     (chat)
import qualified Prompts
import           UnliftIO                (MonadUnliftIO)

type AgenticRWS m = (MonadIO m, MonadRWS Environment Events State m)
type Agentic m a b = AgenticRWS m => Kleisli m a b

newtype Environment = Environment { prompt :: Text }
type Events = [(Text, Text)]
newtype State = State ()

orFail :: Arrow a => a (Either Text c) c
orFail = arr $ either (error . unpack) id

runLLM :: Agentic m Text Text
runLLM = Kleisli $ \prompt' -> do
    reply <- liftIO $ LLM.chat prompt'
    tell [(prompt', reply)]
    pure reply

extractWithRetry :: forall s m. (FromDhall s, ToDhall s) => Agentic m Text (Either Text s)
extractWithRetry = Kleisli $ \input -> do
    let attempt input' = do
            reply <- runAgentic (injectSchema @s >>> runLLM) input'
            parsed <- runAgentic (parse @s) reply
            pure (reply, parsed)
    (reply, parsed) <- attempt input
    case parsed of
        Left err -> do
            let instruction = Prompts.retryError err reply input
            (_reply', parsed') <- attempt instruction
            pure parsed'
        Right result -> pure $ Right result

extract :: forall s m. (FromDhall s, ToDhall s) => Agentic m Text s
extract = injectSchema @s >>> runLLM >>> parse @s >>> orFail

schemaOf :: forall a. FromDhall a => Text
schemaOf = case Dhall.expected (Dhall.auto @a) of
    Success result -> Dhall.Core.pretty result
    Failure err    -> error $ show err

parse :: forall b m. (FromDhall b) => Agentic m Text (Either Text b)
parse = Kleisli $ \input -> do
    result <- liftIO $ try $ Dhall.input Dhall.auto input
    case result of
        Right value -> pure $ Right value
        Left (err :: SomeException) -> pure $ Left $ "Dhall parse error: " <> pack (show err) <> "\nInput was: " <> input

injectSchema :: forall s m. (FromDhall s, ToDhall s) => Agentic m Text Text
injectSchema = Kleisli $ \prompt' -> pure $ Prompts.injectDhallSchema prompt' (schemaOf @s)

inject :: forall s m. (ToDhall s) => s -> Agentic m Text Text
inject obj = Kleisli $ \prompt' -> do
    let dhall = Dhall.Core.pretty $ Dhall.embed Dhall.inject obj
    pure $ Prompts.injectObject prompt' dhall

prompt :: Arrow a => a Text Text
prompt = arr id

runAgentic :: Kleisli m a b -> a -> m b
runAgentic = runKleisli

run :: Kleisli (RWST Environment Events State IO) Text a -> Text -> IO a
run k input = do
    -- Initial conditions for RWS
    let environment = Environment { prompt = input }
        state = State ()

    (a, _finalState, logs) <- runRWST (runKleisli k input) environment state

    mapM_   (\(_llmInput, _llmOutput) -> do
                -- putStrLn $ "LLM Input:\n[" <> unpack llmInput <> "]"
                -- putStrLn $ "LLM Output:\n[" <> unpack llmOutput <> "]"
                pure ()
            ) logs

    pure a

