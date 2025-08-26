{-# LANGUAGE PatternSynonyms #-}

module Agentic where

import           Control.Arrow           (Arrow, Kleisli (..), arr, (>>>))
import           Control.Exception       (SomeException, try)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.RWS       (RWST (RWST), ask, get, runRWST, tell)
import           Control.Monad.RWS.Class (MonadRWS)
import           Data.Either.Validation  (Validation (Failure, Success))
import           Data.Text               hiding (show)
import           Dhall                   (FromDhall, ToDhall)
import qualified Dhall
import qualified Dhall.Core
import qualified LLM                     (chat)
import qualified Prompts
import           UnliftIO                (MonadUnliftIO (withRunInIO),
                                          atomically, modifyTVar, newTVarIO,
                                          readTVarIO)

type AgenticRWS m = (MonadIO m, MonadRWS Environment Events State m)
type Agentic m a b = AgenticRWS m => Kleisli m a b

pattern Agentic :: (a -> m b) -> Kleisli m a b
pattern Agentic f = Kleisli f

newtype Environment = Environment { prompt :: Text }
type Events = [(Prompt, Text)]
newtype State = State ()

data Prompt = Prompt { system :: Text, user :: Text }

orFail :: Arrow a => a (Either Text c) c
orFail = arr $ either (error . unpack) id

runLLM :: Agentic m Prompt Text
runLLM = Kleisli $ \prompt'@(Prompt system user) -> do
    reply <- liftIO $ LLM.chat system user
    tell [(prompt', reply)]
    pure reply

extractWithRetry :: forall s m. (FromDhall s, ToDhall s) => Agentic m Prompt (Either Text s)
extractWithRetry = Kleisli $ \prompt'@(Prompt system user) -> do
    let attempt input' = do
            reply <- run (injectSchema @s >>> runLLM) input'
            parsed <- run (parse @s) reply
            pure (reply, parsed)
    (reply, parsed) <- attempt prompt'
    case parsed of
        Left err -> do
            let instruction = Prompts.retryError err reply user
            (_reply', parsed') <- attempt $ Prompt system instruction
            pure parsed'
        Right result -> pure $ Right result

extract :: forall s m. (FromDhall s, ToDhall s) => Agentic m Prompt s
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

injectSchema :: forall s m. (FromDhall s, ToDhall s) => Agentic m Prompt Prompt
injectSchema = Kleisli $ \(Prompt _system user) ->
    pure $ Prompt Prompts.languageReference1 (Prompts.injectDhallSchema user (schemaOf @s))

inject :: forall s m. (ToDhall s) => s -> Agentic m Prompt Prompt
inject obj = Kleisli $ \(Prompt system user) -> do
    let dhall = Dhall.Core.pretty $ Dhall.embed Dhall.inject obj
    pure $ Prompt system (Prompts.injectObject user dhall)

prompt :: Arrow a => a Text Prompt
prompt = arr $ \user -> Prompt { system = "", user = user }

run :: Kleisli m a b -> a -> m b
run = runKleisli

runIO :: Kleisli (RWST Environment Events State IO) Text a -> Text -> IO a
runIO k input = do
    -- Initial conditions for RWS
    let environment = Environment { prompt = input }
        state = State ()

    (a, _finalState, logs) <- runRWST (runKleisli k input) environment state

    mapM_   (\(Prompt system user, _llmOutput) -> do
                putStrLn $ "LLM Input:\n[" <> unpack user <> "]"
                putStrLn $ "LLM Output:\n[" <> unpack _llmOutput <> "]"
                pure ()
            ) logs

    pure a

-- My merging MonadUnliftIO instance (easy because State is current (), but in future it wont be!)
-- This can probably fall away if / when we move away from RWS to RW alone
instance MonadUnliftIO (RWST Environment Events State IO) where
  withRunInIO :: ((forall a. RWST Environment Events State IO a -> IO a) -> IO b) -> RWST Environment Events State IO b
  withRunInIO action = do
    env <- ask
    state <- get
    events_ref <- liftIO $ newTVarIO []  -- Collect all events

    let runInIO :: forall a. RWST Environment Events State IO a -> IO a
        runInIO (RWST rwst) = do
          (a, _finalState, events) <- rwst env state
          -- Atomically append events
          atomically $ modifyTVar events_ref (++ events)
          return a

    result <- liftIO $ action runInIO

    -- Get all accumulated events and add them to current writer
    final_events <- liftIO $ readTVarIO events_ref
    tell final_events

    return result
