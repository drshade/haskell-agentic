
module Agentic where

import           Control.Arrow                (Arrow, Kleisli (..), arr, (>>>))
import           Control.Exception            (SomeException, try)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.RWS            (RWST (RWST), ask, get, runRWST,
                                               tell)
import           Control.Monad.RWS.Class      (MonadRWS)
import           Data.Either.Validation       (Validation (Failure, Success))
import           Data.Text                    (Text, pack, unpack)
import           Dhall                        (FromDhall, ToDhall)
import qualified Dhall
import qualified Dhall.Core
import qualified LLM.Client
import           Prelude
import qualified Protocol.DhallSchema.Prompts
import           UnliftIO                     (MonadUnliftIO (withRunInIO),
                                               atomically, modifyTVar,
                                               newTVarIO, readTVarIO)

type AgenticRWS m = (MonadIO m, MonadRWS Environment Events State m)
type Agentic m a b = AgenticRWS m => Kleisli m a b

pattern Agentic :: (a -> m b) -> Kleisli m a b
pattern Agentic f = Kleisli f

newtype Environment = Environment { prompt :: Text }
type Events = [(Prompt, Text)]
newtype State = State ()

data Prompt = Prompt { system :: Text, user :: Text }

schemaOf :: forall a. FromDhall a => Text
schemaOf = case Dhall.expected (Dhall.auto @a) of
    Success result -> Dhall.Core.pretty result
    Failure err    -> error $ show err

parse :: forall b m. (FromDhall b) => Agentic m Text (Either Text b)
parse = Agentic $ \input -> do
    result <- liftIO $ try $ Dhall.input Dhall.auto input
    case result of
        Right value -> pure $ Right value
        Left (err :: SomeException) -> pure $ Left $ "Dhall parse error: " <> pack (show err) <> "\nInput was: " <> input

injectSchema :: forall s m. (FromDhall s, ToDhall s) => Agentic m Prompt Prompt
injectSchema = Agentic $ \(Prompt _system user) ->
    pure $ Prompt Protocol.DhallSchema.Prompts.languageReference1 (Protocol.DhallSchema.Prompts.injectDhallSchema user (schemaOf @s))

inject :: forall s m. (ToDhall s) => s -> Agentic m Prompt Prompt
inject obj = Agentic $ \(Prompt system user) -> do
    let dhall = Dhall.Core.pretty $ Dhall.embed Dhall.inject obj
    pure $ Prompt system (Protocol.DhallSchema.Prompts.injectObject user dhall)

orFail :: Arrow a => a (Either Text c) c
orFail = arr $ either (error . unpack) id

runLLM :: Agentic m Prompt Text
runLLM = Kleisli $ \prompt'@(Prompt system user) -> do
    -- liftIO $ putStrLn $ "Calling LLM... [" <> take 20 (unpack user) <> "]"
    reply <- liftIO $ LLM.Client.chat system user
    -- liftIO $ putStrLn "Calling LLM done!"
    tell [(prompt', reply)]
    pure reply

-- Just a single retry for now
extractWithRetry :: forall s m. (FromDhall s, ToDhall s) => Agentic m Prompt (Either Text s)
extractWithRetry = Kleisli $ \prompt'@(Prompt system user) -> do
    let attempt input' = do
            reply <- run (injectSchema @s >>> runLLM) input'
            parsed <- run (parse @s) reply
            pure (reply, parsed)
    (reply, parsed) <- attempt prompt'
    case parsed of
        Left err -> do
            let instruction = Protocol.DhallSchema.Prompts.retryError err reply user
            (_reply', parsed') <- attempt $ Prompt system instruction
            pure parsed'
        Right result -> pure $ Right result

extract :: forall s m. (FromDhall s, ToDhall s) => Agentic m Prompt s
extract = injectSchema @s >>> runLLM >>> parse @s >>> orFail

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

    mapM_   (\(Prompt _system _user, _llmOutput) -> do
                -- putStrLn $ "LLM System: \n[" <> unpack _system <> "]"
                -- putStrLn $ "LLM Input:\n[" <> unpack _user <> "]"
                -- putStrLn $ "LLM Output:\n[" <> unpack _llmOutput <> "]"
                pure ()
            ) logs

    pure a

-- My merging MonadUnliftIO instance (easy because State is current (), but in
-- future it wont be!)
-- This can probably fall away if / when we move away from RWS to RW alone
instance MonadUnliftIO (RWST Environment Events State IO) where
    withRunInIO :: ((forall a. RWST Environment Events State IO a -> IO a) -> IO b) -> RWST Environment Events State IO b
    withRunInIO action = do
        env <- ask
        state <- get
        eventsRef <- liftIO $ newTVarIO []

        let runInIO :: forall a. RWST Environment Events State IO a -> IO a
            runInIO (RWST rwst) = do
                (a, _finalState, events) <- rwst env state
                -- Atomically append events
                atomically $ modifyTVar eventsRef (++ events)
                pure a

        result <- liftIO $ action runInIO
        final_events <- liftIO $ readTVarIO eventsRef
        tell final_events

        pure result
