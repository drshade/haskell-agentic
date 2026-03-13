
module Agentic
    ( -- * Core Types
      Agentic
    , AgenticRWS
    , Environment(..)
    , Events
    , State(..)
    , Prompt(..)
      -- * Pattern
    , pattern Agentic
      -- * Functions
    , orFail
    , runLLM
    , run
    , runIO
    , prompt
    ) where

import           Agentic.Error                (AgenticError)
import           Control.Arrow                (Arrow, Kleisli (..), arr)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.RWS            (RWST (RWST), ask, get, runRWST,
                                               tell)
import           Control.Monad.RWS.Class      (MonadRWS)
import           Data.Text                    (Text)
import qualified LLM.Client
import           Prelude
import           UnliftIO                     (MonadUnliftIO (withRunInIO),
                                               atomically, modifyTVar,
                                               newTVarIO, readTVarIO)

type AgenticRWS m = (MonadUnliftIO m, MonadIO m, MonadRWS Environment Events State m)
type Agentic m a b = AgenticRWS m => Kleisli m a b

pattern Agentic :: (a -> m b) -> Kleisli m a b
pattern Agentic f = Kleisli f

newtype Environment = Environment { prompt :: Text }
type Events = [(Prompt, Text)]
newtype State = State ()

data Prompt = Prompt { system :: Text, user :: Text }

orFail :: Arrow a => a (Either AgenticError c) c
orFail = arr $ either (error . show) id

runLLM :: Agentic m Prompt Text
runLLM = Kleisli $ \prompt'@(Prompt system user) -> do
    -- liftIO $ putStrLn $ "Calling LLM... [" <> take 20 (unpack user) <> "]"
    reply <- liftIO $ LLM.Client.chat system user
    -- liftIO $ putStrLn "Calling LLM done!"
    tell [(prompt', reply)]
    pure reply

prompt :: Agentic m Text Prompt
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
