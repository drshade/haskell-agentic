-- | Core types and functions for the @haskell-agentic@ framework.
--
-- Agents are modelled as Kleisli arrows in a monad satisfying 'AgenticRWS'.
-- This enables composition with standard @Control.Arrow@ combinators such as
-- @>>>@ and @***@.
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
import           Control.Monad.RWS            (RWST (RWST), ask, asks, get,
                                               runRWST, tell)
import           Control.Monad.RWS.Class      (MonadRWS)
import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (Text, pack)
import qualified LLM.Client
import           LLM.Provider                 (LLMConfig (..), defaultConfig)
import           Prelude
import           System.Environment           (lookupEnv)
import           UnliftIO                     (MonadUnliftIO (withRunInIO),
                                               atomically, modifyTVar,
                                               newTVarIO, readTVarIO)

-- | Constraint alias for the monad stack used by agents.
-- Provides 'MonadIO', 'MonadUnliftIO', and the RWS environment.
type AgenticRWS m = (MonadUnliftIO m, MonadIO m, MonadRWS Environment Events State m)

-- | The core agent type. An agent is a Kleisli arrow in a monad satisfying
-- 'AgenticRWS'. Agents compose with @>>>@ and other arrow combinators.
type Agentic m a b = AgenticRWS m => Kleisli m a b

-- | Pattern synonym for constructing and destructuring @Kleisli@ arrows.
-- Equivalent to wrapping\/unwrapping @Kleisli@.
pattern Agentic :: (a -> m b) -> Kleisli m a b
pattern Agentic f = Kleisli f

-- | The reader environment for agent execution.
data Environment = Environment
    { llmConfig    :: LLMConfig  -- ^ LLM backend configuration
    , initialInput :: Text       -- ^ The top-level input to the agent pipeline
    }

-- | Writer output: a log of each (prompt, response) pair produced during execution.
type Events = [(Prompt, Text)]

-- | Mutable state threaded through agent execution.
newtype State = State ()

-- | A prompt to send to the LLM: a system instruction and a user message.
data Prompt = Prompt { system :: Text, user :: Text }

-- | Unwrap an 'Either' result, calling 'error' on 'Left'.
-- Useful at the end of an extraction pipeline.
orFail :: Arrow a => a (Either AgenticError c) c
orFail = arr $ either (error . show) id

-- | Send the current @Prompt@ to the LLM and return the raw response text.
runLLM :: Agentic m Prompt Text
runLLM = Kleisli $ \prompt'@(Prompt system user) -> do
    config <- asks (.llmConfig)
    reply  <- liftIO $ LLM.Client.chatWith config system user
    tell [(prompt', reply)]
    pure reply

-- | Lift a 'Text' input into a @Prompt@ with an empty system prompt.
prompt :: Agentic m Text Prompt
prompt = arr $ \user -> Prompt { system = "", user = user }

-- | Execute a Kleisli arrow in the current monadic context.
run :: Kleisli m a b -> a -> m b
run = runKleisli

-- | Execute an agent pipeline in 'IO', reading LLM config from the environment.
-- Reads @ANTHROPIC_KEY@ from the OS environment.
runIO :: Kleisli (RWST Environment Events State IO) Text a -> Text -> IO a
runIO k input = do
    apiKey' <- fromMaybe "" <$> lookupEnv "ANTHROPIC_KEY"
    let config      = defaultConfig { apiKey = pack apiKey' }
        environment = Environment { llmConfig = config, initialInput = input }
        state       = State ()

    (a, _finalState, _logs) <- runRWST (runKleisli k input) environment state

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
