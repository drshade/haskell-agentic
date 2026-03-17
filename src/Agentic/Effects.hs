module Agentic.Effects
  ( -- * LLM effect
    LLM
  , LLMRequest(..)
  , LLMResponse(..)
  , LLMMessage(..)
  , ToolSchema(..)
  , ToolCall(..)
  , LLMUsage(..)
  , call
    -- * Agent config effect
  , AgentConfig
  , getSystemPrompt
  , getModel
    -- * Agent session effect
  , AgentSession
  , getMessages
  , appendMessage
  , clearMessages
    -- * Observability
  , AgentEvents
  , Event(..)
  , SchemaFormatTag(..)
  , emit
    -- * Interpreters
  , runEventsNoop
  , runEventsStdout
  , runAgentConfig
  , runAgentSession
  ) where

import Agentic.Error (LLMError, SchemaError, ToolError)
import Data.Text (Text)
import Effectful (Eff, Effect, IOE, (:>), liftIO)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import System.IO (hPutStrLn, stderr)

-- ---------------------------------------------------------------------------
-- Request / Response types
-- ---------------------------------------------------------------------------

data LLMMessage = LLMMessage
  { role    :: Text  -- ^ Valid values: "user", "assistant", "tool"
  , content :: Text
  } deriving (Show, Generic)

data ToolSchema = ToolSchema
  { name        :: Text
  , description :: Text
  , inputSchema :: Text
  } deriving (Show, Generic)

data ToolCall = ToolCall
  { callId :: Text
  , name   :: Text
  , input  :: Text
  } deriving (Show, Generic)

data LLMUsage = LLMUsage
  { inputTokens  :: Int
  , outputTokens :: Int
  } deriving (Show, Generic)

data LLMRequest = LLMRequest
  { messages     :: [LLMMessage]
  , tools        :: [ToolSchema]
  , systemPrompt :: Maybe Text
  } deriving (Show, Generic)

data LLMResponse = LLMResponse
  { llmContent :: Maybe Text
  , toolCalls  :: [ToolCall]
  , usage      :: Maybe LLMUsage
  } deriving (Show, Generic)

-- ---------------------------------------------------------------------------
-- Effects
-- ---------------------------------------------------------------------------

data LLM :: Effect where
  Call :: LLMRequest -> LLM m LLMResponse

makeEffect ''LLM

data AgentConfig :: Effect where
  GetSystemPrompt :: AgentConfig m (Maybe Text)
  GetModel        :: AgentConfig m Text

makeEffect ''AgentConfig

data AgentSession :: Effect where
  GetMessages    :: AgentSession m [LLMMessage]
  AppendMessage  :: LLMMessage -> AgentSession m ()
  ClearMessages  :: AgentSession m ()

makeEffect ''AgentSession

-- ---------------------------------------------------------------------------
-- Observability
-- ---------------------------------------------------------------------------

data SchemaFormatTag = DhallFormat | JsonFormat
  deriving (Show, Generic)

data Event
  = LLMCallStarted  LLMRequest
  | LLMCallCompleted LLMResponse
  | LLMCallFailed   LLMError
  | SchemaExtractAttempt
      { attempt      :: Natural
      , schemaFormat :: SchemaFormatTag
      , targetType   :: Text
      }
  | SchemaExtractSuccess
      { schemaFormat :: SchemaFormatTag
      , targetType   :: Text
      }
  | SchemaExtractFailed  SchemaError
  | ToolDispatched Text
  | ToolSucceeded  Text
  | ToolFailed     Text (ToolError Text)
  | RetryAttempt
      { attempt     :: Natural
      , maxAttempts :: Natural
      , reason      :: SchemaError
      }
  deriving (Show)

data AgentEvents :: Effect where
  Emit :: Event -> AgentEvents m ()

makeEffect ''AgentEvents

-- ---------------------------------------------------------------------------
-- Interpreters
-- ---------------------------------------------------------------------------

runEventsNoop :: Eff (AgentEvents : es) a -> Eff es a
runEventsNoop = interpret $ \_ (Emit _) -> pure ()

runEventsStdout :: IOE :> es => Eff (AgentEvents : es) a -> Eff es a
runEventsStdout = interpret $ \_ (Emit event) ->
  liftIO $ hPutStrLn stderr $ "[AgentEvent] " ++ show event

runAgentConfig :: Maybe Text -> Text -> Eff (AgentConfig : es) a -> Eff es a
runAgentConfig sysPrompt modelName = interpret $ \_ eff -> case eff of
  GetSystemPrompt -> pure sysPrompt
  GetModel        -> pure modelName

runAgentSession :: IOE :> es => Eff (AgentSession : es) a -> Eff es a
runAgentSession action = do
  ref <- liftIO $ newIORef ([] :: [LLMMessage])
  interpret (\_ eff -> case eff of
    GetMessages       -> liftIO $ readIORef ref
    AppendMessage m   -> liftIO $ modifyIORef ref (++ [m])
    ClearMessages     -> liftIO $ writeIORef ref []
    ) action
