module Agentic.Provider.Anthropic
  ( AnthropicConfig(..)
  , defaultAnthropicConfig
  , runAnthropic
  ) where

import Agentic.Effects (LLM(..), LLMRequest(..), LLMResponse(..), LLMMessage(..), LLMUsage(..))
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (Eff, IOE, (:>), liftIO)
import Effectful.Dispatch.Dynamic (interpret)
import qualified LLM.Anthropic.Client as Anthropic
import qualified LLM.Anthropic.Types as AT
import System.Environment (setEnv)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data AnthropicConfig = AnthropicConfig
  { apiKey :: Maybe Text  -- ^ Nothing = read ANTHROPIC_KEY env var
  , model  :: Text
  } deriving (Show)

defaultAnthropicConfig :: AnthropicConfig
defaultAnthropicConfig = AnthropicConfig
  { apiKey = Nothing
  , model  = "claude-sonnet-4-20250514"
  }

-- ---------------------------------------------------------------------------
-- Interpreter
-- ---------------------------------------------------------------------------

runAnthropic :: (IOE :> es) => AnthropicConfig -> Eff (LLM : es) a -> Eff es a
runAnthropic cfg = interpret $ \_ (Call req) -> liftIO $ do
  -- If an API key is provided in config, set it in the environment so the
  -- underlying client can pick it up (client always reads ANTHROPIC_KEY).
  case cfg.apiKey of
    Nothing  -> pure ()
    Just key -> setEnv "ANTHROPIC_KEY" (T.unpack key)

  let anthropicReq = toAnthropicRequest cfg.model req
  resp <- Anthropic.messages anthropicReq
  pure (fromAnthropicResponse resp)

-- ---------------------------------------------------------------------------
-- Translation: generic → Anthropic
-- ---------------------------------------------------------------------------

toAnthropicRequest :: Text -> LLMRequest -> AT.MessagesRequest
toAnthropicRequest mdl req = AT.MessagesRequest
  { AT.model         = mdl
  , AT.messages      = map toAnthropicMessage req.messages
  , AT.maxTokens     = 10240
  , AT.system        = fmap (\txt -> [AT.SystemPrompt txt Nothing]) req.systemPrompt
  , AT.temperature   = Nothing
  , AT.tools         = Nothing
  , AT.toolChoice    = Nothing
  , AT.stopSequences = Nothing
  , AT.stream        = Nothing
  , AT.metadata      = Nothing
  }

toAnthropicMessage :: LLMMessage -> AT.Message
toAnthropicMessage msg = AT.Message
  { AT.role    = msg.role
  , AT.content = [AT.TextContent { AT.text = msg.content }]
  }

-- ---------------------------------------------------------------------------
-- Translation: Anthropic → generic
-- ---------------------------------------------------------------------------

fromAnthropicResponse :: AT.MessagesResponse -> LLMResponse
fromAnthropicResponse resp = LLMResponse
  { llmContent = extractText resp.content
  , toolCalls  = []   -- wired in Task 13
  , usage      = Just (fromAnthropicUsage resp.usage)
  }

extractText :: [AT.ResponseContentBlock] -> Maybe Text
extractText blocks =
  case [t | AT.ResponseTextContent t <- blocks] of
    (t : _) -> Just t
    []      -> Nothing

fromAnthropicUsage :: AT.Usage -> LLMUsage
fromAnthropicUsage u = LLMUsage
  { inputTokens  = u.inputTokens
  , outputTokens = u.outputTokens
  }
