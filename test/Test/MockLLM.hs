module Test.MockLLM
  ( runLLMMock
  , staticResponse
  , failingResponse
  ) where

import Agentic.Effects (LLM(..), LLMRequest, LLMResponse(..))
import Data.Text (Text)
import Effectful (Eff)
import Effectful.Dispatch.Dynamic (interpret)

-- | Interpret LLM calls with a pure function. Never hits the network.
runLLMMock :: (LLMRequest -> LLMResponse) -> Eff (LLM : es) a -> Eff es a
runLLMMock handler = interpret $ \_ (Call req) -> pure (handler req)

-- | Always return a fixed text response.
staticResponse :: Text -> LLMRequest -> LLMResponse
staticResponse txt _ = LLMResponse
  { llmContent = Just txt
  , toolCalls  = []
  , usage      = Nothing
  }

-- | Always return empty content (simulates LLM returning nothing).
failingResponse :: LLMRequest -> LLMResponse
failingResponse _ = LLMResponse
  { llmContent = Nothing
  , toolCalls  = []
  , usage      = Nothing
  }
