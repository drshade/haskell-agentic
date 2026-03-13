-- | LLM provider configuration and defaults.
module LLM.Provider
    ( LLMProvider(..)
    , LLMConfig(..)
    , defaultProvider
    , defaultConfig
    , providerModel
    ) where

import Data.Text (Text)

-- | Supported LLM providers.
data LLMProvider = Anthropic
    deriving (Show, Eq)

-- | Configuration for the LLM backend.
data LLMConfig = LLMConfig
    { provider  :: LLMProvider
    , model     :: String
    , maxTokens :: Int
    , apiKey    :: Text  -- ^ Set at runtime from environment variable
    }
    deriving (Show, Eq)

-- | The default provider.
defaultProvider :: LLMProvider
defaultProvider = Anthropic

-- | The model name for a given provider.
providerModel :: LLMProvider -> String
providerModel Anthropic = "claude-sonnet-4-20250514"

-- | Default configuration (API key must be set before use).
defaultConfig :: LLMConfig
defaultConfig = LLMConfig
    { provider  = Anthropic
    , model     = providerModel Anthropic
    , maxTokens = 10240
    , apiKey    = ("" :: Text)
    }
