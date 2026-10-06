-- | Settings that several providers share, as setters that work on any
-- provider's config:
--
-- > withSystemTwo (anthropic & model "claude-sonnet-5-5" & effort Low)
-- > withSystemOne (jev & model "jev-1.13.0")
--
-- Settings only one provider has are plain functions in that provider's module.
module Agentic.Settings
  ( HasModel (..)
  , HasKey (..)
  , HasEndpoint (..)
  , HasTimeout (..)
  , HasSystem (..)
  , HasMaxTokens (..)
  , HasEffort (..)
  , Effort (..)
  , (&)
  ) where

import Data.Function ((&))
import Data.Text (Text)

class HasModel c where
  model :: Text -> c -> c

-- | The API key or token. Providers default to their environment variable.
class HasKey c where
  key :: Text -> c -> c

class HasEndpoint c where
  endpoint :: Text -> c -> c

-- | How long to wait for a response, in seconds.
class HasTimeout c where
  timeout :: Int -> c -> c

-- | A system prompt for every @draft@ in the runtime.
class HasSystem c where
  system :: Text -> c -> c

class HasMaxTokens c where
  maxTokens :: Int -> c -> c

-- | How hard the model thinks. Each provider maps these to its own levels.
class HasEffort c where
  effort :: Effort -> c -> c

data Effort = Low | Medium | High | XHigh | Max
  deriving (Eq, Ord, Show, Enum, Bounded)
