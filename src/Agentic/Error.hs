module Agentic.Error
    ( AgenticError(..)
    ) where

import Control.Exception (Exception)
import Data.Text (Text)

-- | Errors that can occur during agent execution.
data AgenticError
    = ParseError
        { message  :: Text  -- ^ Human-readable error description
        , rawInput :: Text  -- ^ The raw text that failed to parse
        }
    | LLMError
        { message :: Text   -- ^ Error from the LLM provider
        }
    | SchemaError
        { message :: Text   -- ^ Error generating or applying a schema
        }
    deriving (Show, Eq)

instance Exception AgenticError
