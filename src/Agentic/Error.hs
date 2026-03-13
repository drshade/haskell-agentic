module Agentic.Error
  ( SchemaError(..)
  , LLMError(..)
  , ToolError(..)
  ) where

import Control.Exception (Exception)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client (HttpException)

-- | Errors from schema parsing (Dhall or JSON). Carries typed error info.
data SchemaError
  = DhallParseError Text       -- Dhall parse error rendered to Text
  | DhallTypeError  Text       -- Dhall type error rendered to Text
  | JsonParseError  Text Text  -- JSON path, message
  | JsonSchemaViolation Text   -- schema violation description
  deriving (Show, Generic)

instance Exception SchemaError

-- | Errors from LLM provider calls.
data LLMError
  = HttpError HttpException
  | ProviderError Int Text     -- HTTP status code, response body
  | RateLimited (Maybe Int)    -- retry-after seconds if known
  | TokenLimitExceeded Int Int -- tokens used, token limit
  deriving (Show)  -- Generic omitted: HttpException has no Generic instance

instance Exception LLMError

-- | Errors from tool dispatch.
data ToolError e
  = ToolNotFound Text
  | ToolInputDecodeFailed SchemaError
  | ToolExecutionFailed e
  deriving (Show, Generic)

deriving instance (Show e, Exception e) => Exception (ToolError e)
