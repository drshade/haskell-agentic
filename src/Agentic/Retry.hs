module Agentic.Retry
    ( RetryConfig(..)
    , defaultRetry
    , withRetry
    ) where

import Agentic.Error (AgenticError(..))

-- | Configuration for retry behaviour on agent parse failures.
data RetryConfig = RetryConfig
    { maxAttempts :: Int
      -- ^ Maximum number of attempts (including the first). Must be >= 1.
    }
    deriving (Show, Eq)

-- | Default retry config: 2 attempts (1 initial + 1 retry).
defaultRetry :: RetryConfig
defaultRetry = RetryConfig { maxAttempts = 2 }

-- | Run an action, retrying on 'Left' up to 'maxAttempts' times total.
-- Returns the first 'Right', or 'Left' if all attempts fail.
withRetry :: RetryConfig -> IO (Either AgenticError a) -> IO (Either AgenticError a)
withRetry config action = go config.maxAttempts
  where
    go 0 = pure (Left $ ParseError "No attempts remaining" "")
    go n = do
        result <- action
        case result of
            Right x -> pure (Right x)
            Left _  -> go (n - 1)
