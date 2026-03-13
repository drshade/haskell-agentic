module LLM.Client (chat, chatWith) where

import qualified LLM.Anthropic.Client   as Anthropic
import           LLM.Anthropic.Types
import           LLM.Provider           (LLMConfig (..), LLMProvider (..),
                                         defaultConfig)

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as Text
import           System.Environment     (lookupEnv)

-- | Send a chat message using default config (reads API key from ANTHROPIC_KEY env var).
-- Deprecated: use 'chatWith' with explicit config instead.
chat :: MonadIO m => Text.Text -> Text.Text -> m Text.Text
chat system user = do
    apiKey <- liftIO $ fromMaybe "" <$> lookupEnv "ANTHROPIC_KEY"
    let config = defaultConfig { apiKey = apiKey }
    chatWith config system user

-- | Send a chat message using the provided config.
chatWith :: MonadIO m => LLMConfig -> Text.Text -> Text.Text -> m Text.Text
chatWith config system user =
    case config.provider of
        Anthropic -> liftIO $ Anthropic.messages config system user >>= \response ->
            case response.content of
                (ResponseTextContent txt : _) -> pure txt
                _                             -> pure "Something else?"
