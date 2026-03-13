module LLM.Client (chatWith) where

import qualified LLM.Anthropic.Client   as Anthropic
import           LLM.Anthropic.Types
import           LLM.Provider           (LLMConfig (..), LLMProvider (..))

import           Agentic.Error          (AgenticError (..))
import           Control.Exception      (throwIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text              as Text

-- | Send a chat message using the provided config.
chatWith :: MonadIO m => LLMConfig -> Text.Text -> Text.Text -> m Text.Text
chatWith config system user =
    case config.provider of
        Anthropic -> liftIO $ Anthropic.messages config system user >>= \response ->
            case response.content of
                (ResponseTextContent txt : _) -> pure txt
                _                             -> throwIO $ LLMError
                    { message = "Unexpected response shape from LLM provider" }
