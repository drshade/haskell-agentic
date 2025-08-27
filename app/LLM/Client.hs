module LLM.Client where

import qualified LLM.Anthropic.Client   as Anthropic
import           LLM.Anthropic.Types

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text              as Text

-- | Simple chat function that takes system and user prompts and returns the response text
chat :: MonadIO m => Text.Text -> Text.Text -> m Text.Text
chat systemPrompt userPrompt = liftIO $ do
  let request = MessagesRequest
        { model = "claude-sonnet-4-20250514"
        , messages =
            [ Message { role = "user", content = [TextContent { text = userPrompt }] }
            ]
        , maxTokens = 1024
        , system = Just [SystemPrompt { text = systemPrompt, cacheControl = Just (CacheControl { cacheControlType = Ephemeral, ttl = Nothing }) }]
        , temperature = Nothing
        , tools = Nothing
        , toolChoice = Nothing
        , stopSequences = Nothing
        , stream = Nothing
        , metadata = Nothing
        }

  response <- Anthropic.messages request

  -- Extract text from the first text content block
  case response.content of
    (ResponseTextContent text : _) -> pure text
    _                              -> pure "No text response received"
