module LLM.Client where

import qualified LLM.Anthropic.Client   as Anthropic
import           LLM.Anthropic.Types

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (Value (Array, Null, Object, String),
                                         object, (.=))
import qualified Data.Text              as Text
import qualified Data.Vector            as Vector

-- | Simple chat function that takes system and user prompts and returns the response text
chat :: MonadIO m => Text.Text -> Text.Text -> m Text.Text
chat systemPrompt userPrompt = liftIO $ do

  let dummy :: Value = object
        [ "type" .= String "object"
        , "properties" .= Object mempty
        , "required" .= Array mempty
        ]

  let request = MessagesRequest
        { model = "claude-sonnet-4-20250514"
        , messages =
            [ Message { role = "user", content = [TextContent { text = userPrompt }] }
            ]
        , maxTokens = 10240
        , system = Just [ SystemPrompt { text = systemPrompt
                                       , cacheControl = Just (CacheControl { cacheControlType = Ephemeral, ttl = Nothing })
                                      --  , cacheControl = Nothing
                                       }
                        ]
        , temperature = Nothing
        , tools = Nothing -- Just [ Tool { name = "test_tool", description = Just "test description", inputSchema = dummy } ]
        , toolChoice = Nothing
        , stopSequences = Nothing
        , stream = Nothing
        , metadata = Nothing
        }

  response <- Anthropic.messages request

  liftIO $ putStrLn $ "Got back " <> (show $ length response.content) <> " blocks!"

  -- Extract text from the first text content block
  case response.content of
    (ResponseTextContent text : _) -> pure text
    _                              -> pure "Something else?"
