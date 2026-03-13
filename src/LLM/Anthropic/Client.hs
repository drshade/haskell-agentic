module LLM.Anthropic.Client
  ( messages
  , module LLM.Anthropic.Types
  ) where

import           LLM.Anthropic.Types
import           LLM.Provider           (LLMConfig (..))

import           Control.Exception      (throwIO)
import           Data.Aeson             (decode, encode)
import qualified Data.ByteString.Char8  as BS8
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Network.HTTP.Simple

-- | Send a message to the Anthropic API and get a response.
messages :: LLMConfig -> Text -> Text -> IO MessagesResponse
messages config systemPrompt userPrompt = do
  let request = MessagesRequest
        { model = Text.pack config.model
        , messages =
            [ Message { role = "user", content = [TextContent { text = userPrompt }] }
            ]
        , maxTokens = config.maxTokens
        , system = Just [ SystemPrompt { text = systemPrompt
                                       , cacheControl = Just (CacheControl { cacheControlType = Ephemeral, ttl = Nothing })
                                       }
                        ]
        , temperature = Nothing
        , tools = Nothing
        , toolChoice = Nothing
        , stopSequences = Nothing
        , stream = Nothing
        , metadata = Nothing
        }

  let baseRequest = "POST https://api.anthropic.com/v1/messages"
  httpRequest <- parseRequest baseRequest

  let requestWithHeaders = setRequestHeaders
        [ ("x-api-key", BS8.pack config.apiKey)
        , ("anthropic-version", "2023-06-01")
        , ("content-type", "application/json")
        ] httpRequest

  let requestWithBody = setRequestBodyLBS (encode request) requestWithHeaders

  response <- httpLBS requestWithBody

  let responseBody = getResponseBody response

  case decode responseBody of
    Nothing     -> throwIO $ userError $ "Failed to decode response: " ++ show responseBody
    Just result -> pure result
