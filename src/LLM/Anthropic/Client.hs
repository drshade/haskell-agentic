module LLM.Anthropic.Client
  ( messages
  , MessagesRequest(..)
  , MessagesResponse(..)
  , Message(..)
  , ContentBlock(..)
  , module LLM.Anthropic.Types
  ) where

import           LLM.Anthropic.Types

import           Control.Exception     (throwIO)
import           Data.Aeson            (decode, encode)
import qualified Data.ByteString.Char8 as BS8
import           Network.HTTP.Simple

-- | Send a message to the Anthropic API and get a response
messages :: String -> MessagesRequest -> IO MessagesResponse
messages apiKey req = do
  let baseRequest = "POST https://api.anthropic.com/v1/messages"
  request <- parseRequest baseRequest

  let requestWithHeaders = setRequestHeaders
        [ ("x-api-key", BS8.pack apiKey)
        , ("anthropic-version", "2023-06-01")
        , ("content-type", "application/json")
        ] request

  let requestWithBody = setRequestBodyLBS (encode req) requestWithHeaders

  -- putStrLn $ show requestWithBody
  -- putStrLn $ show (encode req)

  response <- httpLBS requestWithBody

  let responseBody = getResponseBody response

  case decode responseBody of
    Nothing -> throwIO $ userError $ "Failed to decode response: " ++ show responseBody
    Just result -> pure result

