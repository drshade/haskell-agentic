-- | Claude as a runtime's System Two (and, through 'viaLLM', System One).
--
-- > rt <- pure runtime >>= withSystemTwo anthropic
module Agentic.Anthropic
  ( Anthropic (..)
  , anthropic
  , AnthropicError (..)
    -- * Wire format
  , requestBody
  , decodeTurn
  ) where

import Agentic.Aeson (fromAeson)
import Agentic.Core (Instruction (..))
import Agentic.JsonSchema (objectSchema, unwrap)
import Agentic.Runtime
import Agentic.Schema (Schema)
import qualified Agentic.Value as A
import Agentic.ViaLLM (viaLLM)
import Control.Exception (Exception (..), throwIO)
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Environment (lookupEnv)

-- | Claude's settings. Start from 'anthropic' and override what you need:
--
-- > anthropic {anthropicModel = "claude-sonnet-5-5", anthropicEffort = Just "low"}
data Anthropic = Anthropic
  { anthropicModel :: Text
  , anthropicSystem :: Maybe Text
    -- ^ A system prompt for every @draft@ in the runtime.
  , anthropicMaxTokens :: Int
  , anthropicEffort :: Maybe Text
    -- ^ @low@, @medium@, @high@, @xhigh@ or @max@; the model's default if unset.
  , anthropicFallbacks :: Bool
    -- ^ Let the API retry a refused request on a fallback model it picks.
  , anthropicKey :: Maybe Text
    -- ^ Defaults to the @ANTHROPIC_API_KEY@ environment variable.
  , anthropicEndpoint :: String
  , anthropicTimeoutSeconds :: Int
  }

anthropic :: Anthropic
anthropic =
  Anthropic
    { anthropicModel = "claude-opus-5-5"
    , anthropicSystem = Nothing
    , anthropicMaxTokens = 16000
    , anthropicEffort = Nothing
    , anthropicFallbacks = True
    , anthropicKey = Nothing
    , anthropicEndpoint = "https://api.anthropic.com/v1/messages"
    , anthropicTimeoutSeconds = 600
    }

data AnthropicError
  = MissingKey
  | HttpError Int Text
    -- ^ A non-200 status, and the API's error body.
  | Refused (Maybe Text)
    -- ^ Claude declined the request, with the category if the API gave one.
  | Truncated
    -- ^ The reply hit the token limit before it was complete.
  | UnexpectedStop Text
  | UnexpectedResponse Text
  deriving (Show)

instance Exception AnthropicError where
  displayException = \case
    MissingKey -> "Anthropic: no API key. Set ANTHROPIC_API_KEY, or anthropicKey in the config."
    HttpError status body -> "Anthropic rejected the request (HTTP " <> show status <> "): " <> T.unpack body
    Refused category -> "Claude declined the request" <> maybe "" (\c -> " (" <> T.unpack c <> ")") category
    Truncated -> "Claude's reply hit the token limit; raise anthropicMaxTokens"
    UnexpectedStop reason -> "Claude stopped for an unexpected reason: " <> T.unpack reason
    UnexpectedResponse problem -> "Anthropic sent a response agentic can't read: " <> T.unpack problem

instance ProvidesSystemTwo Anthropic where
  toSystemTwo cfg = do
    key <- maybe (fmap T.pack <$> lookupEnv "ANTHROPIC_API_KEY") (pure . Just) (anthropicKey cfg) >>= maybe (throwIO MissingKey) pure
    manager <- newTlsManager
    base <- Http.parseRequest (anthropicEndpoint cfg)
    pure $ SystemTwo $ \conversation -> do
      let http =
            base
              { Http.method = "POST"
              , Http.requestHeaders =
                  [ ("x-api-key", T.encodeUtf8 key)
                  , ("anthropic-version", "2023-06-01")
                  , ("content-type", "application/json")
                  ]
                    <> [("anthropic-beta", "server-side-fallback-2026-07-01") | anthropicFallbacks cfg]
              , Http.requestBody = Http.RequestBodyLBS (TL.encodeUtf8 (TL.fromStrict (A.renderJson (requestBody cfg conversation))))
              , Http.responseTimeout = Http.responseTimeoutMicro (anthropicTimeoutSeconds cfg * 1000000)
              }
      response <- Http.httpLbs http manager
      let status = statusCode (Http.responseStatus response)
          body = Http.responseBody response
      if status /= 200
        then throwIO (HttpError status (T.decodeUtf8Lenient (LBS.toStrict body)))
        else case J.eitherDecode body of
          Left problem -> throwIO (UnexpectedResponse (T.pack problem))
          Right value -> either throwIO pure (decodeTurn conversation value)

-- | Claude answers judgements too, with uncalibrated probabilities.
instance ProvidesSystemOne Anthropic where
  toSystemOne cfg = viaLLM <$> toSystemTwo cfg

-- | The Messages API request for one turn of a step. It's the core's 'A.Value'
-- so that schemas keep their field order (see "Agentic.JsonSchema").
requestBody :: Anthropic -> Conversation -> A.Value
requestBody cfg c =
  A.Object $
    [ ("model", A.String (anthropicModel cfg))
    , ("max_tokens", A.Integer (toInteger (anthropicMaxTokens cfg)))
    ]
      <> maybe [] (\s -> [("system", A.String s)]) (anthropicSystem cfg)
      <> [("tools", A.Array (map tool (tools c))) | not (null (tools c))]
      <> [ ("messages", A.Array (task : concatMap exchange (history c)))
         , ( "output_config"
           , A.Object
               ( ("format", A.Object [("type", A.String "json_schema"), ("schema", objectSchema (output c))])
                   : maybe [] (\e -> [("effort", A.String e)]) (anthropicEffort cfg)
               )
           )
         , ("cache_control", A.Object [("type", A.String "ephemeral")])
         ]
      <> [("fallbacks", A.String "default") | anthropicFallbacks cfg]
  where
    task = message "user" (A.String (instructionText (instruction c) <> input))
    input = case state c of
      A.Null -> ""
      s -> "\n\nInput:\n" <> A.renderJson s
    tool spec =
      A.Object
        [ ("name", A.String (specName spec))
        , ("description", A.String (specDescription spec))
        , ("input_schema", objectSchema (specInput spec))
        , ("strict", A.Bool True)
        ]
    exchange = \case
      Called (Raw raw) results ->
        [ message "assistant" raw
        , message "user" (A.Array (map result results))
        ]
      Rejected (Raw raw) problem ->
        [ message "assistant" raw
        , message "user" (A.String ("That answer was rejected: " <> problem <> ". Please answer again."))
        ]
    result (callId', r) = case r of
      ToolOk v -> A.Object [("type", A.String "tool_result"), ("tool_use_id", A.String callId'), ("content", A.String (asText v))]
      ToolFailed problem ->
        A.Object [("type", A.String "tool_result"), ("tool_use_id", A.String callId'), ("content", A.String problem), ("is_error", A.Bool True)]
    asText = \case
      A.String t -> t
      v -> A.renderJson v
    message :: Text -> A.Value -> A.Value
    message role content = A.Object [("role", A.String role), ("content", content)]

-- | Read one turn from a Messages API response.
decodeTurn :: Conversation -> J.Value -> Either AnthropicError Turn
decodeTurn c = either (Left . UnexpectedResponse . T.pack) id . J.parseEither parse
  where
    parse = J.withObject "response" $ \r -> do
      content <- r .: "content"
      blocks <- traverse block content
      stop <- r .: "stop_reason"
      details <- r .:? "stop_details"
      category <- maybe (pure Nothing) (J.withObject "stop_details" (.:? "category")) details
      let raw = Raw (fromAeson (J.toJSON content))
          calls = [ToolCall i n (unwrapInput n v) | ToolUse i n v <- blocks]
          text = T.concat [t | Text t <- blocks]
      pure $ case (stop :: Text) of
        "tool_use" -> Right (Turn raw (CallTools calls))
        "end_turn" -> Right (Turn raw (Respond (final text)))
        "stop_sequence" -> Right (Turn raw (Respond (final text)))
        "refusal" -> Left (Refused category)
        "max_tokens" -> Left Truncated
        other -> Left (UnexpectedStop other)
    block = J.withObject "block" $ \b -> do
      kind <- b .: "type"
      case kind :: Text of
        "text" -> Text <$> b .: "text"
        "tool_use" -> ToolUse <$> b .: "id" <*> b .: "name" <*> (fromAeson <$> b .: "input")
        _ -> pure Other
    -- A reply that isn't JSON goes back to the core as text; the output
    -- contract then rejects it and the model gets another go.
    final text = case J.eitherDecode (TL.encodeUtf8 (TL.fromStrict text)) of
      Right v -> unwrap (output c) (fromAeson v)
      Left _ -> A.String text
    unwrapInput name v = maybe v (`unwrap` v) (inputSchema name)
    inputSchema :: Text -> Maybe Schema
    inputSchema name = case catMaybes [if specName s == name then Just (specInput s) else Nothing | s <- tools c] of
      s : _ -> Just s
      [] -> Nothing

data Block = Text Text | ToolUse Text Text A.Value | Other
