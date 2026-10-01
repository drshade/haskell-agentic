-- | Claude as a runtime's System Two (and, through 'viaLLM', System One).
--
-- > rt <- pure runtime >>= withSystemTwo anthropic
-- > rt <- pure runtime >>= withSystemTwo (anthropic & model "claude-sonnet-5-5" & effort Low)
module Agentic.Anthropic
  ( Anthropic (..)
  , anthropic
  , fallbacks
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
import Agentic.Settings
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

-- | Claude's settings. Start from 'anthropic' and change them with the setters
-- from "Agentic.Settings" ('Agentic.Settings.model', 'Agentic.Settings.system',
-- 'Agentic.Settings.effort', 'Agentic.Settings.maxTokens', 'Agentic.Settings.key',
-- 'Agentic.Settings.endpoint', 'Agentic.Settings.timeout') and @fallbacks@.
data Anthropic = Anthropic
  { model :: Text
  , system :: Maybe Text
    -- ^ A system prompt for every @draft@ in the runtime.
  , maxTokens :: Int
  , effort :: Maybe Effort
    -- ^ The model's default if unset.
  , fallbacks :: Bool
    -- ^ Let the API retry a refused request on a fallback model it picks.
  , key :: Maybe Text
    -- ^ Defaults to the @ANTHROPIC_API_KEY@ environment variable.
  , endpoint :: String
  , timeout :: Int
    -- ^ Seconds.
  }

anthropic :: Anthropic
anthropic =
  Anthropic
    { model = "claude-opus-5-5"
    , system = Nothing
    , maxTokens = 16000
    , effort = Nothing
    , fallbacks = True
    , key = Nothing
    , endpoint = "https://api.anthropic.com/v1/messages"
    , timeout = 600
    }

instance HasModel Anthropic where model m c = c {model = m}
instance HasSystem Anthropic where system t c = c {system = Just t}
instance HasMaxTokens Anthropic where maxTokens n c = c {maxTokens = n}
instance HasEffort Anthropic where effort e c = c {effort = Just e}
instance HasKey Anthropic where key k c = c {key = Just k}
instance HasEndpoint Anthropic where endpoint e c = c {endpoint = e}
instance HasTimeout Anthropic where timeout t c = c {timeout = t}

-- | Whether the API may retry a refused request on a fallback model. On by
-- default.
fallbacks :: Bool -> Anthropic -> Anthropic
fallbacks on c = c {fallbacks = on}

effortName :: Effort -> Text
effortName = \case
  Low -> "low"
  Medium -> "medium"
  High -> "high"
  XHigh -> "xhigh"
  Max -> "max"

-- | What can go wrong talking to the Messages API. Thrown in IO.
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
    MissingKey -> "Anthropic: no API key. Set ANTHROPIC_API_KEY, or use (anthropic & key ...)."
    HttpError status body -> "Anthropic rejected the request (HTTP " <> show status <> "): " <> T.unpack body
    Refused category -> "Claude declined the request" <> maybe "" (\c -> " (" <> T.unpack c <> ")") category
    Truncated -> "Claude's reply hit the token limit; raise it with (anthropic & maxTokens ...)"
    UnexpectedStop reason -> "Claude stopped for an unexpected reason: " <> T.unpack reason
    UnexpectedResponse problem -> "Anthropic sent a response agentic can't read: " <> T.unpack problem

instance ProvidesSystemTwo Anthropic where
  toSystemTwo cfg = do
    key' <- maybe (fmap T.pack <$> lookupEnv "ANTHROPIC_API_KEY") (pure . Just) cfg.key >>= maybe (throwIO MissingKey) pure
    manager <- newTlsManager
    base <- Http.parseRequest cfg.endpoint
    pure $ SystemTwo $ \conversation -> do
      let http =
            base
              { Http.method = "POST"
              , Http.requestHeaders =
                  [ ("x-api-key", T.encodeUtf8 key')
                  , ("anthropic-version", "2023-06-01")
                  , ("content-type", "application/json")
                  ]
                    <> [("anthropic-beta", "server-side-fallback-2026-07-01") | cfg.fallbacks]
              , Http.requestBody = Http.RequestBodyLBS (TL.encodeUtf8 (TL.fromStrict (A.renderJson (requestBody cfg conversation))))
              , Http.responseTimeout = Http.responseTimeoutMicro (cfg.timeout * 1000000)
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
    [ ("model", A.String cfg.model)
    , ("max_tokens", A.Integer (toInteger cfg.maxTokens))
    ]
      <> maybe [] (\s -> [("system", A.String s)]) cfg.system
      <> [("tools", A.Array (map tool (tools c))) | not (null (tools c))]
      <> [ ("messages", A.Array (task : concatMap exchange (history c)))
         , ( "output_config"
           , A.Object
               ( ("format", A.Object [("type", A.String "json_schema"), ("schema", objectSchema (output c))])
                   : maybe [] (\e -> [("effort", A.String (effortName e))]) cfg.effort
               )
           )
         , ("cache_control", A.Object [("type", A.String "ephemeral")])
         ]
      <> [("fallbacks", A.String "default") | cfg.fallbacks]
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
