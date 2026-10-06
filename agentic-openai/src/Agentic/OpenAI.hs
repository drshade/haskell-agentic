-- | OpenAI as a runtime's System Two (and, through 'viaLLM', System One), over
-- the Responses API.
--
-- > rt <- pure runtime >>= withSystemTwo openai
-- > rt <- pure runtime >>= withSystemTwo (openai & model "gpt-6.1-sol" & effort Low)
module Agentic.OpenAI
  ( OpenAI (..)
  , openai
  , OpenAIError (..)
    -- * Wire format
  , requestBody
  , decodeTurn
  ) where

import Agentic.Aeson (fromAeson)
import Agentic.Core (Instruction (..))
import Agentic.JsonSchema (objectSchema, schemaName, unwrap)
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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Environment (lookupEnv)

-- | OpenAI's settings. Start from 'openai' and change them with the setters
-- from "Agentic.Settings": 'Agentic.Settings.model', 'Agentic.Settings.system',
-- 'Agentic.Settings.effort', 'Agentic.Settings.maxTokens', 'Agentic.Settings.key',
-- 'Agentic.Settings.endpoint' and 'Agentic.Settings.timeout'.
data OpenAI = OpenAI
  { model :: Text
  , system :: Maybe Text
    -- ^ Sent as the request's instructions.
  , maxTokens :: Maybe Int
  , effort :: Maybe Effort
    -- ^ The model's default if unset.
  , key :: Maybe Text
    -- ^ Defaults to the @OPENAI_API_KEY@ environment variable.
  , endpoint :: Text
  , timeout :: Int
    -- ^ Seconds.
  }

openai :: OpenAI
openai =
  OpenAI
    { model = "gpt-6-astra"
    , system = Nothing
    , maxTokens = Nothing
    , effort = Nothing
    , key = Nothing
    , endpoint = "https://api.openai.com/v1/responses"
    , timeout = 600
    }

instance HasModel OpenAI where model m c = c {model = m}
instance HasSystem OpenAI where system t c = c {system = Just t}
instance HasMaxTokens OpenAI where maxTokens n c = c {maxTokens = Just n}
instance HasEffort OpenAI where effort e c = c {effort = Just e}
instance HasKey OpenAI where key k c = c {key = Just k}
instance HasEndpoint OpenAI where endpoint e c = c {endpoint = e}
instance HasTimeout OpenAI where timeout t c = c {timeout = t}

effortName :: Effort -> Text
effortName = \case
  Low -> "low"
  Medium -> "medium"
  High -> "high"
  XHigh -> "xhigh"
  Max -> "max"

-- | What can go wrong talking to the Responses API. Thrown in IO.
data OpenAIError
  = MissingKey
  | HttpError Int Text
    -- ^ A non-200 status, and the API's error body.
  | Refused Text
    -- ^ The model refused, with its explanation.
  | Truncated
    -- ^ The reply hit the token limit before it was complete.
  | Incomplete Text
    -- ^ The response stopped early for another reason.
  | UnexpectedResponse Text
  deriving (Show)

instance Exception OpenAIError where
  displayException = \case
    MissingKey -> "OpenAI: no API key. Set OPENAI_API_KEY, or use (openai & key ...)."
    HttpError status body -> "OpenAI rejected the request (HTTP " <> show status <> "): " <> T.unpack body
    Refused why -> "The model refused: " <> T.unpack why
    Truncated -> "The reply hit the token limit; raise it with (openai & maxTokens ...)"
    Incomplete reason -> "The response stopped early: " <> T.unpack reason
    UnexpectedResponse problem -> "OpenAI sent a response agentic can't read: " <> T.unpack problem

instance ProvidesSystemTwo OpenAI where
  toSystemTwo cfg = do
    key' <- maybe (fmap T.pack <$> lookupEnv "OPENAI_API_KEY") (pure . Just) cfg.key >>= maybe (throwIO MissingKey) pure
    manager <- newTlsManager
    base <- Http.parseRequest (T.unpack cfg.endpoint)
    pure $ SystemTwo $ \conversation -> do
      let http =
            base
              { Http.method = "POST"
              , Http.requestHeaders =
                  [ ("Authorization", "Bearer " <> T.encodeUtf8 key')
                  , ("Content-Type", "application/json")
                  ]
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

-- | The model answers judgements too, with uncalibrated probabilities.
instance ProvidesSystemOne OpenAI where
  toSystemOne cfg = viaLLM <$> toSystemTwo cfg

-- | The Responses API request for one turn of a step. Nothing is stored on
-- OpenAI's side: each turn sends the whole step so far, and reasoning comes
-- back encrypted so it can be sent back unchanged.
requestBody :: OpenAI -> Conversation -> A.Value
requestBody cfg c =
  A.Object $
    [("model", A.String cfg.model)]
      <> maybe [] (\s -> [("instructions", A.String s)]) cfg.system
      <> [("tools", A.Array (map tool (tools c))) | not (null (tools c))]
      <> [ ("input", A.Array (task : concatMap exchange (history c)))
         , ( "text"
           , A.Object
               [ ( "format"
                 , A.Object
                     [ ("type", A.String "json_schema")
                     , ("name", A.String (schemaName (outputSchema c)))
                     , ("schema", objectSchema (outputSchema c))
                     , ("strict", A.Bool True)
                     ]
                 )
               ]
           )
         , ("store", A.Bool False)
         , ("include", A.Array [A.String "reasoning.encrypted_content"])
         ]
      <> maybe [] (\e -> [("reasoning", A.Object [("effort", A.String (effortName e))])]) cfg.effort
      <> maybe [] (\n -> [("max_output_tokens", A.Integer (toInteger n))]) cfg.maxTokens
  where
    task = message "user" (instructionText (instruction c) <> inputText)
    inputText = case input c of
      A.Null -> ""
      s -> "\n\nInput:\n" <> A.renderJson s
    tool spec =
      A.Object
        [ ("type", A.String "function")
        , ("name", A.String (specName spec))
        , ("description", A.String (specDescription spec))
        , ("parameters", objectSchema (specInput spec))
        , ("strict", A.Bool True)
        ]
    -- A turn's raw value is its list of output items, which go back as input.
    items (Raw raw) = case raw of
      A.Array xs -> xs
      other -> [other]
    exchange = \case
      Called raw results -> items raw <> map result results
      Rejected raw problem -> items raw <> [message "user" ("That answer was rejected: " <> problem <> ". Please answer again.")]
    result (callId', r) =
      A.Object
        [ ("type", A.String "function_call_output")
        , ("call_id", A.String callId')
        , ("output", A.String (case r of ToolOk v -> asText v; ToolFailed problem -> "Error: " <> problem))
        ]
    asText = \case
      A.String t -> t
      v -> A.renderJson v
    message :: Text -> Text -> A.Value
    message role content = A.Object [("role", A.String role), ("content", A.String content)]

-- | Read one turn from a Responses API response.
decodeTurn :: Conversation -> J.Value -> Either OpenAIError Turn
decodeTurn c = either (Left . UnexpectedResponse . T.pack) id . J.parseEither parse
  where
    parse = J.withObject "response" $ \r -> do
      status <- r .: "status"
      details <- r .:? "incomplete_details"
      reason <- maybe (pure Nothing) (J.withObject "incomplete_details" (.:? "reason")) details
      outputs <- r .: "output" :: J.Parser [J.Value]
      -- Message parts are flattened into the item list.
      parsed <- concatMap (\case Message parts -> parts; x -> [x]) <$> traverse item outputs
      let raw = Raw (fromAeson (J.toJSON outputs))
          calls = [ToolCall i n (unwrapInput n v) | Call i n v <- parsed]
          text = T.concat [t | Text t <- parsed]
          refusals = [t | Refusal t <- parsed]
      pure $ case (status :: Text, reason :: Maybe Text) of
        (_, _) | why : _ <- refusals -> Left (Refused why)
        ("incomplete", Just "max_output_tokens") -> Left Truncated
        ("incomplete", Just "content_filter") -> Left (Refused "content filter")
        ("incomplete", other) -> Left (Incomplete (maybe "unknown" id other))
        ("completed", _)
          | not (null calls) -> Right (Turn raw (CallTools calls))
          | otherwise -> Right (Turn raw (Respond (final text)))
        (other, _) -> Left (Incomplete other)
    item = J.withObject "item" $ \o -> do
      kind <- o .: "type"
      case kind :: Text of
        "function_call" -> do
          arguments <- o .: "arguments"
          Call <$> o .: "call_id" <*> o .: "name" <*> pure (json arguments)
        "message" -> do
          content <- o .: "content"
          Message <$> traverse part content
        _ -> pure Other
    part = J.withObject "part" $ \p -> do
      kind <- p .: "type"
      case kind :: Text of
        "output_text" -> Text <$> p .: "text"
        "refusal" -> Refusal <$> p .: "refusal"
        _ -> pure Other
    -- A reply that isn't JSON goes back to the core as text; the output
    -- contract then rejects it and the model gets another go.
    final text = case decodeJson text of
      Just v -> unwrap (outputSchema c) v
      Nothing -> A.String text
    json t = maybe (A.String t) id (decodeJson t)
    decodeJson t = fromAeson <$> J.decode (TL.encodeUtf8 (TL.fromStrict t))
    unwrapInput name v = maybe v (`unwrap` v) (inputSchema name)
    inputSchema :: Text -> Maybe Schema
    inputSchema name = case [specInput s | s <- tools c, specName s == name] of
      s : _ -> Just s
      [] -> Nothing

data Item = Call Text Text A.Value | Message [Item] | Text Text | Refusal Text | Other
