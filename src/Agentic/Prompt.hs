module Agentic.Prompt
  ( prompt
  , extract
  , extractDhall
  , extractJson
  , extractWith
  , inject
  , injectJson
  ) where

import Agentic.Effects
  ( LLM, LLMMessage(..), LLMRequest(..), LLMResponse(..)
  , AgentEvents
  , call
  )
import Agentic.Effects (SchemaFormatTag(..))
import Agentic.Error (SchemaError)
import Agentic.Retry (withRetry)
import Agentic.Schema (SchemaFormat(..))
import Agentic.Schema.Dhall
  ( parseDhall, dhallSystemPrompt
  , injectDhallSchema, injectDhallObject
  )
import Agentic.Schema.Json
  ( parseJson
  , injectJsonSchema, injectJsonObject
  )
import Autodocodec (HasCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Dhall (FromDhall, ToDhall)
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Error.Static (Error)

-- | Send a plain text prompt to the LLM and return the raw response text.
-- Does not inject any schema — use this when you want the raw LLM output.
prompt :: LLM :> es => Text -> Eff es Text
prompt userMsg = do
  let req = LLMRequest
        { messages     = [LLMMessage { role = "user", content = userMsg }]
        , tools        = []
        , systemPrompt = Nothing
        }
  resp <- call req
  let LLMResponse { llmContent } = resp
  pure $ fromMaybe "" llmContent

-- | Extract a typed Dhall value from an LLM response (default schema: Dhall).
-- Retries up to 3 times on parse failure, feeding the error back to the LLM.
extract :: forall a es.
  ( FromDhall a, ToDhall a
  , LLM :> es
  , IOE :> es
  , AgentEvents :> es
  , Error SchemaError :> es
  )
  => Text -> Eff es a
extract = extractDhall

-- | Extract using Dhall schema explicitly.
extractDhall :: forall a es.
  ( FromDhall a, ToDhall a
  , LLM :> es
  , IOE :> es
  , AgentEvents :> es
  , Error SchemaError :> es
  )
  => Text -> Eff es a
extractDhall userMsg = do
  let schemaPrompt = injectDhallSchema (Proxy @a) userMsg
      req = LLMRequest
        { messages     = [LLMMessage { role = "user", content = schemaPrompt }]
        , tools        = []
        , systemPrompt = Just dhallSystemPrompt
        }
  withRetry 3 req DhallFormat "unknown" $ \responseText ->
    liftIO $ parseDhall @a responseText

-- | Extract using JSON Schema explicitly.
extractJson :: forall a es.
  ( FromJSON a, ToJSON a, HasCodec a
  , LLM :> es
  , AgentEvents :> es
  , Error SchemaError :> es
  )
  => Text -> Eff es a
extractJson userMsg = do
  let schemaPrompt = injectJsonSchema @a userMsg
      req = LLMRequest
        { messages     = [LLMMessage { role = "user", content = schemaPrompt }]
        , tools        = []
        , systemPrompt = Nothing
        }
  withRetry 3 req JsonFormat "unknown" $ \responseText ->
    pure $ parseJson @a responseText

-- | Extract using an explicit SchemaFormat value.
extractWith :: forall a es.
  ( LLM :> es
  , IOE :> es
  , AgentEvents :> es
  , Error SchemaError :> es
  )
  => SchemaFormat a -> Text -> Eff es a
extractWith Dhall msg = extractDhall msg
extractWith Json  msg = extractJson  msg

-- | Inject a Dhall-serialisable value into a prompt string.
inject :: forall a. ToDhall a => a -> Text -> Text
inject = injectDhallObject

-- | Inject a JSON-serialisable value into a prompt string.
injectJson :: forall a. (ToJSON a, HasCodec a) => a -> Text -> Text
injectJson = injectJsonObject
