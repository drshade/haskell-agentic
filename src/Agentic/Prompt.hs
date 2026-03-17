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
  , AgentEvents, SchemaFormatTag(..)
  , call
  )
import Agentic.Error (SchemaError)
import Agentic.Retry (withRetry)
import Agentic.Schema (SchemaFormat(..))
import Agentic.Schema.Dhall
  ( parseDhall, dhallSystemPrompt, dhallRetryPrompt
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
import Data.Text (Text, pack)
import Data.Typeable (Typeable, typeRep)
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
  , Typeable a
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
  , Typeable a
  , LLM :> es
  , IOE :> es
  , AgentEvents :> es
  , Error SchemaError :> es
  )
  => Text -> Eff es a
extractDhall userMsg = do
  let typeName = (pack . show . typeRep) (Proxy @a)
      schemaPrompt = injectDhallSchema (Proxy @a) userMsg
      req = LLMRequest
        { messages     = [LLMMessage { role = "user", content = schemaPrompt }]
        , tools        = []
        , systemPrompt = Just dhallSystemPrompt
        }
  withRetry 3 req DhallFormat typeName dhallRetryPrompt $ \responseText ->
    liftIO $ parseDhall @a responseText

-- | Extract using JSON Schema explicitly.
extractJson :: forall a es.
  ( FromJSON a, ToJSON a, HasCodec a
  , Typeable a
  , LLM :> es
  , AgentEvents :> es
  , Error SchemaError :> es
  )
  => Text -> Eff es a
extractJson userMsg = do
  let typeName = (pack . show . typeRep) (Proxy @a)
      schemaPrompt = injectJsonSchema @a userMsg
      req = LLMRequest
        { messages     = [LLMMessage { role = "user", content = schemaPrompt }]
        , tools        = []
        , systemPrompt = Nothing
        }
      jsonRetryPrompt :: Text -> Text -> Text -> Text
      jsonRetryPrompt err badReply originalInstruction =
        "Your last reply failed JSON parsing with the following error:\n" <> err
        <> "\n\nYour response was:\n" <> badReply
        <> "\n\nThe instruction you were given was:\n" <> originalInstruction
        <> "\n\nPlease fix the problem and respond with valid JSON"
  withRetry 3 req JsonFormat typeName jsonRetryPrompt $ \responseText ->
    pure $ parseJson @a responseText

-- | Extract using an explicit SchemaFormat value.
extractWith :: forall a es.
  ( Typeable a
  , LLM :> es
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
