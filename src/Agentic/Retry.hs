module Agentic.Retry
  ( withRetry
  ) where

import Agentic.Effects
  ( LLM, LLMMessage(..), LLMRequest(..), LLMResponse(..)
  , AgentEvents, Event(..), SchemaFormatTag(..)
  , call, emit
  )
import Agentic.Error (SchemaError(..))
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import GHC.Natural (Natural)

-- | Run an LLM request, parse the response, and retry on parse failure.
-- On each retry the parse error and the LLM's bad response are appended to
-- the conversation so the LLM can self-correct.
withRetry
  :: ( LLM :> es
     , AgentEvents :> es
     , Error SchemaError :> es
     )
  => Natural                                      -- ^ Maximum attempts (e.g. 3)
  -> LLMRequest                                   -- ^ Initial request
  -> SchemaFormatTag                              -- ^ For observability events
  -> Text                                         -- ^ Type name (for observability)
  -> (Text -> Text -> Text -> Text)               -- ^ Retry prompt builder: err -> badReply -> originalInstruction -> retryMsg
  -> (Text -> Eff es (Either SchemaError a))      -- ^ Parser: responseText -> Either error value
  -> Eff es a
withRetry maxAttempts initialReq fmtTag typeName retryMsgBuilder parser =
  go 1 initialReq
  where
    go attempt req
      | attempt > maxAttempts = do
          let err = DhallParseError "extraction failed after max retries"
          emit $ RetryAttempt { attempt, maxAttempts, reason = err }
          throwError err
      | otherwise = do
          emit $ SchemaExtractAttempt { attempt, schemaFormat = fmtTag, targetType = typeName }
          resp <- call req
          case resp.llmContent of
            Nothing -> do
              let err = DhallParseError "LLM returned no content"
              throwError err
            Just responseText -> do
              result <- parser responseText
              case result of
                Right value -> do
                  emit $ SchemaExtractSuccess { schemaFormat = fmtTag, targetType = typeName }
                  pure value
                Left err -> do
                  emit $ RetryAttempt { attempt, maxAttempts, reason = err }
                  let retryReq = appendRetryContext retryMsgBuilder req responseText err
                  go (attempt + 1) retryReq

-- | Append the error context to the conversation for the next retry attempt.
appendRetryContext :: (Text -> Text -> Text -> Text) -> LLMRequest -> Text -> SchemaError -> LLMRequest
appendRetryContext retryMsgBuilder req badResponse err =
  let originalInstruction = lastUserMsg req.messages
      errorText = renderSchemaError err
      retryMsg = retryMsgBuilder errorText badResponse originalInstruction
  in req { messages = req.messages
             ++ [ LLMMessage { role = "assistant", content = badResponse }
                , LLMMessage { role = "user",      content = retryMsg    }
                ] }

lastUserMsg :: [LLMMessage] -> Text
lastUserMsg msgs =
  case [ m.content | m <- msgs, m.role == "user" ] of
    [] -> ""
    xs -> last xs

renderSchemaError :: SchemaError -> Text
renderSchemaError (DhallParseError t)    = t
renderSchemaError (DhallTypeError  t)    = t
renderSchemaError (JsonParseError p m)   = if p == "" then m else p <> ": " <> m
renderSchemaError (JsonSchemaViolation t) = t
