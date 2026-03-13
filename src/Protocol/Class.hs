-- | Typeclass interface for pluggable schema formats.
--
-- A 'SchemaFormat' determines how agent inputs and outputs are encoded,
-- decoded, and described to the LLM. Two instances are provided:
-- 'Protocol.DhallSchema.Marshal.Dhall' and 'Protocol.JSONSchema.Marshal.Json'.
module Protocol.Class where

import           Agentic                (Agentic, AgenticRWS, Prompt (..),
                                         orFail, pattern Agentic, runLLM)
import           Agentic.Error          (AgenticError)
import           Control.Arrow          ((>>>))
import           Control.Monad.IO.Class (MonadIO)
import           Data.Data              (Proxy (Proxy))
import           Data.Kind              (Constraint)
import           Data.Text              hiding (show)

-- | Typeclass for schema format backends.
-- Each instance defines how to describe, parse, and inject values for the LLM.
class SchemaFormat fmt where
    -- | The constraint required on a type to use this format.
    type SchemaConstraint fmt a :: Constraint
    -- | Produce the system prompt that instructs the LLM to use this format.
    systemPrompt :: forall a. SchemaConstraint fmt a => Proxy a -> Text
    -- | Render the schema for type @a@ as text.
    schemaOf :: forall a. SchemaConstraint fmt a => Proxy a -> Text
    -- | Parse an LLM response into a Haskell value, or return an error.
    parseWithSchema :: forall a m. MonadIO m => SchemaConstraint fmt a => Proxy a -> Text -> m (Either AgenticError a)
    -- | Inject a schema description into a user prompt.
    injectSchema :: forall a. SchemaConstraint fmt a => Proxy a -> Text -> Text
    -- | Inject a serialised value into a user prompt.
    injectObject :: SchemaConstraint fmt a => a -> Text -> Text

-- | Extract a typed value from an LLM response using the given schema format.
-- Uses a @Proxy@ to select the format; prefer 'extractWith' for type-application syntax.
extractWithProxy :: forall fmt s m. (SchemaFormat fmt, SchemaConstraint fmt s, AgenticRWS m) => Proxy fmt -> Agentic m Prompt s
extractWithProxy _ = injectSchemaWithProxy >>> runLLM >>> parseWithProxy >>> orFail
    where
      injectSchemaWithProxy :: Agentic m Prompt Prompt
      injectSchemaWithProxy = Agentic $ \(Prompt _system user) ->
          pure $ Prompt (systemPrompt @fmt (Proxy @s)) $ injectSchema @fmt (Proxy @s) user

      parseWithProxy :: Agentic m Text (Either AgenticError s)
      parseWithProxy = Agentic $ \input -> parseWithSchema @fmt (Proxy @s) input

-- | Inject a typed value into a prompt using the given schema format.
-- Uses a @Proxy@ to select the format; prefer 'injectWith' for type-application syntax.
injectWithProxy :: forall fmt s m. (SchemaFormat fmt, SchemaConstraint fmt s) => Proxy fmt -> s -> Agentic m Prompt Prompt
injectWithProxy _ obj = Agentic $ \(Prompt _system user) ->
    pure $ Prompt _system (injectObject @fmt obj user)

-- | Extract a typed value from an LLM response using a schema format selected
-- by type application, e.g. @extractWith \@Dhall \@MyType@.
extractWith :: forall fmt s m. (SchemaFormat fmt, SchemaConstraint fmt s, AgenticRWS m) => Agentic m Prompt s
extractWith = extractWithProxy (Proxy @fmt)

-- | Inject a typed value into a prompt using a schema format selected
-- by type application, e.g. @injectWith \@Dhall myValue@.
injectWith :: forall fmt s m. (SchemaFormat fmt, SchemaConstraint fmt s) => s -> Agentic m Prompt Prompt
injectWith = injectWithProxy (Proxy @fmt)
