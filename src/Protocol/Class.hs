
module Protocol.Class where

import           Agentic                (Agentic, AgenticRWS, Prompt (..),
                                         orFail, pattern Agentic, runLLM)
import           Control.Arrow          ((>>>))
import           Control.Monad.IO.Class (MonadIO)
import           Data.Data              (Proxy (Proxy))
import           Data.Kind              (Constraint)
import           Data.Text              hiding (show)

class SchemaFormat fmt where
    type SchemaConstraint fmt a :: Constraint
    systemPrompt :: forall a. SchemaConstraint fmt a => Proxy a -> Text
    schemaOf :: forall a. SchemaConstraint fmt a => Proxy a -> Text
    parseWithSchema :: forall a m. MonadIO m => SchemaConstraint fmt a => Proxy a -> Text -> m (Either Text a)
    injectSchema :: forall a. SchemaConstraint fmt a => Proxy a -> Text -> Text
    injectObject :: SchemaConstraint fmt a => a -> Text -> Text

extractWithProxy :: forall fmt s m. (SchemaFormat fmt, SchemaConstraint fmt s, AgenticRWS m) => Proxy fmt -> Agentic m Prompt s
extractWithProxy _ = injectSchemaWithProxy >>> runLLM >>> parseWithProxy >>> orFail
    where
      injectSchemaWithProxy :: Agentic m Prompt Prompt
      injectSchemaWithProxy = Agentic $ \(Prompt _system user) ->
          pure $ Prompt (systemPrompt @fmt (Proxy @s)) $ injectSchema @fmt (Proxy @s) user

      parseWithProxy :: Agentic m Text (Either Text s)
      parseWithProxy = Agentic $ \input -> parseWithSchema @fmt (Proxy @s) input

injectWithProxy :: forall fmt s m. (SchemaFormat fmt, SchemaConstraint fmt s) => Proxy fmt -> s -> Agentic m Prompt Prompt
injectWithProxy _ obj = Agentic $ \(Prompt _system user) ->
    pure $ Prompt _system (injectObject @fmt obj user)

extractWith :: forall fmt s m. (SchemaFormat fmt, SchemaConstraint fmt s, AgenticRWS m) => Agentic m Prompt s
extractWith = extractWithProxy (Proxy @fmt)

injectWith :: forall fmt s m. (SchemaFormat fmt, SchemaConstraint fmt s) => s -> Agentic m Prompt Prompt
injectWith = injectWithProxy (Proxy @fmt)
