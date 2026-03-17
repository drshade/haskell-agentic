module Agentic.Tools
  ( Tool(..)
  , ToolName(..)
  , mkTool
  , dispatchTool
  ) where

import Agentic.Error (SchemaError, ToolError(..))
import Agentic.Schema.Dhall (dhallSchemaOf, parseDhall)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Dhall
import qualified Dhall.Core
import Dhall (FromDhall, ToDhall)
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Error.Static (Error, throwError)
import GHC.Generics (Generic)

newtype ToolName = ToolName Text
  deriving (Eq, Ord, Show, Generic)

-- | A fully typed tool. The existential hides the concrete input/output types
-- but the GADT captures their Dhall constraints.
data Tool es where
  MkTool
    :: (FromDhall i, ToDhall i, ToDhall o, FromDhall o)
    => { toolName        :: ToolName
       , toolDescription :: Text
       , toolInputSchema :: Text         -- ^ Dhall schema for the input type, derived at construction
       , toolExecute     :: i -> Eff es o
       }
    -> Tool es

-- | Smart constructor: derives the Dhall input schema from the type at construction time.
mkTool
  :: forall i o es.
     (FromDhall i, ToDhall i, ToDhall o, FromDhall o)
  => ToolName
  -> Text
  -> (i -> Eff es o)
  -> Tool es
mkTool name desc execute = MkTool
  { toolName        = name
  , toolDescription = desc
  , toolInputSchema = dhallSchemaOf (Proxy @i)
  , toolExecute     = execute
  }

-- | Dispatch a tool call: find tool by name, decode input, execute, encode output.
-- Returns the Dhall-encoded output as Text.
dispatchTool
  :: ( Error (ToolError SchemaError) :> es
     , IOE :> es
     )
  => [Tool es]
  -> Text          -- ^ Tool name
  -> Text          -- ^ Raw Dhall-encoded input
  -> Eff es Text   -- ^ Dhall-encoded output
dispatchTool tools nameText rawInput =
  case filter matchName tools of
    [] -> throwError (ToolNotFound nameText :: ToolError SchemaError)
    (MkTool { toolExecute } : _) -> do
      inputResult <- liftIO $ parseDhall rawInput
      input <- case inputResult of
        Left err    -> throwError (ToolInputDecodeFailed err :: ToolError SchemaError)
        Right value -> pure value
      output <- toolExecute input
      pure $ Dhall.Core.pretty $ Dhall.embed Dhall.inject output
  where
    matchName :: Tool es -> Bool
    matchName (MkTool { toolName = ToolName n }) = n == nameText
