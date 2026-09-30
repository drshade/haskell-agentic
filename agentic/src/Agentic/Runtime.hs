-- | The runtime: the one connection between a flow and the outside world.
module Agentic.Runtime
  ( -- * Runtime
    Runtime (..)
  , runtime
  , runtimeWith
  , SystemOne (..)
  , SystemTwo (..)
  , ProvidesSystemOne (..)
  , ProvidesSystemTwo (..)
  , withSystemOne
  , withSystemTwo
    -- * Modifiers
  , observing
  , capped
    -- * System Two turns
  , Conversation (..)
  , ToolSpec (..)
  , Exchange (..)
  , ToolResult (..)
  , Turn (..)
  , Raw (..)
  , Action (..)
  , ToolCall (..)
    -- * Events and errors
  , Event (..)
  , Happened (..)
  , FlowError (..)
  ) where

import Agentic.Core (Instruction, Note)
import Agentic.Questions (Answer, JudgeRequest)
import Agentic.Schema (Schema)
import Agentic.Value (Value (..))
import Control.Exception (Exception, throwIO)
import Data.Text (Text)

-- ---------------------------------------------------------------------------
-- System Two turns

-- | Everything a System Two provider needs to take one turn of a step.
data Conversation = Conversation
  { path :: [Note]
    -- ^ Where this step is in the flow.
  , instruction :: Instruction
  , state :: Value
    -- ^ The step's input, encoded by its contract.
  , stateSchema :: Schema
  , tools :: [ToolSpec]
  , output :: Schema
    -- ^ The schema of the step's result.
  , history :: [Exchange]
    -- ^ Earlier turns of this step, oldest first. Append-only.
  }
  deriving (Eq, Show)

data ToolSpec = ToolSpec
  { specName :: Text
  , specDescription :: Text
  , specInput :: Schema
  }
  deriving (Eq, Show)

data Exchange
  = Called Raw [(Text, ToolResult)]
    -- ^ The model's turn, and the result of each tool call by call id.
  | Rejected Raw Text
    -- ^ The model's final value failed its contract's checks.
  deriving (Eq, Show)

data ToolResult = ToolOk Value | ToolFailed Text
  deriving (Eq, Show)

data Turn = Turn
  { raw :: Raw
  , action :: Action
  }
  deriving (Eq, Show)

-- | A provider's own message for a turn. The core stores it and hands it back
-- unchanged; only the provider looks inside.
newtype Raw = Raw Value
  deriving (Eq, Show)

data Action
  = CallTools [ToolCall]
  | Respond Value
  deriving (Eq, Show)

data ToolCall = ToolCall
  { callId :: Text
  , callName :: Text
  , callInput :: Value
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Events and errors

data Event = Event
  { eventPath :: [Note]
  , happened :: Happened
  }
  deriving (Show)

data Happened
  = Drafting Conversation
  | Turned Turn
  | ToolCalled ToolCall
  | ToolReturned Text ToolResult
  | OutputRejected Text
  | Judged JudgeRequest [Answer]
  | Acted
  deriving (Show)

-- | Errors the core raises itself. Provider errors are the provider's own.
data FlowError
  = NoSystemOne
  | NoSystemTwo
  | MalformedAnswers Text
  | TurnLimit Int
  deriving (Eq, Show)

instance Exception FlowError

-- ---------------------------------------------------------------------------
-- Runtime

-- | Fast, typed judgements (Jev, or an LLM standing in).
newtype SystemOne m = SystemOne {askSystemOne :: JudgeRequest -> m [Answer]}

-- | One LLM turn.
newtype SystemTwo m = SystemTwo {askSystemTwo :: Conversation -> m Turn}

data Runtime m = Runtime
  { systemOne :: SystemOne m
  , systemTwo :: SystemTwo m
  , parallel :: forall a. [m a] -> m [a]
    -- ^ Runs independent work: 'Agentic.Core.each', '&&&', parallel tool calls.
  , observe :: Event -> m ()
  , failure :: forall a. FlowError -> m a
  }

-- | A runtime in IO with no providers: it runs things one after another,
-- observes nothing, and throws 'FlowError's.
runtime :: Runtime IO
runtime = runtimeWith throwIO

-- | A runtime in any monad, given how it raises errors.
runtimeWith :: Monad m => (forall a. FlowError -> m a) -> Runtime m
runtimeWith raise =
  Runtime
    { systemOne = SystemOne (const (raise NoSystemOne))
    , systemTwo = SystemTwo (const (raise NoSystemTwo))
    , parallel = sequence
    , observe = const (pure ())
    , failure = raise
    }

class ProvidesSystemOne p where
  toSystemOne :: p -> IO (SystemOne IO)

class ProvidesSystemTwo p where
  toSystemTwo :: p -> IO (SystemTwo IO)

withSystemOne :: ProvidesSystemOne p => p -> Runtime IO -> IO (Runtime IO)
withSystemOne p rt = (\s -> rt {systemOne = s}) <$> toSystemOne p

withSystemTwo :: ProvidesSystemTwo p => p -> Runtime IO -> IO (Runtime IO)
withSystemTwo p rt = (\s -> rt {systemTwo = s}) <$> toSystemTwo p

-- ---------------------------------------------------------------------------
-- Modifiers

-- | Also send every event to @f@.
observing :: Applicative m => (Event -> m ()) -> Runtime m -> Runtime m
observing f rt = rt {observe = \e -> observe rt e *> f e}

-- | Fail a step that takes more than @n@ turns.
capped :: Int -> Runtime m -> Runtime m
capped n rt = rt {systemTwo = SystemTwo turn}
  where
    turn c
      | length (history c) >= n = failure rt (TurnLimit n)
      | otherwise = askSystemTwo (systemTwo rt) c
