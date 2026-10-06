-- | The runtime: the one connection between a flow and the outside world.
module Agentic.Runtime
  ( -- * Runtime
    Runtime (..)
  , inParallel
  , failWith
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
  , input :: Value
    -- ^ The step's input, encoded by its contract.
  , inputSchema :: Schema
  , tools :: [ToolSpec]
  , outputSchema :: Schema
    -- ^ The schema of the step's result.
  , history :: [Exchange]
    -- ^ Earlier turns of this step, oldest first. Append-only.
  }
  deriving (Eq, Show)

data ToolSpec = ToolSpec
  { name :: Text
  , description :: Text
  , input :: Schema
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
  , name :: Text
  , input :: Value
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Events and errors

data Event = Event
  { path :: [Note]
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
newtype SystemOne m = SystemOne {ask :: JudgeRequest -> m [Answer]}

-- | One LLM turn.
newtype SystemTwo m = SystemTwo {ask :: Conversation -> m Turn}

-- | Everything a flow needs from the outside world: its two kinds of model,
-- how to run independent work, where events go, and how to raise errors.
data Runtime m = Runtime
  { systemOne :: SystemOne m
  , systemTwo :: SystemTwo m
  , parallel :: forall a. [m a] -> m [a]
    -- ^ Runs independent work: 'Agentic.Core.each', 'Control.Arrow.&&&', parallel tool calls.
  , observe :: Event -> m ()
  , failure :: forall a. FlowError -> m a
  }

-- | Run work with a runtime's @parallel@. (Record dot can't select a
-- polymorphic field, so this and 'failWith' are functions.)
inParallel :: Runtime m -> [m a] -> m [a]
inParallel Runtime {parallel = p} = p

-- | Fail with a runtime's @failure@.
failWith :: Runtime m -> FlowError -> m a
failWith Runtime {failure = f} = f

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
observing f rt = rt {observe = \e -> rt.observe e *> f e}

-- | Fail a step that takes more than @n@ turns.
capped :: Int -> Runtime m -> Runtime m
capped n rt = rt {systemTwo = SystemTwo turn}
  where
    turn c
      | length c.history >= n = failWith rt (TurnLimit n)
      | otherwise = rt.systemTwo.ask c
