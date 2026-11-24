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
  , Address
  , addressText
  , Occurrence (..)
  , ContextFrame (..)
  , NodeType (..)
  , Side (..)
  , EdgeId (..)
  , edgeIdText
  , Outcome (..)
  , ActionKind (..)
  , LiveOperation (..)
  , EventPayload (..)
  , Summary (..)
  , summaryValue
  , summarise
  , occurrenceValue
  , Scope (..)
  , scopeWith
  , journalKey
  , ioScope
  , exceptTScope
  , Progress (..)
  , PendingWait (..)
  , ReplyRefused (..)
  , FlowError (..)
  ) where

import Agentic.Journal
import Agentic.Internal.Address
import Agentic.Contract (ProjectionError)
import Agentic.Core (Instruction, Note (..))
import Agentic.Questions (Answer, JudgeRequest)
import Agentic.Schema (Schema)
import Agentic.Value (Value (..))
import Control.Exception (Exception, SomeException, SomeAsyncException, AsyncException (..), catch, fromException, mask, throwIO, try)
import Control.Monad (void)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.List.NonEmpty (NonEmpty)
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

-- | Run-local identity. A recorder supplies the namespace for separate runs,
-- including an @interpret@ nested inside an @act@.
-- Equality uses addresses and the full frame stack, never notes or provider
-- call ids alone. Serialise with 'occurrenceValue', not derived 'Show'.
-- The same occurrence can appear in several resume attempts. Recorders must
-- keep an attempt/recording id to distinguish execution episodes, and the run
-- namespace and flow revision to group them. Neither is inferred from time.
-- Addresses and context remain meaningful within the same flow revision.
data Occurrence = Occurrence
  { address :: !Address
  , context :: ![ContextFrame]
    -- ^ Innermost frame first; pushing a frame shares the enclosing stack.
  } deriving (Eq, Ord, Show)

data ContextFrame
  = EachFrame !Address !Int !Int
    -- ^ Each address, zero-based element index, total elements this attempt.
  | RepeatFrame !Address !Int
    -- ^ Repeat address and zero-based iteration; iterations never share a key.
  | CallFrame !Address !Int !Int !Text
    -- ^ Caller address, zero-based model turn and call position, provider call id.
    -- Reused provider ids remain distinct by turn and position, including recursion.
  deriving (Eq, Ord, Show)

-- | Project every observation frame to its journal identity. Observation
-- occurrences retain element totals; journal keys deliberately do not.
-- Project Executing (KeyedOperation op) and Restored op with the same op: for
-- the same run/revision and logical operation they identify the same completion.
-- A changed Each total can change observation equality without changing that
-- journal key. Do not deduplicate occurrences to infer restored work: plain
-- actions execute again, and retries are new episodes at the same key.
journalKey :: JournalOperation -> Occurrence -> JournalKey
journalKey op occurrence = JournalKey op occurrence.address (map frame occurrence.context)
 where
  frame = \case
    EachFrame a i _ -> ElementFrame a i
    RepeatFrame a i -> IterationFrame a i
    CallFrame a t i c -> InvocationFrame a t i c

data Side = LeftSide | RightSide deriving (Eq, Ord, Show)
data EdgeId = EdgeId !Address !Side deriving (Eq, Ord, Show)

-- | Injective capture-local encoding, identical in observations and graph edges.
edgeIdText :: EdgeId -> Text
edgeIdText (EdgeId a side) = addressText a <> case side of
  LeftSide -> "/select/0"
  RightSide -> "/select/1"

data NodeType
  = RunNode | IdentityNode | PureNode | FirstHalfNode | SecondHalfNode
  | EffectNode | AwaitNode | DraftNode | JudgeNode | SequenceNode | FanoutNode
  | SplitNode | FirstNode | ChoiceNode | EachNode | RepeatNode | NoteNode | CallNode
  deriving (Eq, Ord, Show)

data Outcome = Completed | CallOk | UnknownTool | InvalidInput deriving (Eq, Ord, Show)
data ActionKind = ToolBatch | Response deriving (Eq, Ord, Show)

-- | The operation selected for live execution. KeyedOperation names the
-- operation's journal-key projection even when no journal is installed.
-- KeyedOperation AwaitedReply is representable but never emitted: an await
-- miss suspends instead of invoking an action.
data LiveOperation = PlainAction | KeyedOperation !JournalOperation
  deriving (Eq, Show)

-- | Metadata never contains provider values or diagnostic exceptions.
--
-- Operation grammar within one occurrence in one recording attempt:
--
-- +----------------------+---------------------------------------------------+
-- | Operation            | Classification before its existing result events  |
-- +======================+===================================================+
-- | Plain act            | Executing PlainAction, then callback              |
-- +----------------------+---------------------------------------------------+
-- | Durable / judge /    | Restored op, OR Executing (KeyedOperation op)      |
-- | tool evaluation      | followed by callback                              |
-- +----------------------+---------------------------------------------------+
-- | Draft turn n         | Restored (DraftTurn n), OR Executing keyed turn n; |
-- |                      | then Turned n (unless interrupted or failed)       |
-- +----------------------+---------------------------------------------------+
-- | Await miss           | Neither; then Suspended                           |
-- +----------------------+---------------------------------------------------+
-- | Await hit            | Restored AwaitedReply; then Returned              |
-- +----------------------+---------------------------------------------------+
-- | Empty judge batch    | Neither; then Judged                              |
-- +----------------------+---------------------------------------------------+
--
-- These sequences admit incomplete prefixes. Entered, Drafting and
-- CallResolved do not classify work. A prefix with neither Executing nor
-- Restored is unclassified, not evidence that execution or restoration began.
-- Each operation episode has at most one classification. A retry in a new
-- recording attempt is another episode, even at the same logical journal key.
-- There is no global replay/live phase: branches and turns may interleave.
data Happened
  = Entered !(Maybe Int)
    -- ^ Exact total for Each, including zero; no total for other scopes.
  | Returned !Outcome
    -- ^ Interpreter return only; never forces the result, even to WHNF.
  | Failed
  | Cancelled
  | Suspended
    -- ^ This scope exited with pending waits in this attempt.
  | Selected !EdgeId
  | CallResolved !Address !Address
    -- ^ Binding site and actual body root. Emitted before decoding; invalid
    -- input can therefore resolve a body without entering it.
  | Drafting
  | Turned !Int !ActionKind
  | OutputRejected !Int
  | Executing !LiveOperation
    -- ^ Selected for live execution, not proof of starting, delivery or
    -- completion. Emitted before invoking the callback. Observer failure or
    -- cancellation at this point prevents the callback. ToolExecution covers
    -- evaluation including unknown tools and invalid input, not just body entry.
    -- A known tool emits CallResolved first; an unknown tool cannot resolve.
    -- This event has no rich payload.
  | Restored !JournalOperation
    -- ^ A validated journal hit; a draft identifies each restored turn separately.
  | Judged
    -- ^ Answers available for decoding; an empty batch needs no provider request.
  deriving (Eq, Show)

-- | Optional rich data. Discarding this field must not demand its contents.
data EventPayload
  = DraftPayload Conversation
  | TurnPayload Turn
  | CallPayload ToolCall
  | ResultPayload ToolResult
  | RejectionPayload Text
  | JudgePayload JudgeRequest [Answer]
  | WaitPayload PendingWait
  | ExceptionPayload SomeException
  deriving (Show)

-- | Observers are synchronous but sibling callbacks may overlap. Parent entry
-- completes before child events; child terminals complete before parent exit.
-- A recorder must serialise calls while preserving this causal order.
-- Notification failure (including interruption) can leave an open child beneath
-- a terminated parent. On closing the recording such outcomes are unknown;
-- never invent terminal events. A projection that omits occurrences must retain
-- ancestry explicitly rather than simply filtering event kinds.
data Event = Event
  { occurrence :: Occurrence
  , parent :: Maybe Occurrence
  , nodeType :: NodeType
  , path :: [Note]
  , happened :: Happened
  , payload :: Maybe EventPayload
  } deriving (Show)

data Summary = Summary
  { occurrence :: !Occurrence
  , parent :: !(Maybe Occurrence)
  , nodeType :: !NodeType
  , path :: ![Note]
  , happened :: !Happened
  } deriving (Eq, Show)

-- | Copies metadata only; neither evaluates nor retains payload closures.
summarise :: Event -> Summary
summarise e = forceKey e.occurrence `seq` forceParent e.parent `seq`
  Summary e.occurrence e.parent e.nodeType e.path e.happened
 where
  forceKey (Occurrence (Address segments) frames) = forceList segments `seq` forceList frames
  forceList = foldr (\value rest -> value `seq` rest) ()
  forceParent Nothing = ()
  forceParent (Just k) = forceKey k

-- | Public, lossless JSON-shaped identity encoding. No Show-based identities.
occurrenceValue :: Occurrence -> Value
occurrenceValue (Occurrence a frames) = Object
  [("address", String (addressText a)), ("context", Array (map frame frames))]
 where
  number = Integer . toInteger
  frame = \case
    EachFrame a i n -> Array [String "each", String (addressText a), number i, number n]
    RepeatFrame a i -> Array [String "repeat", String (addressText a), number i]
    CallFrame a t i c -> Array [String "call", String (addressText a), number t, number i, String c]

-- | Portable metadata encoding, with optional payloads already discarded.
-- There is no format-version field. Executing adds an @executing@ event tag;
-- strict consumers must recognise it. Older recordings lack live classification,
-- and absence of a classification must remain unknown, never inferred live.
summaryValue :: Summary -> Value
summaryValue e = Object
  [ ("occurrence", occurrenceValue e.occurrence)
  , ("parent", maybe Null occurrenceValue e.parent)
  , ("node", String (nodeName e.nodeType))
  , ("notes", Array [Object [("name",String n.name),("description",maybe Null String n.description)] | n <- e.path])
  , ("event", eventValue e.happened)
  ]
 where
  number = Integer . toInteger
  eventValue = \case
    Entered total -> Object [("kind",String "entered"),("total",maybe Null number total)]
    Returned outcome -> Object [("kind",String "returned"),("outcome",String (case outcome of
      Completed -> "completed"; CallOk -> "ok"; UnknownTool -> "unknownTool"; InvalidInput -> "invalidInput"))]
    Failed -> Object [("kind",String "failed")]
    Suspended -> Object [("kind",String "suspended")]
    Cancelled -> Object [("kind",String "cancelled")]
    Selected edge -> Object [("kind",String "selected"),("edge",String (edgeIdText edge))]
    CallResolved binding target -> Object [("kind",String "callResolved"),("binding",String (addressText binding)),("body",String (addressText target))]
    Drafting -> Object [("kind",String "drafting")]
    Turned i action -> Object [("kind",String "turned"),("turn",number i),("action",String (case action of ToolBatch -> "tools"; Response -> "response"))]
    OutputRejected i -> Object [("kind",String "outputRejected"),("turn",number i)]
    Executing op -> Object [("kind",String "executing"),("operation",case op of
      PlainAction -> Object [("kind",String "action")]
      KeyedOperation keyed -> Object [("kind",String "keyed"),("operation",operationValue keyed)])]
    Restored op -> Object [("kind",String "restored"),("operation",operationValue op)]
    Judged -> Object [("kind",String "judged")]
  operationValue = \case
    DurableEffect -> String "effect"; ToolExecution -> String "tool"
    Judgement -> String "judge"; AwaitedReply -> String "wait"
    DraftTurn i -> Object [("turn",number i)]
  nodeName = \case
    RunNode -> "run"; IdentityNode -> "identity"; PureNode -> "pure"
    FirstHalfNode -> "firstHalf"; SecondHalfNode -> "secondHalf"; EffectNode -> "effect"
    AwaitNode -> "await"; DraftNode -> "draft"; JudgeNode -> "judge"; SequenceNode -> "sequence"
    FanoutNode -> "fanout"; SplitNode -> "split"; FirstNode -> "first"; ChoiceNode -> "choice"
    EachNode -> "each"; RepeatNode -> "repeat"; NoteNode -> "note"; CallNode -> "call"

-- | A scope hook owns entry, protected execution, and one terminal notification.
-- Entry returns @Right ()@ to proceed or @Left r@ to abort without running the
-- body or a terminal callback. The body returns @Right a@ on success or @Left r@
-- for an explicit failure. On success, call returned once to obtain the final
-- result; on explicit failure, attempt @exited Failed Nothing@ once and return
-- the original @r@. Base-monad exceptional exits are the hook's responsibility.
--
-- Notifications are outside the protected body. Never call both terminal
-- callbacks, retry a callback, or mistake a notification failure for a body
-- failure. Entry and normal-return notification failures propagate; failure
-- reporting must preserve the primary failure against any reporting failure
-- the hook can observe. Do not force @a@ or @r@ beyond the Either discriminator.
-- Lift the hook through each short-circuiting layer whose failures it must see.
-- An interpreter suspension is a normal result at this boundary: the success
-- callback emits Suspended instead of Returned. Its notification failures and
-- cancellation propagate like any other normal-terminal notification.
newtype Scope m = Scope
  { runScope :: forall a r.
      m (Either r ()) ->
      (a -> m r) ->
      (Happened -> Maybe EventPayload -> m ()) ->
      m (Either r a) -> m r
  }

scopeWith :: Scope m -> m (Either r ()) -> (a -> m r) -> (Happened -> Maybe EventPayload -> m ()) -> m (Either r a) -> m r
scopeWith (Scope f) = f

-- | IO observes all exceptional exits. 'SomeAsyncException' means cancellation,
-- except 'StackOverflow' and 'HeapOverflow', which mean failure. Classification
-- uses exception type, not delivery mechanism. Resource exhaustion can itself
-- prevent notification. Notifications run masked (interruptible
-- operations remain interruptible); the body runs in the caller's mask state.
-- Interrupted notification is incomplete delivery: no retry or second terminal.
-- Even cancellation during an exit report yields to the original exception or
-- explicit @Left@ result. Thus a timeout interrupting a blocked failure report
-- can return the original failure (e.g. @Just (Left e)@), rather than @Nothing@.
-- Cancellation during the body or a normal-return report is not suppressed.
ioScope :: Scope IO
ioScope = Scope $ \enter returned exited body -> mask $ \restore -> do
  let report = reportWith exited
  entered <- enter
  case entered of
    Left r -> pure r
    Right () -> do
      result <- try (restore body)
      case result of
        Right (Right a) -> returned a
        Right (Left r) -> report Failed Nothing >> pure r
        Left (e :: SomeException) -> do
          report (classify e) (Just (ExceptionPayload e))
          throwIO e
 where
  reportWith exited happened payload =
    exited happened payload `catch` \(_ :: SomeException) -> pure ()
  classify e = case fromException e :: Maybe AsyncException of
    Just StackOverflow -> Failed
    Just HeapOverflow -> Failed
    _ -> case fromException e :: Maybe SomeAsyncException of
      Just _ -> Cancelled
      Nothing -> Failed

-- | Observe 'ExceptT' short circuits using the base hook's protected execution
-- and failure-reporting boundary. A body @Left e@ reports 'Failed' with no
-- payload and preserves @e@; no 'Show' or exception instance is needed.
-- Entry @Left@ prevents execution, and normal-return @Left@ propagates without
-- a second terminal. Exit-report @Left@ is discarded in favour of the body's
-- original failure. Base exceptions and cancellation retain the base hook's
-- semantics (including the failure-report cancellation rule of 'ioScope').
-- Apply this once per 'ExceptT' layer; it cannot expose failures hidden by an
-- unadapted base monad.
exceptTScope :: Monad m => Scope m -> Scope (ExceptT e m)
exceptTScope base = Scope $ \enter returned exited body -> ExceptT $
  scopeWith base
    (lower <$> runExceptT enter)
    (runExceptT . returned)
    (\h payload -> void (runExceptT (exited h payload)))
    (lower <$> runExceptT body)
 where
  lower = \case
    Left e -> Left (Left e)
    Right (Left r) -> Left (Right r)
    Right (Right a) -> Right a

-- | The reached frontier after every started branch finishes or suspends.
-- A failure or cancellation produces no authoritative wait snapshot. Only
-- reconcile successful attempts of the same run, without overlapping attempts.
data Progress a = RunFinished a | RunWaiting (NonEmpty PendingWait)
  deriving (Eq, Show)

-- | A trusted description returned by a run, not proof of current reachability
-- or of the identity of whoever answers. Store it outside the completion
-- journal if needed. A caller supplies recipient/deadline metadata in its own
-- request type. Each nested interpreter must use its own journal namespace.
data PendingWait = PendingWait
  { identity :: JournalIdentity
  , key :: DurableKey
  , request :: Value
  , requestSchema :: Schema
  , replySchema :: Schema
  , path :: [Note]
  } deriving (Eq, Show)

-- | Expected fulfilment refusals, returned as data so a caller can ask again.
-- Storage failures remain journal errors. Concurrent conflicting fulfilments
-- may still race at the backend's immutable save boundary.
data ReplyRefused
  = ReplyInvalid Text
    -- ^ The actual reply codec rejected the canonical value.
  | ReplyAlreadyAnswered
    -- ^ A different request or reply is already stored under this key.
  | ReplyNonCanonical Text
  | WaitWrongIdentity
  | WaitInvalidKey Text
  | WaitInvalidRequest Text
  deriving (Eq, Show)

-- | Errors the core raises itself. Provider errors are the provider's own.
data FlowError
  = AwaitWithoutJournal
  | AwaitNeedsResume (NonEmpty PendingWait)
  | JournalFailure JournalError
  | ToolNestingLimit Int
    -- ^ Static tool-body nesting exceeded the limit (100). A heuristic,
    -- not cycle detection; it never limits explicit runtime recursion.
  | InvalidRecursion Text
  | NoSystemOne
  | NoSystemTwo
  | UnsupportedContract [Note] ProjectionError
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
    -- ^ Runs independent work. Execute each action at most once, every action
    -- exactly once on success, and preserve result order. Before returning or
    -- raising, finish or unwind all started work; never detach or retry it.
  , observe :: Event -> m ()
  , journal :: Maybe (Journal m)
  , scope :: Scope m
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
runtime = (runtimeWith throwIO) {scope = ioScope}

-- | A runtime in any monad, given how it raises errors. Its default scope
-- observes ordinary returns and explicit protocol failures only; it cannot
-- catch base exceptions or short circuits. Use 'ioScope' for IO, or e.g.
-- @exceptTScope ioScope@ for @ExceptT e IO@.
runtimeWith :: Monad m => (forall a. FlowError -> m a) -> Runtime m
runtimeWith raise =
  Runtime
    { systemOne = SystemOne (const (raise NoSystemOne))
    , systemTwo = SystemTwo (const (raise NoSystemTwo))
    , parallel = sequence
    , observe = const (pure ())
    , journal = Nothing
    , scope = Scope (\enter returned exited body -> enter >>= \case
        Left r -> pure r
        Right () -> body >>= \case
          Left r -> exited Failed Nothing >> pure r
          Right a -> returned a)
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
