-- | Running flows.
module Agentic.Interpret
  ( interpret
  , resumable
  , fulfil
  , preflight
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (sconcat)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Trans.Class (lift)
import Agentic.Journal
import Agentic.Internal.Replay
import Agentic.Value (Value (..), lookupField)
import Agentic.Contract (Codec (..), Structure, compileModel)
import Agentic.Internal.Core
import Agentic.Internal.Address
import Agentic.Internal.Static
import Agentic.Questions (JudgeRequest (..), Questions (..), decodeAnswers)
import Agentic.Runtime
import Data.List (find)
import Data.Text (Text)

-- | Run to completion. If a journaled branch waits, fail through the runtime's
-- ordinary error channel with 'AwaitNeedsResume'. Use 'resumable' to receive
-- waiting as data. Nested plain interpretations therefore fail their enclosing
-- run; their waits are never mistaken for that run's frontier.
{-# INLINABLE interpret #-}
interpret :: Monad m => Runtime m -> Agentic m i o -> i -> m o
interpret rt flow input = case rt.journal of
  Nothing -> worker (direct rt) rt flow input
  Just _ -> resumable rt flow input >>= \case
    Finished value -> pure value
    Waiting waits -> failWith rt (AwaitNeedsResume waits)

-- | Run until completion or until every started independent branch has
-- finished or suspended. A branch that never settles prevents an outcome.
-- Ordinary monad failures remain failures; no new capability is required of m.
{-# INLINABLE resumable #-}
resumable :: Monad m => Runtime m -> Agentic m i o -> i -> m (Progress o)
resumable rt flow input = case rt.journal of
  Nothing -> Finished <$> worker (direct rt) rt flow input
  Just _ -> either Waiting Finished <$> runExceptT (worker (waiting rt) rt flow input)

-- One body, specialised to the user's monad on the no-journal path and to an
-- internal ExceptT on the journaled path. Runtime/provider actions stay in m.
data Execution m n = Execution
  { liftAction :: forall a. m a -> n a
  , parallelAction :: forall a. [n a] -> n [a]
  , scopedAction :: forall a. m () -> (a -> m ())
      -> (Happened -> Maybe EventPayload -> m ())
      -> (NonEmpty PendingWait -> m ()) -> n a -> n a
  , suspendAction :: forall a. NonEmpty PendingWait -> n a
  }

{-# INLINE liftRun #-}
liftRun :: Execution m n -> m a -> n a
liftRun Execution{liftAction=f} = f
{-# INLINE parallelRun #-}
parallelRun :: Execution m n -> [n a] -> n [a]
parallelRun Execution{parallelAction=f} = f
{-# INLINE scopedRun #-}
scopedRun :: Execution m n -> m () -> (a -> m ())
  -> (Happened -> Maybe EventPayload -> m ())
  -> (NonEmpty PendingWait -> m ()) -> n a -> n a
scopedRun Execution{scopedAction=f} = f
{-# INLINE suspendRun #-}
suspendRun :: Execution m n -> NonEmpty PendingWait -> n a
suspendRun Execution{suspendAction=f} = f

{-# INLINE direct #-}
direct :: forall m. Monad m => Runtime m -> Execution m m
direct rt = Execution id (inParallel rt) scoped (failWith rt . AwaitNeedsResume)
 where
  scoped :: forall a. m () -> (a -> m ()) -> (Happened -> Maybe EventPayload -> m ())
    -> (NonEmpty PendingWait -> m ()) -> m a -> m a
  scoped enter returned exited _ action = scopeWith rt.scope
    (Right () <$ enter) (\a -> returned a >> pure a) exited (Right <$> action)

{-# INLINE waiting #-}
waiting :: forall m. Monad m => Runtime m -> Execution m (ExceptT (NonEmpty PendingWait) m)
waiting rt = Execution lift parallel scoped throwE
 where
  parallel :: forall a. [ExceptT (NonEmpty PendingWait) m a] -> ExceptT (NonEmpty PendingWait) m [a]
  parallel actions = ExceptT $ collect <$> inParallel rt (map runExceptT actions)
  scoped :: forall a. m () -> (a -> m ()) -> (Happened -> Maybe EventPayload -> m ())
    -> (NonEmpty PendingWait -> m ()) -> ExceptT (NonEmpty PendingWait) m a
    -> ExceptT (NonEmpty PendingWait) m a
  scoped enter returned exited suspended action = ExceptT $ scopeWith rt.scope
    (Right () <$ enter)
    (\result -> case result of
      Left waits -> suspended waits >> pure (Left waits)
      Right a -> returned a >> pure (Right a))
    exited (Right <$> runExceptT action)
  -- Right-associated concatenation avoids quadratic copying of large frontiers.
  collect xs = case [w | Left w <- xs] of
    [] -> Right [a | Right a <- xs]
    w:ws -> Left (sconcat (w :| ws))

{-# INLINE worker #-}
worker :: forall m n i o. (Monad m, Monad n) => Execution m n -> Runtime m
  -> Agentic m i o -> i -> n o
worker execution rt flow input = within root Nothing RunNode [] Nothing Nothing (const Completed) (const Nothing) $ do
  case rt.journal of
    Nothing -> pure ()
    Just j -> liftRun execution j.initialise
  case staticFlow flow >>= (\plan -> validate plan >> pure plan) of
    Left e -> raise e
    Right plan -> run plan
 where
  root = Occurrence rootAddress []
  emit key parent kind path happened payload = liftRun execution (rt.observe (Event key parent kind path happened payload))
  within :: forall a. Occurrence -> Maybe Occurrence -> NodeType -> [Note] -> Maybe Int -> Maybe EventPayload -> (a -> Outcome) -> (a -> Maybe EventPayload) -> n a -> n a
  within key parent kind path total entryPayload outcome payload action = scopedRun execution
    (rt.observe (Event key parent kind path (Entered total) entryPayload))
    (\a -> rt.observe (Event key parent kind path (Returned (outcome a)) (payload a)))
    (\h p -> rt.observe (Event key parent kind path h p))
    (\waits -> rt.observe (Event key parent kind path Suspended
      (if kind == AwaitNode then Just (WaitPayload (NE.head waits)) else Nothing))) action
  raise :: forall a. FlowError -> n a
  raise = liftRun execution . failWith rt
  parallelW :: forall a. [n a] -> n [a]
  parallelW = parallelRun execution

  -- Only operation boundaries call this helper. With journaling disabled,
  -- neither keys nor codecs/requests are demanded.
  recorded :: forall a. JournalOperation -> Occurrence -> Maybe Occurrence -> NodeType
    -> [Note] -> Value -> (a -> Value) -> (Value -> Either Text a)
    -> (Maybe DurableKey -> n a) -> n a
  recorded op occurrence parent kind path request encode decode live = case rt.journal of
    Nothing -> live Nothing
    Just j -> do
      let key = journalKey op occurrence
      keyValue <- canonical (journalKeyValue key)
      fingerprint <- canonical request
      found <- liftRun execution (j.lookupRecord keyValue)
      case found of
        Just stored -> do
          record <- canonical stored
          result <- case record of
            Object fs -> case (lookupField "request" fs,lookupField "result" fs) of
              (Just old,Just result)
                | old == fingerprint -> pure result
                | otherwise -> raise (JournalFailure (JournalDiverged key))
              _ -> corrupt key "missing completion fields"
            _ -> corrupt key "expected a completion object"
          value <- either (corrupt key) pure (decode result)
          emit occurrence parent kind path (Restored op) Nothing
          pure value
        Nothing -> do
          value <- live (Just (DurableKey j.identity.namespace key))
          result <- canonical (encode value)
          let record = Object [("request",fingerprint),("result",result)]
          liftRun execution (j.saveRecord keyValue record)
          pure value
  canonical = either (raise . JournalFailure . JournalInvalidValue) pure . canonicalValue
  corrupt :: forall a. JournalKey -> Text -> n a
  corrupt key = raise . JournalFailure . JournalDecodeError key
  run plan = go plan [] root [] flow input
   where
    go :: forall a b. StaticFlow -> [ContextFrame] -> Occurrence -> [Note] -> Agentic m a b -> a -> n b
    go cursor context parent path flow x = case (flow, cursor.shape) of
      -- Count in the parent, before the Each scope exists, without forcing values.
      (Each f, SEach b) -> let total = length x in total `seq`
        within key (Just parent) EachNode path (Just total) Nothing (const Completed) (const Nothing)
          (parallelW [go b (EachFrame address i total : context) key path f a | (i,a) <- zip [0..] x])
      _ -> within key (Just parent) (kind cursor) path Nothing Nothing (const Completed) (const Nothing) body
     where
      address = cursor.address
      key = Occurrence address context
      descend :: forall c d. StaticFlow -> Agentic m c d -> c -> n d
      descend c = go c context key path
      body = case (flow, cursor.shape) of
        (Step s, SStep shape) -> step shape key (Just parent) path s x
        (Seq f g, SSeq l r) -> descend l f x >>= descend r g
        (Fanout f g, SFanout l r) -> do
          results <- parallelW [Left <$> descend l f x, Right <$> descend r g x]
          case results of
            [Left b, Right c] -> pure (b, c)
            _ -> error "Agentic.interpret: the runtime's parallel changed its results"
        (First f, SFirst b) -> case x of (a,c) -> (\v -> (v,c)) <$> descend b f a
        (Split f g, SSplit l r) -> case x of
          (a,c) -> do
            results <- parallelW [Left <$> descend l f a, Right <$> descend r g c]
            case results of
              [Left b, Right d] -> pure (b,d)
              _ -> error "Agentic.interpret: the runtime's parallel changed its results"
        (Choose f g, SChoose l r) -> case x of
          Left a -> emit key (Just parent) ChoiceNode path (Selected (EdgeId address LeftSide)) Nothing >> descend l f a
          Right a -> emit key (Just parent) ChoiceNode path (Selected (EdgeId address RightSide)) Nothing >> descend r g a
        (Repeat done f, SRepeat b) ->
          let loop pass a = if done a then pure a else
                go b (RepeatFrame address pass : context) key path f a >>= loop (pass + 1)
           in loop 0 x
        (Noted n f, SNoted _ b) -> go b context key (path <> [n]) f x
        _ -> raise (InvalidRecursion "runtime structure differs from the static flow")

    step :: forall a b. StaticStep -> Occurrence -> Maybe Occurrence -> [Note] -> Step m a b -> a -> n b
    step shape key parent path s x = case (s, shape) of
      (Pass, SIdentity) -> pure x
      (Wrap f, SIdentity) -> pure (f x)
      (TakeFirst, SFirstHalf) -> pure (fst x)
      (TakeSecond, SSecondHalf) -> pure (snd x)
      (Arr f, SGlue) -> pure (f x)
      (Act f, SEffect) -> liftRun execution (f x)
      (Await input output, SAwait _ _) -> case rt.journal of
        Nothing -> raise AwaitWithoutJournal
        Just j -> do
          request <- canonical (input.encode x)
          let pending = PendingWait j.identity
                (DurableKey j.identity.namespace (journalKey Wait key)) request
                input.schema output.schema path
          recorded Wait key parent AwaitNode path request output.encode output.decode
            (const (suspendRun execution (pending :| [])))
      (Durable input output f, SDurable) ->
        recorded DurableEffect key parent EffectNode path (input.encode x)
          output.encode output.decode (\stable -> liftRun execution (f stable x))
      (Judge inputTree qs, SJudge _ _) -> do
        input <- compile path inputTree
        let request = JudgeRequest (input.encode x) qs.specs
        answers <- if null qs.specs then pure [] else
          recorded Judgement key parent JudgeNode path (judgeRequestValue request)
            encodeStoredAnswers (fromMaybeDecode "invalid recorded answers" decodeStoredAnswers)
            (const (liftRun execution (rt.systemOne.ask request)))
        emit key parent JudgeNode path Judged (Just (JudgePayload request answers))
        answered (decodeAnswers qs answers)
      (Draft inTree outTree instruction toolTrees, SDraft _ _ _ staticTools) -> do
        inCodec <- compile path inTree
        out <- compile path outTree
        tools <- if length toolTrees == length staticTools
          then traverse (uncurry (compileTool path)) (zip staticTools toolTrees)
          else raise (InvalidRecursion "runtime tool list differs from the static flow")
        let conversation = Conversation
              { path = path, instruction = instruction, input = inCodec.encode x
              , inputSchema = inCodec.schema, tools = map toolSpec tools
              , outputSchema = out.schema, history = [] }
        emit key parent DraftNode path Drafting (Just (DraftPayload conversation))
        let loop turnIndex past = do
              let request = conversation{history = past}
              turn <- recorded (DraftTurn turnIndex) key parent DraftNode path
                (conversationValue request) encodeStoredTurn
                (fromMaybeDecode "invalid recorded turn" decodeStoredTurn)
                (const (liftRun execution (rt.systemTwo.ask request)))
              let actionKind = case turn.action of Respond _ -> Response; CallTools _ -> ToolBatch
              emit key parent DraftNode path (Turned turnIndex actionKind) (Just (TurnPayload turn))
              case turn.action of
                Respond v -> case out.decode v of
                  Right b -> pure b
                  Left problem -> do
                    emit key parent DraftNode path (OutputRejected turnIndex) (Just (RejectionPayload problem))
                    loop (turnIndex + 1) (past <> [Rejected turn.raw problem])
                CallTools calls -> do
                  results <- parallelW [runTool key path tools turnIndex i c | (i,c) <- zip [0..] calls]
                  loop (turnIndex + 1) (past <> [Called turn.raw (zip (map (.callId) calls) results)])
        loop 0 []
      _ -> raise (InvalidRecursion "runtime step differs from the static flow")
     where
      answered = either (raise . MalformedAnswers) pure

    compile :: [Note] -> Structure a -> n (Codec a)
    compile path = either (raise . UnsupportedContract path) pure . compileModel

    compileTool :: [Note] -> StaticTool -> Tool m -> n (CompiledTool m)
    compileTool path static t = do
      concrete <- either (raise . InvalidRecursion) pure (realTool t)
      cursor <- maybe (raise (InvalidRecursion "missing static tool target")) pure (bodyAt plan (toolTarget static))
      case concrete of
        Tool name doc input out body -> CompiledTool (binding static) (toolTarget static) cursor name doc <$> compile path input <*> compile path out <*> pure body
        _ -> raise (InvalidRecursion "expected a concrete runtime tool")

    binding (StaticTool a _ _ _ _ _) = a

    runTool :: Occurrence -> [Note] -> [CompiledTool m] -> Int -> Int -> ToolCall -> n ToolResult
    runTool caller path tools turn position call = snd <$> within key (Just caller) CallNode path Nothing (Just (CallPayload call)) fst (Just . ResultPayload . snd) body
     where
      context = CallFrame caller.address turn position call.callId : caller.context
      key = Occurrence (child caller.address CallScope) context
      body = case find ((== call.name) . compiledToolName) tools of
        Nothing -> saveCall Nothing Nothing (const (Left "unknown tool has no output codec"))
          (pure (UnknownTool, ToolFailed ("there is no tool named " <> call.name)))
        Just (CompiledTool binding target cursor name _ input out toolBody) -> do
          emit key (Just caller) CallNode path (CallResolved binding target) Nothing
          let check value = () <$ out.decode value
              live = case input.decode call.input of
                Left problem -> pure (InvalidInput, ToolFailed ("invalid input: " <> problem))
                Right i -> do
                  result <- go cursor context key (path <> [Note name Nothing]) toolBody i
                  pure (CallOk, ToolOk (out.encode result))
          saveCall (Just binding) (Just target) check live
      saveCall binding target validateOutput live =
        let addressValue = maybe Null (String . addressText)
            request = Object [("name",String call.name),("input",call.input)
                             ,("binding",addressValue binding),("target",addressValue target)]
            encode completion = Object
              [("binding",addressValue binding),("target",addressValue target)
              ,("completion",completionValue completion)]
            decode (Object fs)
              | lookupField "binding" fs == Just (addressValue binding)
              , lookupField "target" fs == Just (addressValue target)
              , Just v <- lookupField "completion" fs = do
                  c@(outcome,result) <- decodeCompletion v
                  case (outcome,result,binding) of
                    (CallOk,ToolOk value,Just _) -> validateOutput value >> pure c
                    (InvalidInput,ToolFailed _,Just _) -> pure c
                    (UnknownTool,ToolFailed _,Nothing) -> pure c
                    _ -> Left "inconsistent tool completion"
            decode _ = Left "invalid tool binding or completion"
         in recorded ToolExecution key (Just caller) CallNode path request encode decode (const live)

data CompiledTool m where
  CompiledTool :: Address -> Address -> StaticFlow -> Text -> Text -> Codec i -> Codec o -> Agentic m i o -> CompiledTool m

compiledToolName :: CompiledTool m -> Text
compiledToolName (CompiledTool _ _ _ n _ _ _ _) = n

toolSpec :: CompiledTool m -> ToolSpec
toolSpec (CompiledTool _ _ _ name description input _ _) = ToolSpec name description input.schema

kind :: StaticFlow -> NodeType
kind cursor = case cursor.shape of
  SStep s -> case s of
    SIdentity -> IdentityNode
    SGlue -> PureNode
    SFirstHalf -> FirstHalfNode
    SSecondHalf -> SecondHalfNode
    SEffect -> EffectNode
    SAwait _ _ -> AwaitNode
    SDurable -> EffectNode
    SDraft {} -> DraftNode
    SJudge {} -> JudgeNode
  SSeq {} -> SequenceNode
  SFanout {} -> FanoutNode
  SSplit {} -> SplitNode
  SFirst {} -> FirstNode
  SChoose {} -> ChoiceNode
  SEach {} -> EachNode
  SRepeat {} -> RepeatNode
  SNoted {} -> NoteNode

-- | Validate the finite static flow before any effects. Every real group
-- member is checked, including uncalled members. Tool nesting is limited to
-- 100 during static walks; explicit recursive calls have no runtime limit.
preflight :: Agentic m i o -> Either FlowError ()
preflight flow = staticFlow flow >>= validate

-- | Validate and save a reply using the actual await codec found structurally
-- in the supplied flow. This runs no flow action, model, observer or replay.
-- Refusals are data and leave no completion. This does not prove that the wait
-- was reached or is still wanted; the caller trusts its pending descriptor and
-- owns whether to accept a late reply. A different stored request or reply is refused;
-- the same canonical record succeeds without another save. Journal faults
-- remain errors. Callers should serialise fulfilments for the same wait:
-- concurrent deliveries can race between lookup and save, in which case the
-- backend remains authoritative and may raise JournalConflict. Flow-structure failures and a missing
-- journal use the runtime's failure hook.
fulfil :: Monad m => Runtime m -> Agentic m i o -> PendingWait -> Value
  -> m (Either ReplyRefused ())
fulfil rt flow pending reply = do
  j <- maybe (failWith rt AwaitWithoutJournal) pure rt.journal
  if pending.identity /= j.identity || pending.key.namespace /= j.identity.namespace
    then pure (Left WaitWrongIdentity)
    else if pending.key.key.operation /= Wait
      then pure (Left (WaitInvalidKey "key is not a wait"))
      else do
        j.initialise
        plan <- either (failWith rt) pure (staticFlow flow)
        either (failWith rt) pure (validate plan)
        case bodyAt plan pending.key.key.address of
          Just (StaticFlow _ (SStep (SAwait _ output))) ->
            case prepare output of
              Left refused -> pure (Left refused)
              Right (key,record) -> do
                found <- j.lookupRecord key
                case found of
                  Nothing -> j.saveRecord key record >> pure (Right ())
                  Just stored -> do
                    old <- either (failWith rt . JournalFailure . JournalInvalidValue) pure (canonicalValue stored)
                    case old of
                      Object fs | Just _ <- lookupField "request" fs, Just _ <- lookupField "result" fs ->
                        pure (if old == record then Right () else Left ReplyAlreadyAnswered)
                      _ -> failWith rt (JournalFailure (JournalDecodeError pending.key.key "invalid wait completion"))
          _ -> pure (Left (WaitInvalidKey "address does not identify an await"))
 where
  prepare :: Codec a -> Either ReplyRefused (Value,Value)
  prepare output = do
    value <- either (Left . ReplyNonCanonical) Right (canonicalValue reply)
    _ <- either (Left . ReplyInvalid) Right (output.decode value)
    request <- either (Left . WaitInvalidRequest) Right (canonicalValue pending.request)
    key <- either (Left . WaitInvalidKey) Right (canonicalValue (journalKeyValue pending.key.key))
    pure (key,Object [("request",request),("result",value)])
