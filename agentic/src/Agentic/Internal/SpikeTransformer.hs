{-# LANGUAGE QualifiedDo #-}
-- | Running flows.
module Agentic.Internal.SpikeTransformer
  ( interpret
  , preflight
  ) where

import Agentic.Internal.SpikeCommon
import Data.List.NonEmpty (NonEmpty (..))
import Agentic.Internal.SpikeTransformerOps qualified as W
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

-- | Run a flow with a runtime. The root is entered before static preparation;
-- a failure-capable scope reports a preflight error there, with no flow node
-- entered. Invalid structure need not have a usable captured graph.
-- Let concrete callers specialise the monad operations around each scope.
{-# INLINABLE interpret #-}
interpret :: forall m i o. Monad m => Runtime m -> Agentic m i o -> i -> m (Either Waits o)
interpret rt flow input = W.run $ within root Nothing RunNode [] Nothing Nothing (const Completed) (const Nothing) $ W.do
  case rt.journal of
    Nothing -> W.pure ()
    Just j -> W.lift j.initialise
  case staticFlow flow >>= (\plan -> validate plan >> pure plan) of
    Left e -> raise e
    Right plan -> run plan
 where
  root = Occurrence rootAddress []
  emit key parent kind path happened payload = W.lift (rt.observe (Event key parent kind path happened payload))
  within :: forall a. Occurrence -> Maybe Occurrence -> NodeType -> [Note] -> Maybe Int -> Maybe EventPayload -> (a -> Outcome) -> (a -> Maybe EventPayload) -> W.Work m a -> W.Work m a
  within key parent kind path total entryPayload outcome payload action = W.pack $ scopeWith rt.scope
    (Right () <$ rt.observe (Event key parent kind path (Entered total) entryPayload))
    (\result -> case result of
      Left waits -> rt.observe (Event key parent kind path Suspended Nothing) >> pure (Left waits)
      Right value -> rt.observe (Event key parent kind path (Returned (outcome value)) (payload value)) >> pure (Right value))
    (\h p -> rt.observe (Event key parent kind path h p))
    (Right <$> W.run action)
  raise :: forall a. FlowError -> W.Work m a
  raise = W.lift . failWith rt
  parallelW :: forall a. [W.Work m a] -> W.Work m [a]
  parallelW actions = W.pack $ collect <$> inParallel rt (map W.run actions)

  -- Only operation boundaries call this helper. With journaling disabled,
  -- neither keys nor codecs/requests are demanded.
  recorded :: forall a. JournalOperation -> Occurrence -> Maybe Occurrence -> NodeType
    -> [Note] -> Value -> (a -> Value) -> (Value -> Either Text a)
    -> (Maybe DurableKey -> W.Work m a) -> W.Work m a
  recorded op occurrence parent kind path request encode decode live = case rt.journal of
    Nothing -> live Nothing
    Just j -> W.do
      let key = journalKey op occurrence
      keyValue <- canonical (journalKeyValue key)
      fingerprint <- canonical request
      found <- W.lift (j.lookupRecord keyValue)
      case found of
        Just stored -> W.do
          record <- canonical stored
          result <- case record of
            Object fs -> case (lookupField "request" fs,lookupField "result" fs) of
              (Just old,Just result)
                | old == fingerprint -> W.pure result
                | otherwise -> raise (JournalFailure (JournalDiverged key))
              _ -> corrupt key "missing completion fields"
            _ -> corrupt key "expected a completion object"
          value <- either (corrupt key) W.pure (decode result)
          emit occurrence parent kind path (Restored op) Nothing
          W.pure value
        Nothing -> W.do
          value <- live (Just (DurableKey j.identity.namespace key))
          result <- canonical (encode value)
          let record = Object [("request",fingerprint),("result",result)]
          W.lift (j.saveRecord keyValue record)
          W.pure value
  canonical = either (raise . JournalFailure . JournalInvalidValue) W.pure . canonicalValue
  corrupt :: forall a. JournalKey -> Text -> W.Work m a
  corrupt key = raise . JournalFailure . JournalDecodeError key
  run plan = go plan [] root [] flow input
   where
    go :: forall a b. StaticFlow -> [ContextFrame] -> Occurrence -> [Note] -> Agentic m a b -> a -> W.Work m b
    go cursor context parent path flow x = case (flow, cursor.shape) of
      -- Count in the parent, before the Each scope exists, without forcing values.
      (Each f, SEach b) -> let total = length x in total `seq`
        within key (Just parent) EachNode path (Just total) Nothing (const Completed) (const Nothing)
          (parallelW [go b (EachFrame address i total : context) key path f a | (i,a) <- zip [0..] x])
      _ -> within key (Just parent) (kind cursor) path Nothing Nothing (const Completed) (const Nothing) body
     where
      address = cursor.address
      key = Occurrence address context
      descend :: forall c d. StaticFlow -> Agentic m c d -> c -> W.Work m d
      descend c = go c context key path
      body = case (flow, cursor.shape) of
        (Step s, SStep shape) -> step shape key (Just parent) path s x
        (Seq f g, SSeq l r) -> descend l f x W.>>= descend r g
        (Fanout f g, SFanout l r) -> W.do
          results <- parallelW [Left W.<$> descend l f x, Right W.<$> descend r g x]
          case results of
            [Left b, Right c] -> W.pure (b, c)
            _ -> error "Agentic.interpret: the runtime's parallel changed its results"
        (First f, SFirst b) -> case x of (a,c) -> (\v -> (v,c)) W.<$> descend b f a
        (Split f g, SSplit l r) -> case x of
          (a,c) -> W.do
            results <- parallelW [Left W.<$> descend l f a, Right W.<$> descend r g c]
            case results of
              [Left b, Right d] -> W.pure (b,d)
              _ -> error "Agentic.interpret: the runtime's parallel changed its results"
        (Choose f g, SChoose l r) -> case x of
          Left a -> emit key (Just parent) ChoiceNode path (Selected (EdgeId address LeftSide)) Nothing W.>> descend l f a
          Right a -> emit key (Just parent) ChoiceNode path (Selected (EdgeId address RightSide)) Nothing W.>> descend r g a
        (Repeat done f, SRepeat b) ->
          let loop pass a = if done a then W.pure a else
                go b (RepeatFrame address pass : context) key path f a W.>>= loop (pass + 1)
           in loop 0 x
        (Noted n f, SNoted _ b) -> go b context key (path <> [n]) f x
        _ -> raise (InvalidRecursion "runtime structure differs from the static flow")

    step :: forall a b. StaticStep -> Occurrence -> Maybe Occurrence -> [Note] -> Step m a b -> a -> W.Work m b
    step shape key parent path s x = case (s, shape) of
      (Pass, SIdentity) -> W.pure x
      (Wrap f, SIdentity) -> W.pure (f x)
      (TakeFirst, SFirstHalf) -> W.pure (fst x)
      (TakeSecond, SSecondHalf) -> W.pure (snd x)
      (Arr f, SGlue) -> W.pure (f x)
      (Act f, SEffect) -> W.lift (f x)
      (Await input output, SAwait) -> case rt.journal of
        Nothing -> raise (InvalidRecursion "spike: await requires journal")
        Just j -> recorded Wait key parent EffectNode path (input.encode x)
          output.encode output.decode (const (W.stop
            (Pending j.identity (journalKey Wait key) (input.encode x) :| [])))
      (Durable input output f, SDurable) ->
        recorded DurableEffect key parent EffectNode path (input.encode x)
          output.encode output.decode (\stable -> W.lift (f stable x))
      (Judge inputTree qs, SJudge _ _) -> W.do
        input <- compile path inputTree
        let request = JudgeRequest (input.encode x) qs.specs
        answers <- if null qs.specs then W.pure [] else
          recorded Judgement key parent JudgeNode path (judgeRequestValue request)
            encodeStoredAnswers (fromMaybeDecode "invalid recorded answers" decodeStoredAnswers)
            (const (W.lift (rt.systemOne.ask request)))
        emit key parent JudgeNode path Judged (Just (JudgePayload request answers))
        answered (decodeAnswers qs answers)
      (Draft inTree outTree instruction toolTrees, SDraft _ _ _ staticTools) -> W.do
        inCodec <- compile path inTree
        out <- compile path outTree
        tools <- if length toolTrees == length staticTools
          then W.traverse (uncurry (compileTool path)) (zip staticTools toolTrees)
          else raise (InvalidRecursion "runtime tool list differs from the static flow")
        let conversation = Conversation
              { path = path, instruction = instruction, input = inCodec.encode x
              , inputSchema = inCodec.schema, tools = map toolSpec tools
              , outputSchema = out.schema, history = [] }
        emit key parent DraftNode path Drafting (Just (DraftPayload conversation))
        let loop turnIndex past = W.do
              let request = conversation{history = past}
              turn <- recorded (DraftTurn turnIndex) key parent DraftNode path
                (conversationValue request) encodeStoredTurn
                (fromMaybeDecode "invalid recorded turn" decodeStoredTurn)
                (const (W.lift (rt.systemTwo.ask request)))
              let actionKind = case turn.action of Respond _ -> Response; CallTools _ -> ToolBatch
              emit key parent DraftNode path (Turned turnIndex actionKind) (Just (TurnPayload turn))
              case turn.action of
                Respond v -> case out.decode v of
                  Right b -> W.pure b
                  Left problem -> W.do
                    emit key parent DraftNode path (OutputRejected turnIndex) (Just (RejectionPayload problem))
                    loop (turnIndex + 1) (past <> [Rejected turn.raw problem])
                CallTools calls -> W.do
                  results <- parallelW [runTool key path tools turnIndex i c | (i,c) <- zip [0..] calls]
                  loop (turnIndex + 1) (past <> [Called turn.raw (zip (map (.callId) calls) results)])
        loop 0 []
      _ -> raise (InvalidRecursion "runtime step differs from the static flow")
     where
      answered = either (raise . MalformedAnswers) W.pure

    compile :: [Note] -> Structure a -> W.Work m (Codec a)
    compile path = either (raise . UnsupportedContract path) W.pure . compileModel

    compileTool :: [Note] -> StaticTool -> Tool m -> W.Work m (CompiledTool m)
    compileTool path static t = W.do
      concrete <- either (raise . InvalidRecursion) W.pure (realTool t)
      cursor <- maybe (raise (InvalidRecursion "missing static tool target")) W.pure (bodyAt plan (toolTarget static))
      case concrete of
        Tool name doc input out body -> CompiledTool (binding static) (toolTarget static) cursor name doc W.<$> compile path input W.<*> compile path out W.<*> W.pure body
        _ -> raise (InvalidRecursion "expected a concrete runtime tool")

    binding (StaticTool a _ _ _ _ _) = a

    runTool :: Occurrence -> [Note] -> [CompiledTool m] -> Int -> Int -> ToolCall -> W.Work m ToolResult
    runTool caller path tools turn position call = snd W.<$> within key (Just caller) CallNode path Nothing (Just (CallPayload call)) fst (Just . ResultPayload . snd) body
     where
      context = CallFrame caller.address turn position call.callId : caller.context
      key = Occurrence (child caller.address CallScope) context
      body = case find ((== call.name) . compiledToolName) tools of
        Nothing -> saveCall Nothing Nothing (const (Left "unknown tool has no output codec"))
          (W.pure (UnknownTool, ToolFailed ("there is no tool named " <> call.name)))
        Just (CompiledTool binding target cursor name _ input out toolBody) -> W.do
          emit key (Just caller) CallNode path (CallResolved binding target) Nothing
          let check value = () <$ out.decode value
              live = case input.decode call.input of
                Left problem -> W.pure (InvalidInput, ToolFailed ("invalid input: " <> problem))
                Right i -> W.do
                  result <- go cursor context key (path <> [Note name Nothing]) toolBody i
                  W.pure (CallOk, ToolOk (out.encode result))
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
    SAwait -> EffectNode
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
