-- | Running flows.
module Agentic.Interpret
  ( interpret
  , preflight
  ) where

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
interpret :: forall m i o. Monad m => Runtime m -> Agentic m i o -> i -> m o
interpret rt flow input = within root Nothing RunNode [] Nothing Nothing (const Completed) (const Nothing) $
  case staticFlow flow >>= (\plan -> validate plan >> pure plan) of
    Left e -> failWith rt e
    Right plan -> run plan
 where
  root = Occurrence rootAddress []
  emit key parent kind path happened payload = rt.observe (Event key parent kind path happened payload)
  within :: forall a. Occurrence -> Maybe Occurrence -> NodeType -> [Note] -> Maybe Int -> Maybe EventPayload -> (a -> Outcome) -> (a -> Maybe EventPayload) -> m a -> m a
  within key parent kind path total entryPayload outcome payload action = scopeWith rt.scope
    (Right () <$ emit key parent kind path (Entered total) entryPayload)
    (\a -> emit key parent kind path (Returned (outcome a)) (payload a) >> pure a)
    (emit key parent kind path)
    (Right <$> action)
  run plan = go plan [] root [] flow input
   where
    go :: forall a b. StaticFlow -> [ContextFrame] -> Occurrence -> [Note] -> Agentic m a b -> a -> m b
    go cursor context parent path flow x = case (flow, cursor.shape) of
      -- Count in the parent, before the Each scope exists, without forcing values.
      (Each f, SEach b) -> let total = length x in total `seq`
        within key (Just parent) EachNode path (Just total) Nothing (const Completed) (const Nothing)
          (inParallel rt [go b (EachFrame address i total : context) key path f a | (i,a) <- zip [0..] x])
      _ -> within key (Just parent) (kind cursor) path Nothing Nothing (const Completed) (const Nothing) body
     where
      address = cursor.address
      key = Occurrence address context
      descend :: forall c d. StaticFlow -> Agentic m c d -> c -> m d
      descend c = go c context key path
      body = case (flow, cursor.shape) of
        (Step s, SStep shape) -> step shape key (Just parent) path s x
        (Seq f g, SSeq l r) -> descend l f x >>= descend r g
        (Fanout f g, SFanout l r) -> do
          results <- inParallel rt [Left <$> descend l f x, Right <$> descend r g x]
          case results of
            [Left b, Right c] -> pure (b, c)
            _ -> error "Agentic.interpret: the runtime's parallel changed its results"
        (First f, SFirst b) -> case x of (a,c) -> (\v -> (v,c)) <$> descend b f a
        (Split f g, SSplit l r) -> case x of
          (a,c) -> do
            results <- inParallel rt [Left <$> descend l f a, Right <$> descend r g c]
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
        _ -> failWith rt (InvalidRecursion "runtime structure differs from the static flow")

    step :: forall a b. StaticStep -> Occurrence -> Maybe Occurrence -> [Note] -> Step m a b -> a -> m b
    step shape key parent path s x = case (s, shape) of
      (Pass, SIdentity) -> pure x
      (Wrap f, SIdentity) -> pure (f x)
      (TakeFirst, SFirstHalf) -> pure (fst x)
      (TakeSecond, SSecondHalf) -> pure (snd x)
      (Arr f, SGlue) -> pure (f x)
      (Act f, SEffect) -> f x
      (Judge inputTree qs, SJudge _ _) -> do
        input <- compile path inputTree
        let request = JudgeRequest (input.encode x) qs.specs
        answers <- if null qs.specs then pure [] else rt.systemOne.ask request
        emit key parent JudgeNode path Judged (Just (JudgePayload request answers))
        answered (decodeAnswers qs answers)
      (Draft inTree outTree instruction toolTrees, SDraft _ _ _ staticTools) -> do
        inCodec <- compile path inTree
        out <- compile path outTree
        tools <- if length toolTrees == length staticTools
          then traverse (uncurry (compileTool path)) (zip staticTools toolTrees)
          else failWith rt (InvalidRecursion "runtime tool list differs from the static flow")
        let conversation = Conversation
              { path = path, instruction = instruction, input = inCodec.encode x
              , inputSchema = inCodec.schema, tools = map toolSpec tools
              , outputSchema = out.schema, history = [] }
        emit key parent DraftNode path Drafting (Just (DraftPayload conversation))
        let loop turnIndex past = do
              turn <- rt.systemTwo.ask conversation{history = past}
              let actionKind = case turn.action of Respond _ -> Response; CallTools _ -> ToolBatch
              emit key parent DraftNode path (Turned turnIndex actionKind) (Just (TurnPayload turn))
              case turn.action of
                Respond v -> case out.decode v of
                  Right b -> pure b
                  Left problem -> do
                    emit key parent DraftNode path (OutputRejected turnIndex) (Just (RejectionPayload problem))
                    loop (turnIndex + 1) (past <> [Rejected turn.raw problem])
                CallTools calls -> do
                  results <- inParallel rt [runTool key path tools turnIndex i c | (i,c) <- zip [0..] calls]
                  loop (turnIndex + 1) (past <> [Called turn.raw (zip (map (.callId) calls) results)])
        loop 0 []
      _ -> failWith rt (InvalidRecursion "runtime step differs from the static flow")
     where
      answered = either (failWith rt . MalformedAnswers) pure

    compile :: [Note] -> Structure a -> m (Codec a)
    compile path = either (failWith rt . UnsupportedContract path) pure . compileModel

    compileTool :: [Note] -> StaticTool -> Tool m -> m (CompiledTool m)
    compileTool path static t = do
      concrete <- either (failWith rt . InvalidRecursion) pure (realTool t)
      cursor <- maybe (failWith rt (InvalidRecursion "missing static tool target")) pure (bodyAt plan (toolTarget static))
      case concrete of
        Tool name doc input out body -> CompiledTool (binding static) (toolTarget static) cursor name doc <$> compile path input <*> compile path out <*> pure body
        _ -> failWith rt (InvalidRecursion "expected a concrete runtime tool")

    binding (StaticTool a _ _ _ _ _) = a

    runTool :: Occurrence -> [Note] -> [CompiledTool m] -> Int -> Int -> ToolCall -> m ToolResult
    runTool caller path tools turn position call = snd <$> within key (Just caller) CallNode path Nothing (Just (CallPayload call)) fst (Just . ResultPayload . snd) body
     where
      context = CallFrame caller.address turn position call.callId : caller.context
      key = Occurrence (child caller.address CallScope) context
      body = case find ((== call.name) . compiledToolName) tools of
        Nothing -> pure (UnknownTool, ToolFailed ("there is no tool named " <> call.name))
        Just (CompiledTool binding target cursor name _ input out body) -> do
          emit key (Just caller) CallNode path (CallResolved binding target) Nothing
          case input.decode call.input of
            Left problem -> pure (InvalidInput, ToolFailed ("invalid input: " <> problem))
            Right i -> do
              result <- go cursor context key (path <> [Note name Nothing]) body i
              pure (CallOk, ToolOk (out.encode result))

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
