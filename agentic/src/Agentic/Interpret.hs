-- | Running flows.
module Agentic.Interpret
  ( interpret
  , preflight
  ) where

import Agentic.Contract (Codec (..), Structure, compileModel)
import Agentic.Internal.Core
import Agentic.Internal.Static
import Agentic.Questions (JudgeRequest (..), Questions (..), decodeAnswers)
import Agentic.Runtime
import Data.List (find)
import Data.Text (Text)

-- | Run a flow with a runtime.
interpret :: forall m i o. Monad m => Runtime m -> Agentic m i o -> i -> m o
interpret rt flow input = case staticFlow flow >>= (\plan -> validate plan >> pure plan) of
  Left e -> failWith rt e
  Right plan -> run plan
 where
  run plan = go plan [] flow input
   where
    go :: forall a b. StaticFlow -> [Note] -> Agentic m a b -> a -> m b
    go cursor path flow x = case (flow, cursor) of
      (Step s, SStep shape) -> step shape path s x
      (Seq f g, SSeq l r) -> go l path f x >>= go r path g
      (Fanout f g, SFanout l r) -> do
        results <- inParallel rt [Left <$> go l path f x, Right <$> go r path g x]
        case results of
          [Left b, Right c] -> pure (b, c)
          _ -> error "Agentic.interpret: the runtime's parallel changed its results"
      (First f, SFirst b) -> case x of (a, c) -> (\v -> (v, c)) <$> go b path f a
      (Split f g, SSplit l r) -> case x of
        (a, c) -> do
          results <- inParallel rt [Left <$> go l path f a, Right <$> go r path g c]
          case results of
            [Left b, Right d] -> pure (b, d)
            _ -> error "Agentic.interpret: the runtime's parallel changed its results"
      (Choose f g, SChoose l r) -> either (go l path f) (go r path g) x
      (Each f, SEach b) -> inParallel rt (map (go b path f) x)
      (Repeat done f, SRepeat b) ->
        let loop a = if done a then pure a else go b path f a >>= loop
         in loop x
      (Noted n f, SNoted _ b) -> go b (path <> [n]) f x
      _ -> failWith rt (InvalidRecursion "runtime structure differs from the static flow")

    emit :: [Note] -> Happened -> m ()
    emit path = rt.observe . Event path

    step :: forall a b. StaticStep -> [Note] -> Step m a b -> a -> m b
    step shape path s x = case (s, shape) of
      (Pass, SIdentity) -> pure x
      (Wrap f, SIdentity) -> pure (f x)
      (TakeFirst, SFirstHalf) -> pure (fst x)
      (TakeSecond, SSecondHalf) -> pure (snd x)
      (Arr f, SGlue) -> pure (f x)
      (Act f, SEffect) -> emit path Acted >> f x
      (Judge inputTree qs, SJudge _ _) -> do
        input <- compile path inputTree
        if null qs.specs
          then answered (decodeAnswers qs [])
          else do
            let request = JudgeRequest (input.encode x) qs.specs
            answers <- rt.systemOne.ask request
            emit path (Judged request answers)
            answered (decodeAnswers qs answers)
      (Draft inTree outTree instruction toolTrees, SDraft _ _ _ staticTools) -> do
        inCodec <- compile path inTree
        out <- compile path outTree
        tools <- if length toolTrees == length staticTools
          then traverse (uncurry (compileTool path)) (zip staticTools toolTrees)
          else failWith rt (InvalidRecursion "runtime tool list differs from the static flow")
        let conversation =
              Conversation
                { path = path
                , instruction = instruction
                , input = inCodec.encode x
                , inputSchema = inCodec.schema
                , tools = map toolSpec tools
                , outputSchema = out.schema
                , history = []
                }
        emit path (Drafting conversation)
        let loop past = do
              turn <- rt.systemTwo.ask conversation{history = past}
              emit path (Turned turn)
              case turn.action of
                Respond v -> case out.decode v of
                  Right b -> pure b
                  Left problem -> do
                    emit path (OutputRejected problem)
                    loop (past <> [Rejected turn.raw problem])
                CallTools calls -> do
                  results <- inParallel rt (map (runTool path tools) calls)
                  loop (past <> [Called turn.raw (zip (map (.callId) calls) results)])
        loop []
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
        Tool name doc input out body -> CompiledTool cursor name doc <$> compile path input <*> compile path out <*> pure body
        _ -> failWith rt (InvalidRecursion "expected a concrete runtime tool")

    runTool :: [Note] -> [CompiledTool m] -> ToolCall -> m ToolResult
    runTool path tools call = do
      emit path (ToolCalled call)
      result <- case find ((== call.name) . compiledToolName) tools of
        Nothing -> pure (ToolFailed ("there is no tool named " <> call.name))
        Just (CompiledTool cursor name _ input out body) -> case input.decode call.input of
          Left problem -> pure (ToolFailed ("invalid input: " <> problem))
          Right i -> ToolOk . out.encode <$> go cursor (path <> [Note name Nothing]) body i
      emit path (ToolReturned call.callId result)
      pure result

data CompiledTool m where
  CompiledTool :: StaticFlow -> Text -> Text -> Codec i -> Codec o -> Agentic m i o -> CompiledTool m

compiledToolName :: CompiledTool m -> Text
compiledToolName (CompiledTool _ n _ _ _ _) = n

toolSpec :: CompiledTool m -> ToolSpec
toolSpec (CompiledTool _ name description input _ _) = ToolSpec name description input.schema

-- | Validate the finite static flow before any effects. Every real group
-- member is checked, including uncalled members. Tool nesting is limited to
-- 100 during static walks; explicit recursive calls have no runtime limit.
preflight :: Agentic m i o -> Either FlowError ()
preflight flow = staticFlow flow >>= validate
