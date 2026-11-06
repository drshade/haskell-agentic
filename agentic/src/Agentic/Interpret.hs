-- | Running flows.
module Agentic.Interpret
  ( interpret
  , preflight
  ) where

import Agentic.Contract (Codec (..), ContractTree, ProjectionError (..), compileModel)
import Agentic.Core
import Agentic.Questions (JudgeRequest (..), Questions (..), decodeAnswers)
import Agentic.Runtime
import Data.List (find)
import Data.Text (Text)

-- | Run a flow with a runtime.
interpret :: forall m i o. Monad m => Runtime m -> Agentic m i o -> i -> m o
interpret rt flow input = case preflight flow of
  Left e -> failWith rt e
  Right () -> go [] flow input
 where
  go :: forall a b. [Note] -> Agentic m a b -> a -> m b
  go path flow x = case flow of
    Step s -> step path s x
    Seq f g -> go path f x >>= go path g
    Fanout f g -> do
      results <- inParallel rt [Left <$> go path f x, Right <$> go path g x]
      case results of
        [Left b, Right c] -> pure (b, c)
        _ -> error "Agentic.interpret: the runtime's parallel changed its results"
    First f -> case x of (a, c) -> (\b -> (b, c)) <$> go path f a
    Split f g -> case x of
      (a, c) -> do
        results <- inParallel rt [Left <$> go path f a, Right <$> go path g c]
        case results of
          [Left b, Right d] -> pure (b, d)
          _ -> error "Agentic.interpret: the runtime's parallel changed its results"
    Choose f g -> either (go path f) (go path g) x
    Each f -> inParallel rt (map (go path f) x)
    Repeat done f ->
      let loop a = if done a then pure a else go path f a >>= loop
       in loop x
    Noted n f -> go (path <> [n]) f x

  emit :: [Note] -> Happened -> m ()
  emit path = rt.observe . Event path

  step :: forall a b. [Note] -> Step m a b -> a -> m b
  step path s x = case s of
    Pass -> pure x
    Wrap f -> pure (f x)
    TakeFirst -> pure (fst x)
    TakeSecond -> pure (snd x)
    Arr f -> pure (f x)
    Act f -> emit path Acted >> f x
    Judge inputTree qs -> do
      input <- compile path inputTree
      if null qs.specs
        then answered (decodeAnswers qs [])
        else do
          let request = JudgeRequest (input.encode x) qs.specs
          answers <- rt.systemOne.ask request
          emit path (Judged request answers)
          answered (decodeAnswers qs answers)
    Draft inTree outTree instruction toolTrees -> do
      inCodec <- compile path inTree
      out <- compile path outTree
      tools <- traverse (compileTool path) toolTrees
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
   where
    answered = either (failWith rt . MalformedAnswers) pure

  compile :: [Note] -> ContractTree a -> m (Codec a)
  compile path = either (failWith rt . UnsupportedContract path) pure . compileModel

  compileTool :: [Note] -> Tool m -> m (CompiledTool m)
  compileTool path (Tool name doc input out body) = CompiledTool name doc <$> compile path input <*> compile path out <*> pure body

  runTool :: [Note] -> [CompiledTool m] -> ToolCall -> m ToolResult
  runTool path tools call = do
    emit path (ToolCalled call)
    result <- case find ((== call.name) . compiledToolName) tools of
      Nothing -> pure (ToolFailed ("there is no tool named " <> call.name))
      Just (CompiledTool name _ input out body) -> case input.decode call.input of
        Left problem -> pure (ToolFailed ("invalid input: " <> problem))
        Right i -> ToolOk . out.encode <$> go (path <> [Note name Nothing]) body i
    emit path (ToolReturned call.callId result)
    pure result

data CompiledTool m where
  CompiledTool :: Text -> Text -> Codec i -> Codec o -> Agentic m i o -> CompiledTool m

compiledToolName :: CompiledTool m -> Text
compiledToolName (CompiledTool n _ _ _ _) = n

toolSpec :: CompiledTool m -> ToolSpec
toolSpec (CompiledTool name description input _ _) = ToolSpec name description input.schema

-- | Validate all contracts, branches and tool bodies without executing effects.
-- Interpret runs this automatically before running any step.
preflight :: Agentic m i o -> Either FlowError ()
preflight = go []
 where
  check notes label c = case compileModel c of
    Left (ProjectionError p e) -> Left (UnsupportedContract notes (ProjectionError (label : p) e))
    Right _ -> Right ()
  go :: [Note] -> Agentic m a b -> Either FlowError ()
  go notes = \case
    Step (Draft i o _ ts) -> check notes "draft.input" i >> check notes "draft.output" o >> mapM_ (toolCheck notes) ts
    Step (Judge i _) -> check notes "judge.input" i
    Step _ -> Right ()
    Seq f g -> go notes f >> go notes g
    Fanout f g -> go notes f >> go notes g
    Split f g -> go notes f >> go notes g
    Choose f g -> go notes f >> go notes g
    First f -> go notes f
    Each f -> go notes f
    Repeat _ f -> go notes f
    Noted n f -> go (notes <> [n]) f
  toolCheck notes (Tool n _ i o body) =
    let path = notes <> [Note n Nothing]
     in check path "tool.input" i >> check path "tool.output" o >> go path body
