-- | Running flows.
module Agentic.Interpret
  ( interpret
  ) where

import Agentic.Contract (Codec (..))
import Agentic.Core
import Agentic.Questions (JudgeRequest (..), Questions (..), decodeAnswers)
import Agentic.Runtime
import Data.List (find)

-- | Run a flow with a runtime.
interpret :: forall m i o. Monad m => Runtime m -> Agentic m i o -> i -> m o
interpret rt = go []
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
      Judge input qs
        | null qs.specs -> answered (decodeAnswers qs [])
        | otherwise -> do
            let request = JudgeRequest (input.encode x) qs.specs
            answers <- rt.systemOne.ask request
            emit path (Judged request answers)
            answered (decodeAnswers qs answers)
      Draft inCodec out instruction tools -> do
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
              turn <- rt.systemTwo.ask conversation {history = past}
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

    runTool :: [Note] -> [Tool m] -> ToolCall -> m ToolResult
    runTool path tools call = do
      emit path (ToolCalled call)
      result <- case find ((== call.name) . toolName) tools of
        Nothing -> pure (ToolFailed ("there is no tool named " <> call.name))
        Just (Tool name _ input out body) -> case input.decode call.input of
          Left problem -> pure (ToolFailed ("invalid input: " <> problem))
          Right i -> ToolOk . out.encode <$> go (path <> [Note name Nothing]) body i
      emit path (ToolReturned call.callId result)
      pure result

toolSpec :: Tool m -> ToolSpec
toolSpec (Tool name description input _ _) = ToolSpec name description input.schema
