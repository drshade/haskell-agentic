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
        results <- parallel rt [Left <$> go path f x, Right <$> go path g x]
        case results of
          [Left b, Right c] -> pure (b, c)
          _ -> error "Agentic.interpret: the runtime's parallel changed its results"
      First f -> case x of (a, c) -> (\b -> (b, c)) <$> go path f a
      Choose f g -> either (go path f) (go path g) x
      Each f -> parallel rt (map (go path f) x)
      Noted n f -> go (path <> [n]) f x

    emit :: [Note] -> Happened -> m ()
    emit path = observe rt . Event path

    step :: forall a b. [Note] -> Step m a b -> a -> m b
    step path s x = case s of
      Arr f -> pure (f x)
      Act f -> emit path Acted >> f x
      Judge input qs
        | null (specs qs) -> answered (decodeAnswers qs [])
        | otherwise -> do
            let request = JudgeRequest (encode input x) (specs qs)
            answers <- askSystemOne (systemOne rt) request
            emit path (Judged request answers)
            answered (decodeAnswers qs answers)
      Draft input out instruction tools -> do
        let conversation =
              Conversation
                { path = path
                , instruction = instruction
                , state = encode input x
                , stateSchema = codecSchema input
                , tools = map toolSpec tools
                , output = codecSchema out
                , history = []
                }
        emit path (Drafting conversation)
        let loop past = do
              turn <- askSystemTwo (systemTwo rt) conversation {history = past}
              emit path (Turned turn)
              case action turn of
                Respond v -> case decode out v of
                  Right b -> pure b
                  Left problem -> do
                    emit path (OutputRejected problem)
                    loop (past <> [Rejected (raw turn) problem])
                CallTools calls -> do
                  results <- parallel rt (map (runTool path tools) calls)
                  loop (past <> [Called (raw turn) (zip (map callId calls) results)])
        loop []
      where
        answered = either (failure rt . MalformedAnswers) pure

    runTool :: [Note] -> [Tool m] -> ToolCall -> m ToolResult
    runTool path tools call = do
      emit path (ToolCalled call)
      result <- case find ((== callName call) . nameOf) tools of
        Nothing -> pure (ToolFailed ("there is no tool named " <> callName call))
        Just (Tool name _ input out body) -> case decode input (callInput call) of
          Left problem -> pure (ToolFailed ("invalid input: " <> problem))
          Right i -> ToolOk . encode out <$> go (path <> [Note name Nothing]) body i
      emit path (ToolReturned (callId call) result)
      pure result
      where
        nameOf (Tool name _ _ _ _) = name

toolSpec :: Tool m -> ToolSpec
toolSpec (Tool name description input _ _) = ToolSpec name description (codecSchema input)
