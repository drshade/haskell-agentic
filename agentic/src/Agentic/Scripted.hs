-- | Providers for tests: the same flows, with no network.
module Agentic.Scripted
  ( -- * System Two
    scripted
  , replyingWith
  , respond
  , callTools
    -- * System One
  , answering
  , fixedAnswers
  ) where

import Agentic.Contract (Codec (..), Contract (..))
import Agentic.Questions
import Agentic.Runtime
import Agentic.Value (Value (..))
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Text (Text)

-- | Take turns from a script, in order. Running out of script is an error.
scripted :: [Action] -> IO (SystemTwo IO)
scripted actions = do
  ref <- newIORef actions
  pure $ SystemTwo $ \_ -> do
    next <- atomicModifyIORef' ref $ \case
      a : rest -> (rest, Just a)
      [] -> ([], Nothing)
    maybe (fail "Agentic.Scripted: the script ran out of turns") (pure . Turn (Raw Null)) next

-- | Answer each turn with a pure function of the conversation.
replyingWith :: Applicative m => (Conversation -> Action) -> SystemTwo m
replyingWith f = SystemTwo (pure . Turn (Raw Null) . f)

-- | A final answer, encoded with its contract.
respond :: Contract a => a -> Action
respond = Respond . encode contract

callTools :: [(Text, Value)] -> Action
callTools calls = CallTools [ToolCall ("call-" <> name) name input | (name, input) <- calls]

-- | Answer every question with a pure function of it.
answering :: Applicative m => (QuestionSpec -> Answer) -> SystemOne m
answering f = SystemOne (pure . map f . requestQuestions)

-- | Yes/no questions get probability @p@; choices and scores pick the first
-- option with certainty.
fixedAnswers :: Applicative m => Probability -> SystemOne m
fixedAnswers p = answering $ \case
  AskYesNo _ -> YesNoAnswer p
  AskChoice _ ((l, _) : _) -> ChoiceAnswer l [(l, 1)] 1
  AskChoice _ [] -> ChoiceAnswer "" [] 0
  AskScore _ _ -> ScoreAnswer 0 [(0, 1)] 1
