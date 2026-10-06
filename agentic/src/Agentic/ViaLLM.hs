-- | Letting an LLM stand in as System One, when there's no Jev.
module Agentic.ViaLLM
  ( viaLLM
  ) where

import Agentic.Core (Instruction (..))
import Agentic.Questions
import Agentic.Runtime
import Agentic.Schema
import Agentic.Value (Value (..), lookupField, renderJson)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T

-- | Answer judgements with one LLM turn per request. The LLM gives a
-- probability for each answer; unlike Jev's, they aren't calibrated.
viaLLM :: MonadFail m => SystemTwo m -> SystemOne m
viaLLM two = SystemOne $ \request -> do
  let qs = zip ids (requestQuestions request)
      conversation =
        Conversation
          { path = []
          , instruction = Instruction "Answer each question about the input. Give every probability as a number from 0 to 1."
          , input = requestInput request
          , inputSchema = schemaOf SNull
          , tools = []
          , outputSchema = schemaOf (SObject [Field qid (questionSchema q) True | (qid, q) <- qs])
          , history = []
          }
  turn <- askSystemTwo two conversation
  case action turn of
    Respond (Object kvs) -> either (fail . T.unpack) pure (traverse (\(qid, q) -> answer q =<< field' qid kvs) qs)
    Respond other -> fail ("viaLLM: expected an object of answers, got " <> T.unpack (renderJson other))
    CallTools _ -> fail "viaLLM: the model called a tool while answering questions"
  where
    ids = ["q" <> T.pack (show n) | n <- [0 :: Int ..]]
    field' k kvs = maybe (Left ("missing " <> k)) Right (lookupField k kvs)

questionSchema :: QuestionSpec -> Schema
questionSchema = \case
  AskYesNo q ->
    documentedSchema q (schemaOf (SObject [Field "probabilityYes" (documentedSchema "The probability that the answer is yes" (schemaOf SNumber)) True]))
  AskChoice q opts -> distribution (q <> " Give each option's probability; they should sum to 1.") opts
  AskScore q levels -> distribution (q <> " The options are ordered levels, lowest first. Give each level's probability; they should sum to 1.") levels
  where
    distribution q opts =
      documentedSchema q (schemaOf (SObject [Field l (documentedSchema (maybe l id d) (schemaOf SNumber)) True | (l, d) <- opts]))

answer :: QuestionSpec -> Value -> Either Text Answer
answer spec v = case (spec, v) of
  (AskYesNo _, Object kvs) -> YesNoAnswer <$> (number =<< get "probabilityYes" kvs)
  (AskChoice _ opts, Object kvs) -> do
    ps <- traverse (\(l, _) -> (l,) <$> (number =<< get l kvs)) opts
    let (best, p) = maximumBy (comparing snd) ps
    pure (ChoiceAnswer best ps p)
  (AskScore _ levels, Object kvs) -> do
    ps <- traverse (\(l, _) -> number =<< get l kvs) levels
    let weights = map probability ps
        total = sum weights
        pos = if total > 0 then sum (zipWith (*) [0 ..] weights) / total else 0
    pure (ScoreAnswer pos (zip [0 ..] ps) (maximum ps))
  (_, other) -> Left ("expected an object, got " <> renderJson other)
  where
    get k kvs = maybe (Left ("missing " <> k)) Right (lookupField k kvs)
    number = \case
      Number d -> Right (toProbability d)
      Integer n -> Right (toProbability (fromInteger n))
      other -> Left ("expected a probability, got " <> renderJson other)
