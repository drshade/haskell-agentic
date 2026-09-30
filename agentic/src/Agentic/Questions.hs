{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Questions for a System One model such as Jev. Following Jev's terms, a step
-- asks 'Questions' about its input, the /state/.
module Agentic.Questions
  ( -- * Questions
    Questions (..)
  , yesNo
  , choice
  , score
    -- * Answers
  , Probability
  , probability
  , fromBasisPoints
  , basisPoints
  , YesNo (..)
  , Choice (..)
  , Score (..)
    -- * Wire types
  , QuestionSpec (..)
  , Answer (..)
  , JudgeRequest (..)
  , decodeAnswers
  ) where

import Agentic.Contract (Contract (..), Option (..), OptionSet (..), Options (..), mapCodec, record, required)
import Agentic.Value (Value (..))
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Probabilities

-- | A probability, held as basis points (0–10000) so that results replay and
-- compare exactly. Write literals directly: @0.9 :: Probability@.
newtype Probability = Probability Int
  deriving (Eq, Ord)

instance Show Probability where
  show p = show (probability p)

instance Num Probability where
  Probability a + Probability b = clamp (a + b)
  Probability a - Probability b = clamp (a - b)
  Probability a * Probability b = clamp ((a * b) `div` 10000)
  abs = id
  signum (Probability a) = Probability (if a > 0 then 10000 else 0)
  fromInteger n = clamp (fromInteger n * 10000)

instance Fractional Probability where
  fromRational r = clamp (round (r * 10000))
  Probability a / Probability b = clamp ((a * 10000) `div` max 1 b)

clamp :: Int -> Probability
clamp = Probability . max 0 . min 10000

probability :: Probability -> Double
probability (Probability bp) = fromIntegral bp / 10000

-- | Convert a provider's probability, rounding once (half to even).
fromBasisPoints :: Double -> Probability
fromBasisPoints d = clamp (round (d * 10000))

basisPoints :: Probability -> Int
basisPoints (Probability bp) = bp

-- ---------------------------------------------------------------------------
-- Answers

-- | Jev's Noul: the probability that the answer is yes.
newtype YesNo = YesNo {yes :: Probability}
  deriving (Eq, Show)

data Choice a = Choice
  { chosen :: a
  , choiceProbabilities :: [(a, Probability)]
  , choiceConfidence :: Probability
  }
  deriving (Eq, Show)

data Score a = Score
  { position :: Double
    -- ^ The probability-weighted position, from 0 (the first option) upwards.
  , scoreProbabilities :: [(a, Probability)]
  , scoreConfidence :: Probability
  }
  deriving (Eq, Show)

-- Answers have contracts, so a judgement can be a tool's output.

instance Contract Probability where
  contract = mapCodec fromBasisPoints probability (contract @Double)

instance Contract YesNo where
  contract = record "A yes/no judgement" (YesNo <$> required "yes" "The probability that the answer is yes" yes)

instance Contract a => Contract (Choice a) where
  contract =
    record "A choice between options" $
      Choice
        <$> required "chosen" "The most likely option" chosen
        <*> required "probabilities" "Each option's probability" choiceProbabilities
        <*> required "confidence" "How concentrated the probabilities are" choiceConfidence

instance Contract a => Contract (Score a) where
  contract =
    record "A position on ordered levels" $
      Score
        <$> required "position" "The probability-weighted position, from 0 upwards" position
        <*> required "probabilities" "Each level's probability" scoreProbabilities
        <*> required "confidence" "How concentrated the probabilities are" scoreConfidence

-- ---------------------------------------------------------------------------
-- Wire types

data QuestionSpec
  = AskYesNo Text
  | AskChoice Text [(Text, Maybe Text)]
    -- ^ Option labels with their descriptions.
  | AskScore Text [(Text, Maybe Text)]
    -- ^ Levels in order, lowest first.
  deriving (Eq, Ord, Show)

data Answer
  = YesNoAnswer Probability
  | ChoiceAnswer Text [(Text, Probability)] Probability
    -- ^ The chosen label, every label's probability, and the confidence.
  | ScoreAnswer Double [(Int, Probability)] Probability
    -- ^ The position, each level's probability (by index), and the confidence.
  deriving (Eq, Show)

-- | What a System One provider receives: the encoded state and the questions.
data JudgeRequest = JudgeRequest
  { requestState :: Value
  , requestQuestions :: [QuestionSpec]
  }
  deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- Questions

-- | One or more questions about the same state, sent as one request. Combine
-- them applicatively:
--
-- > judge (Review <$> funny <*> groan)
data Questions a = Questions
  { specs :: [QuestionSpec]
  , decoder :: [Answer] -> Either Text a
  }

instance Functor Questions where
  fmap f q = q {decoder = fmap f . decoder q}

instance Applicative Questions where
  pure x = Questions [] (\case [] -> Right x; _ -> Left "too many answers")
  Questions l dl <*> Questions r dr = Questions (l <> r) $ \answers ->
    let (before, after) = splitAt (length l) answers
     in dl before <*> dr after

decodeAnswers :: Questions a -> [Answer] -> Either Text a
decodeAnswers = decoder

single :: QuestionSpec -> (Answer -> Either Text a) -> Questions a
single spec decode = Questions [spec] $ \case
  [answer] -> decode answer
  answers -> Left ("expected one answer, got " <> T.pack (show (length answers)))

-- | Jev's Noul primitive: how likely is it that the answer is yes?
yesNo :: Text -> Questions YesNo
yesNo q = single (AskYesNo q) $ \case
  YesNoAnswer p -> Right (YesNo p)
  other -> Left ("expected a yes/no answer, got " <> T.pack (show other))

-- | Pick one of an 'Options' type's values.
choice :: forall a. Options a => Text -> Questions (Choice a)
choice q = single (AskChoice q (labels opts)) $ \case
  ChoiceAnswer picked ps conf ->
    Choice <$> byLabel opts picked <*> traverse (\(l, p) -> (,p) <$> byLabel opts l) ps <*> pure conf
  other -> Left ("expected a choice answer, got " <> T.pack (show other))
  where
    opts = optionList (options @a)

-- | Place the state on an 'Options' type's levels, lowest first.
score :: forall a. Options a => Text -> Questions (Score a)
score q = single (AskScore q (labels opts)) $ \case
  ScoreAnswer pos ps conf ->
    Score pos <$> traverse (\(i, p) -> (,p) <$> byIndex i) ps <*> pure conf
  other -> Left ("expected a score answer, got " <> T.pack (show other))
  where
    opts = optionList (options @a)
    byIndex i = case drop i opts of
      o : _ | i >= 0 -> Right (optionValue o)
      _ -> Left ("no level " <> T.pack (show i))

labels :: [Option a] -> [(Text, Maybe Text)]
labels = map (\o -> (optionLabel o, optionDoc o))

byLabel :: [Option a] -> Text -> Either Text a
byLabel opts l = maybe (Left ("unknown option " <> l)) (Right . optionValue) (find ((== l) . optionLabel) opts)
