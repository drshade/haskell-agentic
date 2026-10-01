{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Flows: typed, inspectable descriptions of agentic work.
module Agentic.Core
  ( -- * Flows
    Agentic (..)
  , Step (..)
  , Tool (..)
  , Note (..)
  , Instruction (..)
    -- * Steps
  , draft
  , draftWith
  , judge
  , act
    -- * Tools
  , tool
    -- * Structure
  , each
  , repeatUntil
  , note
  , named
    -- * Judgement helpers
  , keep
  , gate
    -- * Re-exports
  , module Control.Arrow
  ) where

import Agentic.Contract (Codec (..), Contract (..))
import Agentic.Questions (Probability, Questions, YesNo (..))
import Control.Arrow
import qualified Control.Category as Category
import Data.String (IsString (..))
import Agentic.Schema (titled)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy (..))

-- | What a model step is asked to do.
newtype Instruction = Instruction {instructionText :: Text}
  deriving (Eq, Ord, Show)

instance IsString Instruction where
  fromString = Instruction . T.pack

-- | A name, and optionally a description, for a sub-flow. Notes are for whoever
-- is watching the flow, not for the model.
data Note = Note
  { noteName :: Text
  , noteDescription :: Maybe Text
  }
  deriving (Eq, Ord, Show)

-- | The leaves of a flow: the steps that do the work.
data Step m i o where
  Pass :: Step m i i
    -- ^ The input, unchanged: 'id' and 'returnA'.
  Wrap :: (i -> o) -> Step m i o
    -- ^ The input re-wrapped without changing it ('Left', 'Right'), so that
    -- 'describe' can show it as a pass-through.
  Arr :: (i -> o) -> Step m i o
  Act :: (i -> m o) -> Step m i o
  Draft :: Codec i -> Codec o -> Instruction -> [Tool m] -> Step m i o
  Judge :: Codec i -> Questions o -> Step m i o

-- | A flow from @i@ to @o@ in effect @m@. Build flows from steps with the
-- 'Arrow' combinators; run them with 'Agentic.Interpret.interpret'.
data Agentic m i o where
  Step :: Step m i o -> Agentic m i o
  Seq :: Agentic m a b -> Agentic m b c -> Agentic m a c
  Fanout :: Agentic m a b -> Agentic m a c -> Agentic m a (b, c)
  Split :: Agentic m a b -> Agentic m c d -> Agentic m (a, c) (b, d)
  First :: Agentic m a b -> Agentic m (a, c) (b, c)
  Choose :: Agentic m a c -> Agentic m b c -> Agentic m (Either a b) c
  Each :: Agentic m a b -> Agentic m [a] [b]
  Repeat :: (a -> Bool) -> Agentic m a a -> Agentic m a a
  Noted :: Note -> Agentic m i o -> Agentic m i o

-- | A named flow a model can call.
data Tool m where
  Tool :: {toolName :: Text, toolDescription :: Text, toolInput :: Codec i, toolOutput :: Codec o, toolBody :: Agentic m i o} -> Tool m

instance Category.Category (Agentic m) where
  id = Step Pass
  g . f = Seq f g

-- The overrides keep the structure visible to 'Agentic.Describe.describe'
-- instead of the defaults' plumbing through @arr swap@.
instance Arrow (Agentic m) where
  arr = Step . Arr
  first = First
  second = Split (Step Pass)
  (***) = Split
  f &&& g = Fanout f g

instance ArrowChoice (Agentic m) where
  left f = Choose (f >>> Step (Wrap Left)) (Step (Wrap Right))
  right f = Choose (Step (Wrap Left)) (f >>> Step (Wrap Right))
  f +++ g = Choose (f >>> Step (Wrap Left)) (g >>> Step (Wrap Right))
  f ||| g = Choose f g

-- | An LLM writes an @o@ from the step's input.
--
-- > draft @Joke "a joke please"
draft :: forall o i m. (Contract i, Contract o, Typeable i, Typeable o) => Instruction -> Agentic m i o
draft = draftWith @o []

-- | An LLM writes an @o@, calling the tools as often as it likes along the way.
draftWith :: forall o i m. (Contract i, Contract o, Typeable i, Typeable o) => [Tool m] -> Instruction -> Agentic m i o
draftWith tools instruction = Step (Draft (titledContract @i) (titledContract @o) instruction tools)

-- | A System One model answers questions about the step's input.
judge :: forall i o m. (Contract i, Typeable i) => Questions o -> Agentic m i o
judge = Step . Judge (titledContract @i)

-- | Plain code with an effect.
act :: (i -> m o) -> Agentic m i o
act = Step . Act

-- | A tool: a name and description for the model, and a flow to run.
tool :: forall i o m. (Contract i, Contract o, Typeable i, Typeable o) => Text -> Text -> Agentic m i o -> Tool m
tool name description = Tool name description (titledContract @i) (titledContract @o)

-- | A type's contract, with its schema named after the type if it isn't already.
titledContract :: forall a. (Contract a, Typeable a) => Codec a
titledContract = c {codecSchema = titled (T.pack (show (typeRep (Proxy @a)))) (codecSchema c)}
  where
    c = contract @a

-- | Map a flow over a list. The runtime may run the items concurrently.
each :: Agentic m a b -> Agentic m [a] [b]
each = Each

-- | Run a flow again and again on its own output until the condition holds.
-- The condition is checked first, so an input that already satisfies it is
-- returned unchanged. The model decides how many rounds it takes; name the
-- loop to say what it waits for:
--
-- > repeatUntil ended nextMove `named` "play until the game ends"
repeatUntil :: (a -> Bool) -> Agentic m a a -> Agentic m a a
repeatUntil = Repeat

-- | Name and describe a sub-flow.
note :: Text -> Text -> Agentic m i o -> Agentic m i o
note name description = Noted (Note name (if T.null description then Nothing else Just description))

-- | Name a flow. Written infix, it names exactly the expression before it,
-- because it binds as tightly as function application:
--
-- > draft @[Creature] "Name 10 prehistoric creatures"
-- >   >>> arr (partition clearDinosaur) `named` "keep the clear dinosaurs"
-- >   >>> ...
--
-- Bracket a larger sub-flow to name all of it.
named :: Agentic m i o -> Text -> Agentic m i o
f `named` name = Noted (Note name Nothing) f

infixl 9 `named`

-- | Keep the items where the probability of yes is at least @p@.
keep :: (Contract i, Typeable i) => Probability -> Questions YesNo -> Agentic m [i] [i]
keep p q =
  note ("keep " <> T.pack (show p)) "" $
    each (returnA &&& judge q)
      >>> arr (map fst . filter ((>= p) . yes . snd))

-- | Send the input 'Right' if the probability of yes is at least @p@, and
-- 'Left' otherwise.
gate :: (Contract i, Typeable i) => Probability -> Questions YesNo -> Agentic m i (Either i i)
gate p q =
  note ("gate " <> T.pack (show p)) "" $
    (returnA &&& judge q)
      >>> arr (\(x, a) -> if yes a >= p then Right x else Left x)
