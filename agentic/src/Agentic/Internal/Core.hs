{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Flows: typed, inspectable descriptions of agentic work.
module Agentic.Internal.Core
  ( -- * Flows
    Agentic (..)
  , Step (..)
  , Tool (..)
  , Group (..)
  , concreteMembers
  , realTool
  , ToolTuple (..)
  , recursiveTools
  , recursiveTool
  , toolName
  , toolDescription
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
    -- * Plumbing
  , takeFirst
  , takeSecond
  , (:/\)
  , pattern (:/\)
    -- * Judgement helpers
  , keep
  , gate
    -- * Re-exports
  , module Control.Arrow
  ) where

import Agentic.Contract (Structure, Contract (..), nominal)
import Agentic.Questions (Probability, Questions, YesNo (..))
import Control.Arrow
import qualified Control.Category as Category
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

-- | What a model step is asked to do.
newtype Instruction = Instruction {text :: Text}
  deriving (Eq, Ord, Show)

instance IsString Instruction where
  fromString = Instruction . T.pack

-- | A name, and optionally a description, for a sub-flow. Notes are for whoever
-- is watching the flow, not for the model.
data Note = Note
  { name :: Text
  , description :: Maybe Text
  }
  deriving (Eq, Ord, Show)

-- | The leaves of a flow: the steps that do the work.
data Step m i o where
  Pass :: Step m i i
    -- ^ The input, unchanged: 'id' and 'returnA'.
  Wrap :: (i -> o) -> Step m i o
    -- ^ The input re-wrapped without changing it ('Left', 'Right'), so that
    -- 'Agentic.Describe.describe' can show it as a pass-through.
  Arr :: (i -> o) -> Step m i o
  TakeFirst :: Step m (a, b) a
    -- ^ 'takeFirst': unlike @arr fst@, 'Agentic.Describe.describe' can see
    -- which half it keeps.
  TakeSecond :: Step m (a, b) b
  Act :: (i -> m o) -> Step m i o
  Draft :: Structure i -> Structure o -> Instruction -> [Tool m] -> Step m i o
  Judge :: Structure i -> Questions o -> Step m i o

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
  -- | A name, a description for the model, the input and output contracts, and
  -- the flow to run.
  Tool :: Text -> Text -> Structure i -> Structure o -> Agentic m i o -> Tool m

  GroupMember :: Group m -> Int -> Tool m
  Reference :: Int -> Int -> Tool m -> Tool m

-- A group stores its builder. Static walks and execution supply different
-- representations of its members; only execution can expose tools to effects.
data Group m = Group Int ([Tool m] -> [Tool m])

-- | Supported recursive group shapes: one tool or tuples of two through five.
-- Methods are internal; authors use 'recursiveTool' or 'recursiveTools'.
class ToolTuple t m | t -> m where
  tupleSize :: proxy t -> Int
  toTools :: t -> [Tool m]
  fromTools :: [Tool m] -> t

instance ToolTuple (Tool m) m where
  tupleSize _ = 1
  toTools t = [t]
  fromTools ts = ts !! 0

instance ToolTuple (Tool m, Tool m) m where
  tupleSize _ = 2
  toTools (a, b) = [a, b]
  fromTools ts = (ts !! 0, ts !! 1)

instance ToolTuple (Tool m, Tool m, Tool m) m where
  tupleSize _ = 3
  toTools (a, b, c) = [a, b, c]
  fromTools ts = (ts !! 0, ts !! 1, ts !! 2)

instance ToolTuple (Tool m, Tool m, Tool m, Tool m) m where
  tupleSize _ = 4
  toTools (a, b, c, d) = [a, b, c, d]
  fromTools ts = (ts !! 0, ts !! 1, ts !! 2, ts !! 3)

instance ToolTuple (Tool m, Tool m, Tool m, Tool m, Tool m) m where
  tupleSize _ = 5
  toTools (a, b, c, d, e) = [a, b, c, d, e]
  fromTools ts = (ts !! 0, ts !! 1, ts !! 2, ts !! 3, ts !! 4)

-- | Define a finite group of mutually recursive tools. The builder receives
-- its own members; use them in tool bodies. Supports one tool or tuples of
-- two through five tools. For example:
--
-- > (ping, pong) = recursiveTools $ \(ping, pong) ->
-- >   (tool "ping" "Ping" (draftWith [pong] "ping"),
-- >    tool "pong" "Pong" (draftWith [ping] "pong"))
--
-- The builder must be pure and productive: return concrete 'tool' definitions,
-- and compute metadata without cyclic dependencies. Arguments can be passed
-- into bodies or inspected through 'toolName'/'toolDescription'. Do not tie
-- a separate Haskell knot around the returned tools; use the supplied members.
recursiveTools :: forall t m. ToolTuple t m => (t -> t) -> t
recursiveTools f = fromTools [GroupMember group k | k <- [0 .. n - 1]]
 where
  n = tupleSize (Proxy @t)
  group = Group n (toTools . f . fromTools)

-- | Define one self-recursive tool. Runtime recursion has no depth limit.
recursiveTool :: (Tool m -> Tool m) -> Tool m
recursiveTool = recursiveTools

-- Only concrete definitions may be returned by a binder. Check constructors
-- before reading metadata: an alias such as recursiveTool id must not chase
-- its own metadata indefinitely.
concreteMembers :: [Tool m] -> Either Text ()
concreteMembers = mapM_ $ \case
  Tool{} -> Right ()
  _ -> Left "recursiveTools must return tool definitions, not member aliases"

-- Runtime expansion supplies self-contained members, never static references.
realTool :: Tool m -> Either Text (Tool m)
realTool t@Tool{} = Right t
realTool (GroupMember group@(Group n f) k) = do
  let defs = f [GroupMember group j | j <- [0 .. n - 1]]
  concreteMembers defs
  pure (defs !! k)
realTool Reference{} = Left "a static tool reference reached execution"

-- Metadata may be inspected before preflight. Aliases get a clear error;
-- arbitrary cyclic computations of a name remain partial Haskell values.
-- Return only metadata, so no body containing these temporary references can
-- accidentally be executed or passed to a static walk.
metadata :: Tool m -> (Text, Text)
metadata (Tool name description _ _ _) = (name, description)
metadata (Reference _ _ t) = metadata t
metadata (GroupMember (Group n f) k) =
  case concreteMembers defs of
    Left problem -> error (T.unpack problem)
    Right () -> case defs !! k of
      Tool name description _ _ _ -> (name, description)
      _ -> error "metadata: expected concrete tool"
 where
  defs = f [Reference 0 j (defs !! j) | j <- [0 .. n - 1]]

toolName :: Tool m -> Text
toolName = fst . metadata

toolDescription :: Tool m -> Text
toolDescription = snd . metadata

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

-- | The first half of a pair. It does what @arr fst@ does, but a diagram can
-- follow it: after @&&&@ or @***@, it knows which step the half came from.
takeFirst :: Agentic m (a, b) a
takeFirst = Step TakeFirst

-- | The second half of a pair, like 'takeFirst'.
takeSecond :: Agentic m (a, b) b
takeSecond = Step TakeSecond

infixr 6 :/\

-- | A pair, written so that the nested pairs @&&&@ and @***@ build read flat:
-- @a &&& b &&& c@ gives an @A :\/\\ B :\/\\ C@, which is @(A, (B, C))@.
type a :/\ b = (a, b)

-- | Build or match a pair the same way:
--
-- > arr (\(creature :/\ picture :/\ card) -> Entry creature picture card)
pattern (:/\) :: a -> b -> (a, b)
pattern a :/\ b = (a, b)

{-# COMPLETE (:/\) #-}

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

-- | Preserve a step's nominal type identity, including handwritten contracts.
titledContract :: forall a. (Contract a, Typeable a) => Structure a
titledContract = nominal (contract @a)

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
      >>> arr (map fst . filter ((>= p) . (.yes) . snd))

-- | Send the input 'Right' if the probability of yes is at least @p@, and
-- 'Left' otherwise.
gate :: (Contract i, Typeable i) => Probability -> Questions YesNo -> Agentic m i (Either i i)
gate p q =
  note ("gate " <> T.pack (show p)) "" $
    (returnA &&& judge q)
      >>> arr (\(x, a) -> if a.yes >= p then Right x else Left x)
