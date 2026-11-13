-- | A finite, effect-free view of a flow. References exist only in this walk;
-- execution expands real tools and follows this view as its structural cursor.
module Agentic.Internal.Static where

import Agentic.Contract (Structure, ProjectionError (..), compileModel)
import Agentic.Internal.Core
import Agentic.Questions (Questions)
import Agentic.Runtime (FlowError (..))
import Data.Text (Text)
import Data.Text qualified as T

data Segment
  = SeqLeft | SeqRight | FanLeft | FanRight | SplitLeft | SplitRight
  | ChooseLeft | ChooseRight | FirstBody | EachBody | RepeatBody | NotedBody
  | ToolAt Int | MemberAt Int
  deriving (Eq, Ord, Show)

-- | An opaque position within one captured flow, comparable with 'Eq'/'Ord'.
-- It is not an identity across code revisions.
newtype Address = Address [Segment] deriving (Eq, Ord)

instance Show Address where
  show = T.unpack . addressText

toolNestingLimit :: Int
toolNestingLimit = 100

child :: Address -> Segment -> Address
child (Address path) s = Address (path <> [s])

-- | Render a capture-local identifier for display or serialization. The text
-- format is unspecified and may change; retain it with its captured description.
addressText :: Address -> Text
addressText (Address path) = T.intercalate "/" (map render path)
 where
  render = \case
    SeqLeft -> "seq/0"
    SeqRight -> "seq/1"
    FanLeft -> "fan/0"
    FanRight -> "fan/1"
    SplitLeft -> "split/0"
    SplitRight -> "split/1"
    ChooseLeft -> "choose/0"
    ChooseRight -> "choose/1"
    FirstBody -> "first"
    EachBody -> "each"
    RepeatBody -> "repeat"
    NotedBody -> "note"
    ToolAt k -> "tool/" <> T.pack (show k)
    MemberAt k -> "member/" <> T.pack (show k)

data StaticFlow
  = SStep StaticStep
  | SSeq StaticFlow StaticFlow
  | SFanout StaticFlow StaticFlow
  | SSplit StaticFlow StaticFlow
  | SFirst StaticFlow
  | SChoose StaticFlow StaticFlow
  | SEach StaticFlow
  | SRepeat StaticFlow
  | SNoted Note StaticFlow

data StaticStep where
  SIdentity :: StaticStep
  SGlue :: StaticStep
  SFirstHalf :: StaticStep
  SSecondHalf :: StaticStep
  SEffect :: StaticStep
  SDraft :: Structure i -> Structure o -> Instruction -> [StaticTool] -> StaticStep
  SJudge :: Structure i -> Questions o -> StaticStep

data StaticTool where
  StaticTool :: Address -> Text -> Text -> Structure i -> Structure o -> ToolBody -> StaticTool

data ToolBody
  = Body StaticFlow
  | Members Int [StaticTool]
  | ReferenceTo Address

data Frame m = Frame Address [Tool m]
type Env m = [Frame m]

-- | Bound static tool nesting, not combinator depth or runtime calls. This is
-- a deliberately finite limit, not detection of arbitrary Haskell cycles.
staticFlow :: Agentic m i o -> Either FlowError StaticFlow
staticFlow = walk 0 [] (Address [])

walk :: forall m i o. Int -> Env m -> Address -> Agentic m i o -> Either FlowError StaticFlow
walk depth env address = \case
  Step s -> SStep <$> case s of
    Pass -> pure SIdentity
    Wrap _ -> pure SIdentity
    Arr _ -> pure SGlue
    TakeFirst -> pure SFirstHalf
    TakeSecond -> pure SSecondHalf
    Act _ -> pure SEffect
    Draft i o instruction ts -> SDraft i o instruction <$> traverse (\(k,t) -> walkTool depth env (child address (ToolAt k)) t) (zip [0..] ts)
    Judge i qs -> pure (SJudge i qs)
  Seq f g -> SSeq <$> descend SeqLeft f <*> descend SeqRight g
  Fanout f g -> SFanout <$> descend FanLeft f <*> descend FanRight g
  Split f g -> SSplit <$> descend SplitLeft f <*> descend SplitRight g
  Choose f g -> SChoose <$> descend ChooseLeft f <*> descend ChooseRight g
  First f -> SFirst <$> descend FirstBody f
  Each f -> SEach <$> descend EachBody f
  Repeat _ f -> SRepeat <$> descend RepeatBody f
  Noted n f -> SNoted n <$> descend NotedBody f
 where
  descend :: Segment -> Agentic m a b -> Either FlowError StaticFlow
  descend s = walk depth env (child address s)

walkTool :: Int -> Env m -> Address -> Tool m -> Either FlowError StaticTool
walkTool depth env address = \case
  t@Tool{} -> definition env address t
  GroupMember (Group n f) selected -> do
    let level = length env
        defs = f [Reference level k (defs !! k) | k <- [0 .. n - 1]]
        env' = env <> [Frame address defs]
    either (Left . InvalidRecursion) pure (concreteMembers defs)
    members <- traverse (\(k,t) -> definition env' (child address (MemberAt k)) t) (zip [0..] defs)
    case members !! selected of
      StaticTool _ name doc i o _ -> pure (StaticTool address name doc i o (Members selected members))
  Reference level k _ -> case drop level env of
    Frame target defs : _ | k >= 0, k < length defs -> case defs !! k of
      Tool name doc i o _ -> pure (StaticTool address name doc i o (ReferenceTo (child target (MemberAt k))))
      _ -> Left (InvalidRecursion "a reference targets a non-definition")
    _ -> Left (InvalidRecursion "a recursive reference is outside its binder")
 where
  definition env' target (Tool name doc i o body)
    | depth >= toolNestingLimit = Left (ToolNestingLimit toolNestingLimit)
    | otherwise = StaticTool target name doc i o . Body <$> walk (depth + 1) env' target body
  definition _ _ _ = Left (InvalidRecursion "expected a concrete tool definition")

toolTarget :: StaticTool -> Address
toolTarget (StaticTool address _ _ _ _ body) = case body of
  Body _ -> address
  Members k ts -> toolTarget (ts !! k)
  ReferenceTo target -> target

-- | Follow a structural path in the finite tree instead of scanning a flat
-- route table. The cost is proportional to path depth and tool-list indexing.
bodyAt :: StaticFlow -> Address -> Maybe StaticFlow
bodyAt root (Address path) = go root path
 where
  go f [] = Just f
  go f (s : rest) = case (f, s) of
    (SSeq l _, SeqLeft) -> go l rest
    (SSeq _ r, SeqRight) -> go r rest
    (SFanout l _, FanLeft) -> go l rest
    (SFanout _ r, FanRight) -> go r rest
    (SSplit l _, SplitLeft) -> go l rest
    (SSplit _ r, SplitRight) -> go r rest
    (SChoose l _, ChooseLeft) -> go l rest
    (SChoose _ r, ChooseRight) -> go r rest
    (SFirst b, FirstBody) -> go b rest
    (SEach b, EachBody) -> go b rest
    (SRepeat b, RepeatBody) -> go b rest
    (SNoted _ b, NotedBody) -> go b rest
    (SStep (SDraft _ _ _ ts), ToolAt k) -> at ts k >>= (`inside` rest)
    _ -> Nothing
  inside (StaticTool _ _ _ _ _ body) rest = case (body, rest) of
    (Body f, _) -> go f rest
    (Members _ ts, MemberAt k : remaining) -> at ts k >>= (`inside` remaining)
    _ -> Nothing
  at xs k = case drop k xs of
    x : _ | k >= 0 -> Just x
    _ -> Nothing

-- | Model-specific validation is separate from structural discovery. Every
-- concrete member is validated once per static group entry, including unused
-- members. A reference neither repeats validation nor expands its target.
validate :: StaticFlow -> Either FlowError ()
validate = go []
 where
  check :: [Note] -> Text -> Structure a -> Either FlowError ()
  check notes label c = case compileModel c of
    Left (ProjectionError path problem) -> Left (UnsupportedContract notes (ProjectionError (label : path) problem))
    Right _ -> Right ()
  go notes = \case
    SStep (SDraft i o _ ts) -> check notes "draft.input" i >> check notes "draft.output" o >> mapM_ (toolCheck notes) ts
    SStep (SJudge i _) -> check notes "judge.input" i
    SStep _ -> Right ()
    SSeq l r -> both l r
    SFanout l r -> both l r
    SSplit l r -> both l r
    SChoose l r -> both l r
    SFirst b -> go notes b
    SEach b -> go notes b
    SRepeat b -> go notes b
    SNoted n b -> go (notes <> [n]) b
   where both l r = go notes l >> go notes r
  toolCheck notes (StaticTool _ name _ i o body) = case body of
    Members _ ts -> mapM_ (toolCheck notes) ts
    ReferenceTo _ -> Right ()
    Body f ->
      let path = notes <> [Note name Nothing]
       in check path "tool.input" i >> check path "tool.output" o >> go path f
