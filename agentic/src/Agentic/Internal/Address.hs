module Agentic.Internal.Address where

import Data.Text (Text)
import Data.Text qualified as T

data Segment
  = SeqLeft | SeqRight | FanLeft | FanRight | SplitLeft | SplitRight
  | ChooseLeft | ChooseRight | FirstBody | EachBody | RepeatBody | NotedBody
  | ToolAt Int | MemberAt Int | RunRoot | CallScope
  deriving (Eq, Ord, Show)

-- | An opaque position within one captured flow, comparable with 'Eq'/'Ord'.
-- It is not an identity across code revisions.
newtype Address = Address [Segment] deriving (Eq, Ord)

instance Show Address where
  show = T.unpack . addressText

child :: Address -> Segment -> Address
child (Address path) s = Address (path <> [s])

-- | Injective encoding within a captured graph. Retain with that graph;
-- the format is opaque and has no stability guarantee across revisions.
addressText :: Address -> Text
addressText (Address path) = T.intercalate "/" ("flow" : map render path)
 where
  render = \case
    RunRoot -> "run"
    CallScope -> "call"
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


rootAddress :: Address
rootAddress = Address [RunRoot]

flowAddress :: Address
flowAddress = Address []
