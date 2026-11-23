-- | Completion journals for resuming a single workflow revision.
module Agentic.Journal
  ( Journal (..), JournalIdentity (..), JournalOperation (..)
  , JournalKey (..), JournalFrame (..), DurableKey (..)
  , JournalError (..), journalKeyValue, durableKeyValue, durableKeyText
  , canonicalValue
  ) where

import Agentic.Internal.Address (Address, addressText)
import Agentic.Value (Value (..), renderJson)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T

-- | The caller supplies both identities, including code/codec/provider changes
-- in the revision. Nested interpreters must use a separate run namespace.
data JournalIdentity = JournalIdentity {namespace :: Text, revision :: Text}
  deriving (Eq, Show)

-- | Lookup and save a fully encoded completion, scoped to 'identity'.
-- 'initialise' checks the configured identity against backing state. It is
-- called inside the root scope on each interpretation, before flow nodes or
-- effects. A backend may validate eagerly when opening instead.
--
-- Saves must finish before acknowledging success or publishing to a cache.
-- A completion is immutable: identical writes are idempotent, conflicting
-- writes fail. Errors never mean a miss. Serialise writes within a concurrent
-- runtime; the caller owns single-writer use across runtimes/processes.
-- Core supplies a separate @request@ fingerprint and @result@ in each record.
-- The fingerprint is the exact canonical request Value, not a hash. Draft turn
-- requests include all history, so storage grows quadratically with turn count.
-- A backend may compress its representation while preserving lookup semantics.
-- The backend states what durability it provides. Core forces/canonicalises
-- values before calling save; a backend must also validate values it loads.
data Journal m = Journal
  { identity :: JournalIdentity
  , initialise :: m ()
  , lookupRecord :: Value -> m (Maybe Value)
  , saveRecord :: Value -> Value -> m ()
  }

data JournalOperation = DurableEffect | ToolExecution | Judgement | AwaitedReply | DraftTurn Int
  deriving (Eq, Ord, Show)

-- | Unlike observation frames, element frames omit the list total. Indices
-- and all enclosing frames remain part of the key; list edits are not migrated.
data JournalFrame
  = ElementFrame Address Int
  | IterationFrame Address Int
  | InvocationFrame Address Int Int Text
  deriving (Eq, Ord, Show)

data JournalKey = JournalKey
  { operation :: JournalOperation, address :: Address, context :: [JournalFrame] }
  deriving (Eq, Ord, Show)

-- | A run-scoped idempotency key. External systems may or may not support it.
data DurableKey = DurableKey {namespace :: Text, key :: JournalKey}
  deriving (Eq, Ord, Show)

data JournalError
  = JournalMismatch Text
  | JournalDiverged JournalKey
  | JournalDecodeError JournalKey Text
  | JournalInvalidValue Text
  | JournalConflict Value
  | JournalUnreadable FilePath Int Text
  | JournalWriteFailed FilePath Text
  deriving (Eq, Show)

journalKeyValue :: JournalKey -> Value
journalKeyValue k = Object
  [("operation",op k.operation),("address",addr k.address)
  ,("context",Array (map frame k.context))]
 where
  addr = String . addressText
  number = Integer . toInteger
  op = \case
    DurableEffect -> Array [String "effect"]
    ToolExecution -> Array [String "tool"]
    AwaitedReply -> Array [String "wait"]
    Judgement -> Array [String "judge"]
    DraftTurn n -> Array [String "turn",number n]
  frame = \case
    ElementFrame a i -> Array [String "each",addr a,number i]
    IterationFrame a i -> Array [String "repeat",addr a,number i]
    InvocationFrame a t i c -> Array [String "call",addr a,number t,number i,String c]

durableKeyValue :: DurableKey -> Value
durableKeyValue k = Object [("run",String k.namespace),("key",journalKeyValue k.key)]

-- | Stable JSON text, not a short digest. Deeply nested keys can exceed a
-- service's idempotency-key limit; callers may hash this text for that service.
durableKeyText :: DurableKey -> Text
durableKeyText = renderJson . durableKeyValue

-- | Exact canonical request comparison rather than a lossy hash. Object order
-- and integral Number/Integer representations do not affect equality. Reject
-- duplicate object fields and non-finite numbers, which cannot round-trip
-- unambiguously through JSON. Force the whole value before returning Right.
canonicalValue :: Value -> Either Text Value
canonicalValue v = do
  c <- canonical v
  force c `seq` pure c
 where
  canonical = \case
    Object fs -> do
      let ordered = sortOn fst fs
      if any (uncurry (==)) (zip (map fst ordered) (drop 1 (map fst ordered)))
        then Left "duplicate JSON object field"
        else Object <$> traverse (\(k,x) -> (k,) <$> canonical x) ordered
    Array xs -> Array <$> traverse canonical xs
    Number n | isNaN n || isInfinite n -> Left "non-finite JSON number"
             | n == fromInteger (round n) -> Right (Integer (round n))
    x -> Right x
  force = \case
    Object fs -> foldr (\(k,x) z -> T.length k `seq` force x `seq` z) () fs
    Array xs -> foldr (\x z -> force x `seq` z) () xs
    String t -> T.length t `seq` ()
    Integer n -> n `seq` ()
    Number n -> n `seq` ()
    Bool b -> b `seq` ()
    Null -> ()
