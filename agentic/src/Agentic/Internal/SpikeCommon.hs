{-# LANGUAGE GHC2021, DuplicateRecordFields, NoFieldSelectors, OverloadedRecordDot #-}
module Agentic.Internal.SpikeCommon where
import Agentic.Journal
import Agentic.Value
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (sconcat)

data Pending = Pending JournalIdentity JournalKey Value deriving (Eq, Show)
type Waits = NonEmpty Pending
collect :: [Either Waits a] -> Either Waits [a]
collect xs = case [w | Left w <- xs] of
  [] -> Right [a | Right a <- xs]
  w:ws -> Left (sconcat (w :| ws))
