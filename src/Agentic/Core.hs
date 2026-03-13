module Agentic.Core
  ( Agent
  , fanoutMap
  , sequential
  ) where

import Control.Monad ((>=>))
import Effectful (Eff)
import UnliftIO (mapConcurrently)

-- | An agent is a plain effectful function from input to output.
type Agent es a b = a -> Eff es b

-- | Map an agent over a list, running each element concurrently.
fanoutMap :: Agent es a b -> Agent es [a] [b]
fanoutMap agent inputs = mapConcurrently agent inputs

-- | Sequential composition: feed output of first agent into second.
-- Prefer using (>=>) from Control.Monad directly in your code.
sequential :: Agent es a b -> Agent es b c -> Agent es a c
sequential = (>=>)
