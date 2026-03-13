module Agentic.Core
  ( Agent
  , fanoutMap
  , sequential
  ) where

import Control.Monad ((>=>))
import Effectful (Eff)
import UnliftIO.Async (mapConcurrently)

-- | An agent is a plain effectful function from input to output.
type Agent es a b = a -> Eff es b

-- | Map an agent over a list, running each element concurrently.
fanoutMap :: Agent es a b -> Agent es [a] [b]
fanoutMap agent inputs = mapConcurrently agent inputs

-- | Sequential composition: feed output of first agent into second.
-- Equivalent to @(>=>)@ — use either form.
sequential :: Agent es a b -> Agent es b c -> Agent es a c
sequential = (>=>)
