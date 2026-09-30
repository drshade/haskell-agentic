-- | Running a flow's independent work at the same time.
module Agentic.IO.Concurrent
  ( concurrently
  ) where

import Agentic.Runtime (Runtime (..))
import Control.Concurrent.Async (mapConcurrently)

-- | Run 'Agentic.Core.each', '&&&' and parallel tool calls concurrently. If one
-- branch fails, the others are cancelled and the error is raised in the caller.
concurrently :: Runtime IO -> Runtime IO
concurrently rt = rt {parallel = mapConcurrently id}
