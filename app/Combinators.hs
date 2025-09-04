module Combinators where
import           Agentic
import           Control.Arrow  (Kleisli (..), (>>>))
import           Dhall          (FromDhall, ToDhall)
import           UnliftIO       (MonadUnliftIO)
import           UnliftIO.Async (mapConcurrently)

-- Glue two arrows together, injecting the schema of the type required as input to second arrow
-- to the prompt given by the first (including parsing or failing of that type)
(>...>) :: forall a b c m. (FromDhall b, ToDhall b) => Agentic m a Prompt -> Agentic m b c -> Agentic m a c
(>...>) l r = l >>> extract @b >>> r

-- Fanout
(<<.>>) :: forall a b s m. (MonadUnliftIO m, FromDhall b, ToDhall b, FromDhall s, ToDhall s) => Agentic m a [s] -> Agentic m s b -> Agentic m a [b]
(<<.>>) l r = Kleisli $ \input -> do
    tasks :: [s] <- run l input
    results :: [b] <- mapConcurrently (run r) tasks
    pure results


