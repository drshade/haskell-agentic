-- | Arrow combinators for composing agents.
module Combinators where
import           Agentic
import           Control.Arrow                (Kleisli (..), (>>>))
import           Protocol.Class               (SchemaConstraint, SchemaFormat,
                                               extractWith)
import           UnliftIO.Async               (mapConcurrently)

-- | Schema-injecting sequential composition.
-- Compose two arrows where the left produces a @Prompt@ and the right
-- consumes @b@. The schema for @b@ is injected into the prompt automatically
-- using the format @fmt@.
--
-- Example:
--
-- @
-- myPipeline = describeTask >...> \@Dhall executeTask
-- @
(>...>) :: forall fmt a b c m. (SchemaFormat fmt, SchemaConstraint fmt b, AgenticRWS m)
        => Agentic m a Prompt -> Agentic m b c -> Agentic m a c
(>...>) l r = l >>> extractWith @fmt @b >>> r

-- | Concurrent fanout.
-- Applies the right agent to each element of the list produced by the left,
-- running all applications concurrently.
(<<.>>) :: forall a b s m. AgenticRWS m => Agentic m a [s] -> Agentic m s b -> Agentic m a [b]
(<<.>>) l r = Kleisli $ \input -> do
    tasks :: [s] <- run l input
    results :: [b] <- mapConcurrently (run r) tasks
    pure results
