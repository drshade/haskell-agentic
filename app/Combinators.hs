module Combinators where
import           Agentic
import           Control.Arrow (Kleisli (..), (>>>))
import           Control.Monad (join)
import           Data.Text     (Text)
import           Dhall         (FromDhall, ToDhall)

-- Glue two arrows together, injecting the schema of the type required as input to second arrow
-- to the prompt given by the first (including parsing or failing of that type)
(>...>) :: forall a b c m. (FromDhall b, ToDhall b) => Agentic m a Text -> Agentic m b c -> Agentic m a c
(>...>) l r = l >>> extract @b >>> r

-- Fanout
(**.**) :: forall a b s m. (FromDhall b, ToDhall b, FromDhall s, ToDhall s) => Agentic m a [s] -> Agentic m s b -> Agentic m a [b]
(**.**) l r = Kleisli $ \input -> do
    tasks :: [s] <- runAgentic l input

    -- mapM         :: (a -> m b) -> t a -> m (t b)
    -- runAgentic   :: Kleisli m a b -> a -> m b
    results :: [b] <- mapM (runAgentic r) tasks

    pure results
