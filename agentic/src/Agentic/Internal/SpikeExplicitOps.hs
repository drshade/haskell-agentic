{-# LANGUAGE GHC2021 #-}
module Agentic.Internal.SpikeExplicitOps where
import Prelude hiding (pure, (>>=), (>>), (<$>), (<*>))
import Prelude qualified as P
import Agentic.Internal.SpikeCommon

type Work m a = m (Either Waits a)
pure :: Monad m => a -> Work m a
pure = P.pure . Right
(>>=) :: Monad m => Work m a -> (a -> Work m b) -> Work m b
x >>= f = x P.>>= either (P.pure . Left) f
(>>) :: Monad m => Work m a -> Work m b -> Work m b
x >> y = x >>= const y
(<$>) :: Monad m => (a -> b) -> Work m a -> Work m b
f <$> x = x >>= pure . f
(<*>) :: Monad m => Work m (a -> b) -> Work m a -> Work m b
f <*> x = f >>= (\g -> g <$> x)
lift :: Monad m => m a -> Work m a
lift x = Right P.<$> x
stop :: Monad m => Waits -> Work m a
stop = P.pure . Left
run :: Work m a -> m (Either Waits a)
run = id
pack :: m (Either Waits a) -> Work m a
pack = id
traverse :: Monad m => (a -> Work m b) -> [a] -> Work m [b]
traverse f = foldr (\a rest -> f a >>= \b -> (b:) <$> rest) (pure [])
infixl 1 >>=, >>
infixl 4 <$>, <*>
