{-# LANGUAGE GHC2021 #-}
module Agentic.Internal.SpikeTransformerOps
 (Work, pure, (>>=), (>>), (<$>), (<*>), lift, stop, run, pack, traverse) where
import Prelude
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
import Agentic.Internal.SpikeCommon

type Work m = ExceptT Waits m
stop :: Monad m => Waits -> Work m a
stop = throwE
run :: Work m a -> m (Either Waits a)
run = runExceptT
pack :: m (Either Waits a) -> Work m a
pack = ExceptT
