{-# LANGUAGE CPP #-}
module Main (main) where

import Agentic hiding (key, record)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM)
import Agentic.Codec qualified as C
import Agentic.Schema (Shape (..))
import Data.Map.Strict qualified as Map
#ifdef HAS_AWAIT
import Data.List.NonEmpty qualified as NE
#endif
import Data.List (sort)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (getRTSStats, RTSStats (..))
import System.Mem (performGC)
import Text.Printf (printf)

-- Compile this same source without HAS_AWAIT against a main checkout to obtain
-- a paired baseline. Store setup and warm-up are outside the timed region.
main :: IO ()
main = do
  putStrLn "case,median_ms,allocated_MB (5000 elements; median 7; GHC -O1; RTS -T)"
  measure "no-journal-cheap" (pure runtime) (completed cheap)
  forM_ [False,True] $ \parallel -> do
    let name = if parallel then "concurrent" else "sequential"
        fresh = memoryRuntime parallel
        warm = do
          rt <- fresh
          _ <- interpret rt durable [1..5000]
          pure rt
    measure (name <> "-journal-cheap") fresh (completed cheap)
    measure (name <> "-durable-miss") fresh (completed durable)
    measure (name <> "-durable-hit") warm (completed durable)
#ifdef HAS_AWAIT
    measure (name <> "-waiting") fresh $ \rt ->
      resumable rt (each (await intCodec intCodec)) [1..5000] >>= \case
        RunWaiting waits -> evaluate (NE.length waits)
        RunFinished _ -> error "expected pending waits"
#endif
 where
  completed flow rt = interpret rt flow [1..5000] >>= evaluate . sum

cheap :: Agentic IO [Int] [Int]
cheap = each ((arr (+1) `named` "increment") >>> arr (*2))

durable :: Agentic IO [Int] [Int]
durable = each (durably intCodec intCodec (\_ -> pure . (+1)))

intCodec :: C.Codec Int
intCodec = C.primitive SInteger (Integer . toInteger) $ \case
  Integer n -> Right (fromInteger n)
  _ -> Left "expected integer"

memoryRuntime :: Bool -> IO (Runtime IO)
memoryRuntime parallel = do
  cell <- newMVar Map.empty
  let journal = Journal (JournalIdentity "benchmark" "r1") (pure ())
        (\key -> withMVar cell (pure . Map.lookup key))
        (\key record -> modifyMVar_ cell $ \records -> case Map.lookup key records of
          Just old | old /= record -> failWith runtime (JournalFailure (JournalConflict key))
          _ -> pure (Map.insert key record records))
      rt = runtime{journal=Just journal}
  pure (if parallel then rt{parallel=mapConcurrently id} else rt)

measure :: String -> IO (Runtime IO) -> (Runtime IO -> IO Int) -> IO ()
measure label setup action = do
  samples <- replicateM 7 $ do
    rt <- setup
    performGC
    before <- getRTSStats
    start <- getMonotonicTimeNSec
    _ <- action rt
    end <- getMonotonicTimeNSec
    performGC
    after <- getRTSStats
    pure (fromIntegral (end-start)/1000000::Double,
      fromIntegral (allocated_bytes after-allocated_bytes before)/1000000::Double)
  printf "%s,%.3f,%.3f\n" label (sort (map fst samples) !! 3) (sort (map snd samples) !! 3)
