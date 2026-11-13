module Main (main) where

import Agentic
import Control.Exception (bracket, evaluate)
import Control.Monad (replicateM)
import Data.IORef
import Data.List (sort)
import Data.Text.IO qualified as T
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO
import Text.Printf (printf)

flow :: Agentic IO [Int] [Int]
flow = each ((arr (+1) `named` "increment") >>> arr (*2))

main :: IO ()
main = do
  putStrLn "10,000 elements; each body = named arr (+1) >>> arr (*2); median of 5 runs"
  measure "no observer" (pure runtime)
  measure "strict summary + count" $ do
    count <- newIORef (0::Int)
    pure (observing (\e -> evaluate (summarise e) >> modifyIORef' count (+1)) runtime)
  temp <- getTemporaryDirectory
  bracket (openTempFile temp "agentic-observation-bench.jsonl")
    (\(path,h) -> hClose h >> removeFile path) $ \(_,h) ->
      measure "summary JSON + append/flush per event" (pure (observing (\e -> T.hPutStrLn h (renderJson (summaryValue (summarise e))) >> hFlush h) runtime))
  count <- newIORef (0::Int)
  _ <- interpret (observing (\_ -> modifyIORef' count (+1)) runtime) flow [1..10000]
  readIORef count >>= printf "Events per run: %d\n"
  putStrLn "File flush is a local stand-in, not Kyyn guest/host RPC or fsync. It excludes transport latency."
 where
  measure label setup = do
    times <- replicateM 5 $ do
      rt <- setup
      begin <- getMonotonicTimeNSec
      result <- interpret rt flow [1..10000]
      _ <- evaluate (sum result)
      end <- getMonotonicTimeNSec
      pure (fromIntegral (end-begin) / 1000000 :: Double)
    printf "%s: %.3f ms\n" (label :: String) (sort times !! 2)
