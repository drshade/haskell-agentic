module Progress where

import           Agentic                  (Environment (..), Events, State (..))
import           Control.Arrow            (Kleisli (..))
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception        (bracket)
import           Control.Monad            (when)
import           Control.Monad.RWS        (RWST, runRWST)
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text, pack)
import qualified Data.Text                as T
import           LLM.Provider             (LLMConfig (..), defaultConfig)
import           System.Console.ANSI
import           System.Environment       (lookupEnv)
import           System.IO                (hFlush, stdout)

data Status = Started | InProgress | Completed | Failed Text
  deriving (Show)

-- Simple progress that works in GHCI
data SimpleProgress = SimpleProgress
  { spCurrent :: TVar Text
  , spRunning :: TVar Bool
  , spSpinner :: TVar Int
  }

newSimpleProgress :: IO SimpleProgress
newSimpleProgress = do
  current <- newTVarIO ""
  running <- newTVarIO False
  spinner <- newTVarIO 0
  return $ SimpleProgress current running spinner

-- Non-blocking progress display
startProgressDisplay :: SimpleProgress -> IO (Async ())
startProgressDisplay sp = async $ do
  let (SimpleProgress _ running _) = sp
  atomically $ writeTVar running True
  -- Draw initial line
  initialProgressDisplay sp
  -- Then start the spinner animation loop
  progressLoop sp

progressLoop :: SimpleProgress -> IO ()
progressLoop sp@(SimpleProgress _ runningTVar spinnerTVar) = do
  running <- readTVarIO runningTVar
  when running $ do
    spinner <- showSpinner spinnerTVar
    -- Backspace over the spinner character, redraw it, then move cursor to end
    putStr $ "\b\b" <> spinner <> " "  -- Add a space to move cursor away from spinner
    hFlush stdout

    threadDelay 75000  -- 50ms for smoother animation
    progressLoop sp

-- Initial display setup
initialProgressDisplay :: SimpleProgress -> IO ()
initialProgressDisplay (SimpleProgress currentTVar _ spinnerTVar) = do
  current <- readTVarIO currentTVar
  spinner <- showSpinner spinnerTVar
  -- Hide cursor and draw the full line initially
  hideCursor
  putStr $ "[LLM] " <> T.unpack current <> " " <> spinner <> " "
  hFlush stdout

showSpinner :: TVar Int -> IO String
showSpinner spinnerTVar = do
  count <- atomically $ do
    current <- readTVar spinnerTVar
    let next = (current + 1) `mod` 10
    writeTVar spinnerTVar next
    return current
  let frames = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
  return [frames !! count]

-- Update progress
updateSimpleProgress :: SimpleProgress -> Text -> Status -> IO ()
updateSimpleProgress (SimpleProgress currentTVar _ _) step _status = atomically $ do
  writeTVar currentTVar step

-- Stop progress
stopProgress :: SimpleProgress -> Async () -> IO ()
stopProgress (SimpleProgress _ runningTVar _) progressAsync = do
  -- Always restore cursor and clean up, even if interrupted
  atomically $ writeTVar runningTVar False
  cancel progressAsync
  putStr "\r"
  clearLine
  showCursor  -- Always restore cursor
  -- Don't print "Complete!" if we were interrupted - just clean up silently

-- GHCI-friendly runner
runWithProgress :: Kleisli (RWST Environment Events State IO) Text b -> Text -> IO b
runWithProgress agent input = do
  sp <- newSimpleProgress

  -- Set initial status before starting display
  updateSimpleProgress sp "Processing request..." InProgress

  -- Build the environment (same as runIO in Agentic.hs)
  apiKey' <- fromMaybe "" <$> lookupEnv "ANTHROPIC_KEY"
  let config      = defaultConfig { apiKey = pack apiKey' }
      environment = Environment { llmConfig = config, initialInput = input }
      state       = State ()

  -- Use bracket to ensure cleanup happens even on interrupts
  (result, logs) <- bracket
    (startProgressDisplay sp)  -- setup: start progress display
    (stopProgress sp)          -- cleanup: stop progress display
    (\_ -> do                  -- action: run the agent
      (a, _finalState, evts) <- runRWST (runKleisli agent input) environment state
      return (a, evts))

  -- Show completion with LLM call count
  let callCount = length logs
      callWord  = if callCount == 1 then "LLM call" else "LLM calls"
  putStrLn $ "(" <> show callCount <> " " <> callWord <> ")"
  return result

