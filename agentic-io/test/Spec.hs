module Main (main) where

import Agentic
import Agentic.IO
import Agentic.Scripted (alwaysYes, replyingWith, respond)
import Control.Exception (try)
import Data.IORef
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Directory (getTemporaryDirectory, removeFile, doesFileExist)
import System.FilePath ((</>))
import Test.Hspec hiding (describe)
import qualified Test.Hspec

data Joke = Joke {setup :: Text, punchline :: Text}
  deriving (Generic, Show, Eq, Contract)

data Groan = Mild | Solid | Unbearable
  deriving (Generic, Show, Eq, Options)

joke :: Joke
joke = Joke "Why was the scarecrow promoted?" "He was outstanding in his field."

-- | A flow with one draft and one judgement.
flow :: Agentic IO Text (Joke, (YesNo, Choice Groan, Score Groan))
flow = draft @Joke "a joke about this" >>> (returnA &&& judge ((,,) <$> yesNo "Is it funny?" <*> choice "Reaction?" <*> score "Groaning?"))

-- | A runtime whose models count their calls.
counting :: IO (Runtime IO, IORef Int, IORef Int)
counting = do
  turns <- newIORef 0
  judgements <- newIORef 0
  let SystemTwo two = replyingWith (const (respond joke))
      SystemOne one = alwaysYes 0.8
  pure
    ( runtime
        { systemTwo = SystemTwo (\c -> modifyIORef turns (+ 1) >> two c)
        , systemOne = SystemOne (\r -> modifyIORef judgements (+ 1) >> one r)
        }
    , turns
    , judgements
    )

-- | A runtime whose models must not be called.
offline :: Runtime IO
offline =
  runtime
    { systemTwo = SystemTwo (const (fail "the model was called"))
    , systemOne = SystemOne (const (fail "Jev was called"))
    }

fresh :: String -> IO FilePath
fresh name = do
  dir <- getTemporaryDirectory
  let file = dir </> name
  exists <- doesFileExist file
  if exists then removeFile file else pure ()
  pure file

main :: IO ()
main = hspec $ Test.Hspec.describe "withStore" $ do
  it "records, then replays without calling the models" $ do
    file <- fresh "agentic-store-replay.jsonl"
    (live, turns, judgements) <- counting
    recording <- withStore Record file live
    recorded <- interpret recording flow "scarecrows"
    (,) <$> readIORef turns <*> readIORef judgements `shouldReturn` (1, 1)
    replaying <- withStore Replay file offline
    interpret replaying flow "scarecrows" `shouldReturn` recorded

  it "fails clearly when a replay asks something new" $ do
    file <- fresh "agentic-store-miss.jsonl"
    (live, _, _) <- counting
    recording <- withStore Record file live
    _ <- interpret recording flow "scarecrows"
    replaying <- withStore Replay file offline
    result <- try (interpret replaying flow "penguins")
    either (\(StoreMiss _ what) -> what) (const "no miss") result `shouldBe` "a turn of: a joke about this"

  it "replays what it has and records what it doesn't" $ do
    file <- fresh "agentic-store-both.jsonl"
    (live, turns, _) <- counting
    store <- withStore ReplayOrRecord file live
    _ <- interpret store flow "scarecrows"
    _ <- interpret store flow "scarecrows"
    readIORef turns `shouldReturn` 1
    _ <- interpret store flow "penguins"
    readIORef turns `shouldReturn` 2
    again <- withStore ReplayOrRecord file offline
    _ <- interpret again flow "penguins"
    pure ()
