module Examples where


import           Agentic                (AgenticRWS, extract, extractWithRetry,
                                         inject, prompt, run, runAgentic)
import           Control.Arrow          (Kleisli (Kleisli), (>>>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Loops    (iterateUntilM)
import           Control.Monad.RWS      (MonadIO)
import           Data.Text
import           Dhall                  (FromDhall, ToDhall)
import           GHC.Generics           (Generic)
import           Prelude                hiding (show)
import           UnliftIO               (MonadUnliftIO)
import           UnliftIO.Async

data Joke = Joke
    { genre     :: Text
    , setup     :: Text
    , punchline :: Text
    }
    deriving (Generic, Show, FromDhall, ToDhall)

newtype Dog = Dog { name :: String }
    deriving (Generic, Show, FromDhall, ToDhall)

data BetterJoke
    = DadJoke { setup :: Text, punchline :: Text }
    | OneLiner { line :: Text }
    | Story { paragraphs :: [Text] }
    | KnockKnock { whosThere :: Text, punchline :: Text }
    deriving (Generic, Show, FromDhall, ToDhall)

data MaritalStatus = Unmarried | Married | Widowed
    deriving (Generic, Show, FromDhall, ToDhall)

data ApplicationForm = ApplicationForm
    { name          :: Text
    , age           :: Int
    , maritalStatus :: MaritalStatus
    }
    deriving (Generic, Show, FromDhall, ToDhall)

data TaskStatus = Todo | InProgress | Failed { reason :: Text } | Completed { result :: Text }
    deriving (Generic, Show, FromDhall, ToDhall, Eq)

data Task = Task
    { title       :: Text
    , description :: Maybe Text
    , status      :: TaskStatus
    }
    deriving (Generic, Show, FromDhall, ToDhall)

data Dino = Dino
    { name        :: Text
    , description :: Text
    , asciiPic    :: Text
    }
    deriving (Generic, Show, FromDhall, ToDhall)

dinoProject :: forall a m. (AgenticRWS m, MonadUnliftIO m) => Kleisli m a [Dino]
dinoProject = Kleisli $ \_ -> do
    names <- runAgentic (prompt >>> extract @[Text]) "Suggest 3 dinosaur names for my grade 5 project"
    dinos <- mapConcurrently (\name -> runAgentic (prompt >>> inject name >>> extract @Dino) "Do the research on this dinosaur") names
    pure dinos

printDinos :: AgenticRWS m => Kleisli m [Dino] ()
printDinos = Kleisli $ \dinos -> do
    mapM_ (\dino -> do
            liftIO $ do
                putStrLn $ unpack dino.name
                putStrLn $ unpack dino.description
                putStrLn $ unpack dino.asciiPic
        ) dinos

runProject :: IO ()
runProject = run (dinoProject >>> printDinos) "run!"

withTasks :: AgenticRWS m => Kleisli m Text Text
withTasks = Kleisli $ \input -> do
    let instruction = "Goal: \n" <> input
            <> "\n\n" <> "Return the list of tasks required to complete this, and I will call you back with each individual task"

    tasks <- runAgentic (prompt >>> extract @[Task]) instruction

    liftIO $ do
        print "Returned tasks"
        mapM_ (\task -> print $ "  " <> task.title <> " " <> show task.status) tasks

    completedTasks <-
        mapM (\task -> do

                liftIO $ print $ "  Working on " <> task.title <> "..."

                let taskInstruction = "Complete the following task:\n"
                                        <> show task <> "\n\nThis is part of the overall goal: \n"
                                        <> input <> "\n\n"
                                        <> "Subtask: " <> show task
                                        <> "\n\n" <> "Work only on the subtask and return this, I will bring all the results back to you for consolidation once all tasks are completed"
                                        <> "\n\n" <> "You should return the updated task in either Failed or Completed state (do not leave it as Todo or InProgress)"
                updatedTask <- runAgentic (prompt >>> extractWithRetry @Task) taskInstruction
                liftIO $ print $ show updatedTask
                pure updatedTask
              ) tasks

    liftIO $ do
        print "Completed Tasks:"
        print $ show completedTasks

    pure "hello"

data Space = Blank | X | O
    deriving (Generic, Show, FromDhall, ToDhall)

data Row = Row Space Space Space
    deriving (Generic, Show, FromDhall, ToDhall)

data Board = Board Row Row Row
    deriving (Generic, Show, FromDhall, ToDhall)

data GameState = Playing | Ended
    deriving (Generic, Eq, Show, FromDhall, ToDhall)

data Game = Game Board GameState
    deriving (Generic, Show, FromDhall, ToDhall)

playTicTacToe :: AgenticRWS m => Kleisli m a Game
playTicTacToe = Kleisli $ \_input -> do
    iterateUntilM
        -- Repeat until game ends
        (\(Game _ state) -> state == Ended)
        -- Ask the LLM to play the next move
        (\game -> do
            liftIO $ print game
            runAgentic (prompt >>> inject game >>> extract @Game) "Play the next move!"
        )
        -- The starting game state
        (Game (Board (Row Blank Blank Blank) (Row Blank Blank Blank) (Row Blank Blank Blank)) Playing)


