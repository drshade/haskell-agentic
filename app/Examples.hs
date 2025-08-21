module Examples where


import           Agentic                (AgenticRWS, prompt, roundtripAs,
                                         roundtripAsWithRetry, runAgentic)
import           Control.Arrow          (Kleisli (Kleisli, runKleisli), (>>>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text
import           Dhall                  (FromDhall, ToDhall)
import           GHC.Generics           (Generic)
import           Prelude                hiding (show)

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

withTasks :: AgenticRWS m => Kleisli m Text Text
withTasks = Kleisli $ \input -> do
    let instruction = "Goal: \n" <> input <> "\n\n" <> "Return the list of tasks required to complete this, and I will call you back with each individual task"

    tasks <- runAgentic (prompt >>> roundtripAs @[Task]) instruction

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
                updatedTask <- runAgentic (prompt >>> roundtripAsWithRetry @Task) taskInstruction
                liftIO $ print $ show updatedTask
                pure updatedTask
              ) tasks

    liftIO $ do
        print "Completed Tasks:"
        print $ show completedTasks

    pure "hello"




