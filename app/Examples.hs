
module Examples where


import           Agentic                (Agentic, AgenticRWS, extract,
                                         extractWithRetry, inject,
                                         pattern Agentic, prompt, run, runIO)
import           Combinators            ((<<.>>))
import           Control.Arrow          ((>>>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Loops    (iterateUntilM)
import           Data.Text
import           Dhall                  (FromDhall, ToDhall)
import           GHC.Generics           (Generic)
import           Prelude                hiding (show)
import           UnliftIO               (MonadUnliftIO)

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
    }
    deriving (Generic, Show, FromDhall, ToDhall)

data DinoPic = DinoPic
    { name     :: Text
    , asciiPic :: Text
    }
    deriving (Generic, Show, FromDhall, ToDhall)

dinoProject :: forall a m. (AgenticRWS m, MonadUnliftIO m) => Agentic m a Text
dinoProject =
    let suggestDinos :: Agentic m a [Text]
        suggestDinos = Agentic $ const $ run (prompt >>> extract @[Text]) "Suggest 3 dinosaur names for my grade 5 project"

        researchDino :: Agentic m Text Dino
        researchDino = Agentic $ \name -> run (prompt >>> inject name >>> extract @Dino) "Research this dinosaur"

        drawPic :: Agentic m Dino (Dino, DinoPic)
        drawPic = Agentic $ \dino -> do
            pic <- run (prompt >>> inject dino.name >>> extract @DinoPic) "Draw an ascii picture of this dinosaur, 10 lines high"
            pure (dino, pic)

        buildPoster :: Agentic m [(Dino, DinoPic)] Text
        buildPoster = Agentic $ \dinos -> run (prompt >>> inject dinos >>> extract @Text) "Create the poster"

    in (suggestDinos <<.>> (researchDino >>> drawPic)) >>> buildPoster

withTasks :: AgenticRWS m => Agentic m Text Text
withTasks = Agentic $ \input -> do
    let instruction = "Goal: \n" <> input
            <> "\n\n" <> "Return the list of tasks required to complete this, and I will call you back with each individual task"

    tasks <- run (prompt >>> extract @[Task]) instruction

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
                updatedTask <- run (prompt >>> extractWithRetry @Task) taskInstruction
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

playTicTacToe :: AgenticRWS m => Agentic m a Game
playTicTacToe = Agentic $ \_input -> do
    iterateUntilM
        -- Repeat until game ends
        (\(Game _ state) -> state == Ended)
        -- Ask the LLM to play the next move
        (\game -> do
            liftIO $ print game
            run (prompt >>> inject game >>> extract @Game) "Play the next move!"
        )
        -- The starting game state
        (Game (Board (Row Blank Blank Blank) (Row Blank Blank Blank) (Row Blank Blank Blank)) Playing)


increment :: AgenticRWS m => Agentic m Text Int
increment = Agentic$ \input -> do
    run (prompt >>> inject input >>> extract @Int) "increment this"
