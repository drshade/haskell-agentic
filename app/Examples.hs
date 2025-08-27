
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
        suggestDinos = Agentic $ const $ run (prompt >>> extract @[Text])
                        "Suggest 3 dinosaur names for my grade 5 project"

        researchDino :: Agentic m Text Dino
        researchDino = Agentic $ \name -> run (prompt >>> inject name >>> extract @Dino)
                        "Research this dinosaur"

        drawPic :: Agentic m Dino (Dino, DinoPic)
        drawPic = Agentic $ \dino -> do
            pic <- run (prompt >>> inject dino.name >>> extract @DinoPic)
                        "Draw an ascii picture of this dinosaur, 10 lines high"
            pure (dino, pic)

        buildPoster :: Agentic m [(Dino, DinoPic)] Text
        buildPoster = Agentic $ \dinos -> run (prompt >>> inject dinos >>> extract @Text)
                        "Create the poster"

    in (suggestDinos <<.>> (researchDino >>> drawPic)) >>> buildPoster

data TaskStatus = Todo | Failed { reason :: Text } | Completed { result :: Text }
    deriving (Generic, Show, FromDhall, ToDhall, Eq)

data Task = Task
    { title       :: Text
    , description :: Maybe Text
    , status      :: TaskStatus
    }
    deriving (Generic, Show, FromDhall, ToDhall)

data Goal = Goal { goal  :: Text
                 , tasks :: [Task]
                 }
    deriving (Generic, Show, FromDhall, ToDhall)

withTasks :: AgenticRWS m => Int -> Agentic m Text Text
withTasks max = Agentic $ \goal -> do
    let instruction = "Goal: \n" <> goal
            <> "\n\n" <> "Come up with a list of tasks to achieve the goal, set all tasks to Todo to start"
            <> "\n\n" <> "Do not use more than " <> show max <> " tasks, but you are welcome to return less"

    tasks <- run (prompt >>> extract @[Task]) instruction

    let printGoal (Goal goal tasks) = do
                                putStrLn $ "Goal: " <> unpack goal
                                mapM_ (\task -> putStrLn $ "  " <> unpack task.title <> " " <> unpack (show task.status)) tasks

    completed :: Goal <- iterateUntilM
        (\(Goal _ tasks') -> Prelude.all
                                    (\task -> case task.status of
                                        Completed { result } -> True
                                        Failed { reason }    -> True
                                        _                    -> False
                                    ) tasks'
        )
        (\goal' -> do
            liftIO $ printGoal goal'
            run (prompt >>> inject goal' >>> extract @Goal) "Work on the next task in the list, and set to to Completed (including the result in the field itself)")
        (Goal goal tasks)

    liftIO $ putStrLn "Final goal:"
    liftIO $ printGoal completed

    final <- run (prompt >>> inject completed >>> extract @Text) "Using all these tasks, complete the goal as described - i.e. produce the final result. Do not include any status or progress report, simply output what is required by the goal stated"

    liftIO $ putStrLn $ unpack $ show final

    pure final

    -- completedTasks <-
    --     mapM (\task -> do
    --             liftIO $ putStrLn $ "  Working on " <> unpack task.title <> "..."
    --             updatedTask <- run (prompt >>> inject task >>> extract @(Task, String)) "Work on this task, returning the result"
    --             liftIO $ putStrLn $ unpack $ show updatedTask
    --             pure updatedTask
    --           ) tasks

    -- reduced <- run (prompt >>> inject (goal, completedTasks) >>> extract @Text) "Complete the goal based on these completed tasks"

    -- liftIO $ do
    --     putStrLn $ "Reduced:"
    --     putStrLn $ unpack reduced

    -- pure reduced

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
increment = Agentic $ \input -> do
    run (prompt >>> inject input >>> extract @Int) "increment this"

-- extract :: forall s m. (FromDhall s, ToDhall s) => Agentic m Prompt s
-- extract = injectSchema @s >>> runLLM >>> parse @s >>> orFail

-- data Job s = Job
--     { instruction :: Text
--     , result      :: s
--     }
--     deriving (Generic, Show, FromDhall, ToDhall)

-- synthesize :: forall i m. (FromDhall i, ToDhall i) => Agentic m Text Text
-- synthesize = Agentic $ \input -> do
--     job <- run (prompt >>> extract @(Job i)) $ "Complete the goal using whatever schema you like for field `result`: " <> input
--     complete <- run (prompt >>> inject job >>> extract @Text) "Using the input, convert the output to text"
--     pure complete

-- test :: IO Text
-- test = runIO synthesize "what is the date"
