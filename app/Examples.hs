
module Examples where


import           Agentic                      (Agentic, AgenticRWS,
                                               extractWithRetry, orFail,
                                               pattern Agentic, prompt, run)
import           Autodocodec
import           Combinators                  ((<<.>>))
import           Control.Arrow                ((&&&), (>>>))
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Loops          (iterateUntilM)
import           Data.Text
import           Dhall                        (FromDhall, ToDhall)
import           GHC.Generics                 (Generic)
import           Prelude                      hiding (show)
import           Protocol.Class               (extractWith, injectWith)
import           Protocol.DhallSchema.Marshal (Dhall)
import           Protocol.JSONSchema.Marshal  (Json)
import           UnliftIO                     (MonadUnliftIO)

data Joke = Joke
    { genre     :: Text
    , setup     :: Text
    , punchline :: Text
    }
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
    { asciiPic :: Text
    }
    deriving (Generic, Show, FromDhall, ToDhall)

data DinoTrumpCard = DinoTrumpCard
    { size         :: Int
    , ferocity     :: Int
    , speed        :: Int
    , intelligence :: Int
    , armor        :: Int
    }
    deriving (Generic, Show, FromDhall, ToDhall)

dinoProject :: forall a m. (AgenticRWS m, MonadUnliftIO m) => Agentic m a Text
dinoProject =
    let suggestDinos :: Agentic m a [Text]
        suggestDinos = Agentic $ const $ run (prompt >>> extractWith @Dhall @[Text])
                        "Suggest 3 dinosaur names for my grade 5 project"

        researchDino :: Agentic m Text Dino
        researchDino = Agentic $ \name -> run (prompt >>> injectWith @Dhall name >>> extractWith @Dhall @Dino)
                        "Research this dinosaur"

        drawPic :: Agentic m Text DinoPic
        drawPic = Agentic $ \name -> run (prompt >>> injectWith @Dhall name >>> extractWith @Dhall @DinoPic)
                        "Draw an ascii picture of this dinosaur, 10 lines high"

        buildTrumpCard :: Agentic m Text DinoTrumpCard
        buildTrumpCard = Agentic $ \name -> run (prompt >>> injectWith @Dhall name >>> extractWith @Dhall @DinoTrumpCard)
                        "Output a trump card for this dinosaur"

        buildPoster :: Agentic m [(Dino, (DinoPic, DinoTrumpCard))] Text
        buildPoster = Agentic $ \dinos -> run (prompt >>> injectWith @Dhall dinos >>> extractWith @Dhall @Text)
                        "Create the poster"

    in (suggestDinos <<.>> (researchDino &&& drawPic &&& buildTrumpCard)) >>> buildPoster

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
withTasks max' = Agentic $ \goal -> do
    let instruction = "Goal: \n" <> goal
            <> "\n\n" <> "Come up with a list of tasks to achieve the goal, set all tasks to Todo to start"
            <> "\n\n" <> "Do not use more than " <> show max' <> " tasks, but you are welcome to return less"

    tasks <- run (prompt >>> extractWith @Dhall @[Task]) instruction

    let printGoal (Goal goal' tasks') = do
                                putStrLn $ "Goal: " <> unpack goal'
                                mapM_ (\task -> putStrLn $ "  " <> unpack task.title <> " " <> unpack (show task.status)) tasks'

    completed :: Goal <- iterateUntilM
        (\(Goal _ tasks') -> Prelude.all
                                    (\task -> case task.status of
                                        Completed { result = _ } -> True
                                        Failed { reason = _ }    -> True
                                        _                        -> False
                                    ) tasks'
        )
        (\goal' -> do
            liftIO $ printGoal goal'
            run (prompt >>> injectWith @Dhall goal' >>> extractWith @Dhall @Goal) "Work on the next task in the list, and set to to Completed (including the result in the field itself)")
        (Goal goal tasks)

    liftIO $ putStrLn "Final goal:"
    liftIO $ printGoal completed

    final <- run (prompt >>> injectWith @Dhall completed >>> extractWith @Dhall @Text)
                "Using all these tasks, complete the goal as described - i.e. produce the final result. Do not include any status or progress report, simply output what is required by the goal stated"

    liftIO $ putStrLn $ unpack $ show final

    pure final

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
            run (prompt >>> injectWith @Dhall game >>> extractWith @Dhall @Game) "Play the next move!"
        )
        -- The starting game state
        (Game (Board (Row Blank Blank Blank) (Row Blank Blank Blank) (Row Blank Blank Blank)) Playing)


increment :: AgenticRWS m => Agentic m Text Int
increment = Agentic $ \input -> do
    run (prompt >>> injectWith @Dhall input >>> extractWith @Dhall @Int) "increment this"

data Tool
    = ListFiles
    | SearchWeb { q :: Text }
    | SetVolume { vol :: Int }
    deriving (Generic, Show, FromDhall, ToDhall)

data ToolUse s
    = NoTool s
    | UseTool Tool
    deriving (Generic, Show, FromDhall, ToDhall)

data AfterToolCall s
    = AfterToolCall { original :: s, toolResult :: ToolResult }
    deriving (Generic, Show, FromDhall, ToDhall)

data ToolResult
    = Files { results :: [Text] }
    | SearchResults { q :: Text, results :: [Text] }
    | Audio { vol :: Int }
    deriving (Generic, Show, FromDhall, ToDhall)

toolExample :: AgenticRWS m => Agentic m Text Text
toolExample = Agentic $ \input -> do
    response <- run (prompt >>> injectWith @Dhall input >>> extractWithRetry @(ToolUse Text) >>> orFail)
                    "Execute the query, but also consider the available tools available in the schema. If you respond with these, I will execute the tool for you and inject the results into the subsequent request."

    case response of
        NoTool s -> pure s
        UseTool tool -> do
            liftIO $ putStrLn $ "LLM requests to call tool => [" <> unpack (show tool) <> "]"

            let result = case tool of
                    ListFiles   -> Files { results = ["app", "CHANGELOG.md", "haskell-agentic.cabal", "README.md", "cabal.project", "dist-newstyle", "LICENSE"] }
                    SearchWeb q -> SearchResults { q, results = [""] }
                    SetVolume vol -> Audio vol

            run (prompt >>> injectWith @Dhall (AfterToolCall { original = input, toolResult = result }) >>> extractWith @Dhall @Text)
                "Results of your tool call are provided, now continue with the original query"

--------------------------
-- JSON Schema examples
--------------------------

instance HasCodec Joke where
    codec :: JSONCodec Joke
    codec = object "A simple joke structure"
          $ Joke
            <$> requiredFieldWith' "genre" validGenre .= (.genre)
            <*> requiredField "setup" "The setup of the joke" .= (.setup)
            <*> requiredField' "punchline" .= (.punchline)
        where validGenre = stringConstCodec [ ("dad", "dad")
                                            , ("pun", "pun")
                                            , ("oneliner", "oneliner")
                                            , ("knock-knock", "knock-knock")
                                            ]

jokeTellingAgent :: Agentic m a Text
jokeTellingAgent = Agentic $ \_ ->
    run (prompt >>> extractWith @Json @Text) "Tell a short joke"

shoutingAgent :: Agentic m Text Text
shoutingAgent = Agentic $ \input ->
    run (prompt >>> injectWith @Json input >>> extractWith @Json @Text) "please uppercase this"

repackagingAgent :: Agentic m Text Joke
repackagingAgent = Agentic $ \input ->
    run (prompt >>> injectWith @Json input >>> extractWith @Json @Joke) "repack into this structure"

loudJokeTeller :: Agentic m a Joke
loudJokeTeller = jokeTellingAgent >>> shoutingAgent >>> repackagingAgent

