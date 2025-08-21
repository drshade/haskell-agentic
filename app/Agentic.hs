module Agentic where

import           Control.Arrow           (Arrow, Kleisli (..), arr, (>>>))
import           Control.Exception       (SomeException, try)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.RWS       (RWST, runRWST, tell)
import           Control.Monad.RWS.Class (MonadRWS)
import           Data.Either.Validation  (Validation (Failure, Success))
import           Data.Text               hiding (show)
import           Dhall                   (FromDhall, ToDhall)
import qualified Dhall
import           Dhall.Core              (Expr (Text))
import qualified Dhall.Core
import qualified LLM                     (chat)

type AgenticRWS m = (MonadIO m, MonadRWS Environment Events State m)
type Agentic m a b = AgenticRWS m => Kleisli m a b

newtype Environment = Environment { prompt :: Text }
type Events = [(Text, Text)]
newtype State = State ()

orFail :: Arrow a => a (Either Text c) c
orFail = arr $ either (error . unpack) id

-- Glue two arrows together, injecting the schema of the type required as input to second arrow
-- to the prompt given by the first (including parsing or failing of that type)
(>...>) :: forall a b c m. (FromDhall b, ToDhall b) => Agentic m a Text -> Agentic m b c -> Agentic m a c
(>...>) l r = l >>> extract @b >>> r

runLLM :: Agentic m Text Text
runLLM = Kleisli $ \prompt' -> do
    -- liftIO $ putStrLn $ "------\nTo LLM [\n" <> unpack prompt' <> "]\n------\n"
    reply <- liftIO $ LLM.chat prompt'
    -- liftIO $ putStrLn $ "------\nFrom LLM [\n" <> unpack reply <> "]\n------\n"
    tell [(prompt', reply)]
    pure reply

roundtripAsWithRetry :: forall s m. (FromDhall s, ToDhall s) => Agentic m Text (Either Text s)
roundtripAsWithRetry = Kleisli $ \input -> do
    let attempt input' = do
            reply <- runAgentic (injectDhallSchema @s >>> runLLM) input'
            parsed <- runAgentic (dhallParse @s) reply
            pure (reply, parsed)
    (reply, parsed) <- attempt input
    case parsed of
        Left err -> do
            let instruction =
                    "Your last reply failed Dhall parsing with the following error:\n" <> err
                    <> "\n\n" <> "Your response was:\n" <> reply
                    <> "\n\n" <> "The instruction you were given was:\n" <> input
                    <> "\n\n" <> "Please fix the problem and respond with the correct output"
            (_reply', parsed') <- attempt instruction
            pure parsed'
        Right result -> pure $ Right result

extract :: forall s m. (FromDhall s, ToDhall s) => Agentic m Text s
extract = injectDhallSchema @s >>> runLLM >>> dhallParse @s >>> orFail

-- Generate Dhall schema from type
dhallSchema :: forall a. FromDhall a => Text
dhallSchema = case Dhall.expected (Dhall.auto @a) of
    Success result -> pack $ show $ Dhall.Core.pretty result
    Failure _      -> "-- Schema generation failed"

-- Parse Dhall text into Haskell value
dhallParse :: forall b m. (FromDhall b) => Agentic m Text (Either Text b)
dhallParse = Kleisli $ \input -> do
    result <- liftIO $ try $ Dhall.input Dhall.auto input
    case result of
        Right value -> pure $ Right value
        Left (err :: SomeException) -> pure $ Left $ "Dhall parse error: " <> pack (show err) <> "\nInput was: " <> input

-- Inject Dhall schema and instructions into prompt
injectDhallSchema :: forall s m. (FromDhall s, ToDhall s) => Agentic m Text Text
injectDhallSchema = Kleisli $ \prompt' -> pure $ prompt' <> "\n\n" <> instructions <> "\n" <> dhallSchema @s <> "\n\n" <> examples <> "\n\n" <> syntax <> "\n\n" <> extras
    where
        instructions :: Text
        instructions = "\
            \Return response in Dhall format using the following schema:\n\
            \"

        extras :: Text
        extras = "\
        \Additionally - try to be as uncreative as possible when abiding by the schema. \
        \e.g. if there is only field and the prompt has asked for many, don't try to \
        \squash all the results into this single field. Just insert one. \
        \Do not break the contract of the schema as it will not be interpretted by a \
        \human or other agent, but rather a structured parser. \
        \You may introduce your own variables and functions using let syntax (see examples) for \
        \dealing with repeated values and/or to save space.\
        \If asked to do repetitive work, always introduce a function to construct the output.\n\
        \"

        syntax :: Text
        syntax = "\
        \Extra syntax rules:\n\
        \Escape double-quotes with backslash (but not single quotes) in Text\n\
        \Natural numbers need no prefixes but Integers always need the sign prefixed (e.g. +10 or -100)\n\
        \"

        examples :: Text
        examples = "\
        \Examples:\n\
        \Example schema: < Dog : { name : Text, age : Natural, breed : Text } | Cat : { name : Text } > \n\
        \Valid response: \n\
        \   let Schema = < Dog : { name : Text, age : Natural, breed : Text } \n\
        \                  Cat : { name : Text } >\n\
        \   in Schema.Dog { name = \"Rex\", age = 7, breed = \"schauzer\" }\n\
        \Invalid response: \n\
        \   Dog { name = \"Rex\", age = 7, breed = \"schauzer\" }\n\
        \\n\
        \Example schema: { name : Text, age : Integer, maritalStatus : < Unmarried | Married | Widowed > }\n\
        \Valid response: \n\
        \   let Schema = { name : Text, age : Integer, maritalStatus : < Unmarried | Married | Widowed > }\n\
        \   in { name = \"Jane Doe\", age = +29, maritalStatus = < Unmarried | Married | Widowed >.Unmarried } : Schema\n\
        \Valid response: \n\
        \   let MaritalStatus = < Unmarried | Married | Widowed > \n\
        \   let Schema = { name : Text, age : Integer, maritalStatus : MaritalStatus }\n\
        \   in { name = \"Jane Doe\", age = +29, maritalStatus = MaritalStatus.Unmarried } : Schema\n\
        \\n\
        \Example schema: List { name : Text, description : Optional Text }\n\
        \Valid response: \n\
        \   let Schema = { name : Text, description : Optional Text }\n\
        \   in [ { name = \"Pizza\", description = Some \"Tasty!\" }, { name = \"Bangers 'n mash\", description = None Text } ] : List Schema\n\
        \Valid response: \n\
        \   let Schema = { name : Text, description : Optional Text }\n\
        \   let mkFood = \\(n : Text) ->\n\
        \          let description = None Text\n\
        \          in { name = n, description }\n\
        \   in [ mkFood \"pizza\", mkFood \"burgers\" ] : List Schema\n\
        \\n\
        \Example schema: { _1 : { name : Text }, _2 : < Unmarried | Married > }\n\
        \Valid response: \n\
        \   let MaritalStatus = < Unmarried | Married > \n\
        \   let Schema = { _1 : { name : Text }, _2 : MaritalStatus }\n\
        \   in { _1 = { name = \"Robert\"}, _2 = MaritalStatus.Unmarried } : Schema\n\
        \Invalid response: \n\
        \   let Schema = { _1 : { name : Text }, _2 : MaritalStatus }\n\
        \   in { _1 = { name = \"Robert\"}, _2 = Schema._2.Unmarried } : Schema\n\
        \"

inject :: forall s m. (ToDhall s) => s -> Agentic m Text Text
inject obj = Kleisli $ \prompt -> do
    let dhall = Dhall.Core.pretty $ Dhall.embed Dhall.inject obj
    pure $ prompt <> "\n\nInput:\n" <> dhall

prompt :: Arrow a => a Text Text
prompt = arr id

runAgentic :: Kleisli m a b -> a -> m b
runAgentic = runKleisli

run :: Kleisli (RWST Environment Events State IO) Text a -> Text -> IO a
run k input = do
    -- Initial conditions for RWS
    let environment = Environment { prompt = input }
        state = State ()

    (a, _finalState, logs) <- runRWST (runKleisli k input) environment state

    mapM_   (\(llmInput, llmOutput) -> do
                putStrLn $ "LLM Input:\n[" <> unpack llmInput <> "]"
                putStrLn $ "LLM Output:\n[" <> unpack llmOutput <> "]"
            ) logs

    pure a
