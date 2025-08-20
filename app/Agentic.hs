module Agentic where

import           Control.Arrow          (Arrow, Kleisli (..), arr, (>>>))
import           Control.Exception      (SomeException, try)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Either.Validation (Validation (Failure, Success))
import           Data.Text              hiding (show)
import           Dhall                  (FromDhall, ToDhall)
import qualified Dhall
import qualified Dhall.Core
import qualified LLM                    (chat)

orFail :: Arrow a => a (Either Text c) c
orFail = arr $ either (error . unpack) id

-- Glue two arrows together, injecting the schema of the type required as input to second arrow
-- to the prompt given by the first (including parsing or failing of that type)
(>...>) :: forall a b c m. (MonadIO m, FromDhall b, ToDhall b) => Kleisli m a Text -> Kleisli m b c -> Kleisli m a c
(>...>) l r = l >>> roundtripAsDhall @b >>> r

runLLM :: MonadIO m => Kleisli m Text Text
runLLM = Kleisli $ \prompt -> do
    r <- liftIO $ LLM.chat prompt
    -- liftIO $ putStrLn $ unpack r
    pure r

roundtripAsDhall :: forall s m. (MonadIO m, FromDhall s, ToDhall s) => Kleisli m Text s
roundtripAsDhall = injectDhallSchema @s >>> runLLM >>> dhallParse @s >>> orFail

-- Generate Dhall schema from type
dhallSchema :: forall a. FromDhall a => Text
dhallSchema = case Dhall.expected (Dhall.auto @a) of
    Success result -> pack $ show $ Dhall.Core.pretty result
    Failure _      -> "-- Schema generation failed"

-- Parse Dhall text into Haskell value
dhallParse :: forall b m. (MonadIO m, FromDhall b) => Kleisli m Text (Either Text b)
dhallParse = Kleisli $ \input -> do
    result <- liftIO $ try $ Dhall.input Dhall.auto input
    case result of
        Right value -> pure $ Right value
        Left (err :: SomeException) -> pure $ Left $ "Dhall parse error: " <> pack (show err) <> "\nInput was: " <> input

-- Inject Dhall schema and instructions into prompt
injectDhallSchema :: forall s a. (FromDhall s, ToDhall s, Arrow a) => a Text Text
injectDhallSchema = arr $ \prompt -> prompt <> "\n\n" <> instructions <> "\n" <> dhallSchema @s <> "\n\n" <> examples <> "\n\n" <> extras
    where
        instructions :: Text
        instructions = "\
            \Return response in Dhall format using the following schema:\n\
            \"

        extras :: Text
        extras = "\
        \Additionally - try to be as uncreative as possible when abiding by the schema.\
        \ e.g. if there is only field and the prompt has asked for many, don't try to \
        \squash all the results into this single field. Just insert one. \
        \Do not break the contract of the schema as it will not be interpretted by a \
        \human or other agent, but rather a structured parser.\
        \You may introduce your own variables using let syntax (see examples) for \
        \dealing with repeated values to save space.\
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
        \   let MartitalStatus = < Unmarried | Married | Widowed > \n\
        \   let Schema = { name : Text, age : Integer, maritalStatus : MaritalStatus }\n\
        \   in { name = \"Jane Doe\", age = +29, maritalStatus = MartitalStatus.Unmarried } : Schema\n\
        \\n\
        \Example schema: List { name : Text, description : Optional Text }\n\
        \Valid response: \n\
        \   let Schema = { name : Text, description : Optional Text }\n\
        \   in [ { name = \"Pizza\", description = Some \"Tasty!\" }, { name = \"Burgers\", description = None Text } ] : List Schema\n\
        \Valid response: \n\
        \   let Schema = { name : Text, description : Optional Text }\n\
        \   let mkFood = \\(n : Text) ->\n\
        \          let description = None Text\n\
        \          in { name = n, description }\n\
        \   in [ mkFood \"pizza\", mkFood \"burgers\" ] : List Schema\n\
        \"

prompt :: Arrow a => a Text Text
prompt = arr id

