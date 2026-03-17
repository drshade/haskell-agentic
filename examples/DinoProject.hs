{-# LANGUAGE ConstraintKinds #-}

module DinoProject where

import           Agentic
import           Agentic.Provider.Anthropic (defaultAnthropicConfig,
                                             runAnthropic)
import           Control.Monad              ((>=>))
import           Data.Text                  (Text, unpack)
import           Dhall                      (FromDhall, ToDhall)
import           Effectful.Error.Static     (Error, runError)
import           GHC.Generics               (Generic)

-- Types

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

-- Wrapper record for passing all three pieces to buildPoster
data DinoEntry = DinoEntry
  { dino      :: Dino
  , pic       :: DinoPic
  , trumpCard :: DinoTrumpCard
  }
  deriving (Generic, Show, FromDhall, ToDhall)

-- Agents (plain functions, no Kleisli arrows)

type DinoEffects es =
  ( LLM :> es
  , IOE :> es
  , Concurrent :> es
  , AgentEvents :> es
  , Error SchemaError :> es
  )

suggestDinos :: DinoEffects es => Agent es () [Text]
suggestDinos _ = extract @[Text]
  "Suggest 3 dinosaur names for a grade 5 project"

researchDino :: DinoEffects es => Agent es Text Dino
researchDino name = extract @Dino
  (inject name "Research this dinosaur")

drawPic :: DinoEffects es => Agent es Text DinoPic
drawPic name = extract @DinoPic
  (inject name "Draw an ascii picture of this dinosaur, 10 lines high")

buildTrumpCard :: DinoEffects es => Agent es Text DinoTrumpCard
buildTrumpCard name = extract @DinoTrumpCard
  (inject name "Output a trump card for this dinosaur")

-- Each dino: research + pic + trump card sequentially
researchOneDino :: DinoEffects es => Agent es Text DinoEntry
researchOneDino name = do
  d <- researchDino name
  p <- drawPic name
  c <- buildTrumpCard name
  pure DinoEntry { dino = d, pic = p, trumpCard = c }

buildPoster :: DinoEffects es => Agent es [DinoEntry] Text
buildPoster entries = extract @Text
  (inject entries "Create a poster for these dinosaurs with their pictures and trump cards")

-- The full pipeline
dinoProject :: DinoEffects es => Agent es () Text
dinoProject = suggestDinos >=> fanoutMap researchOneDino >=> buildPoster

-- Runner

runDinoProject :: IO ()
runDinoProject = do
  result <- runEff
    . runConcurrent
    . runAnthropic defaultAnthropicConfig
    . runEventsNoop
    . runError @LLMError
    . runError @SchemaError
    $ dinoProject ()
  case result of
    Left (_, llmErr)            -> putStrLn $ "LLM error: "    ++ show llmErr
    Right (Left (_, schemaErr)) -> putStrLn $ "Schema error: " ++ show schemaErr
    Right (Right posterText)    -> putStrLn (unpack posterText)
