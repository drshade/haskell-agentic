module Main (main) where

import Agentic
import Agentic.OpenAI (openai)
import Agentic.IO.DotEnv (loadDotEnv)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import Test.Hspec hiding (describe)
import qualified Test.Hspec

data Joke = Joke {genre :: Text, setup :: Text, punchline :: Text}
  deriving (Generic, Show, Contract)

data BetterJoke
  = DadJoke {setup :: Text, punchline :: Text}
  | OneLiner {line :: Text}
  | KnockKnock {whosThere :: Text, punchline :: Text}
  deriving (Generic, Show, Contract)

data Groan = Mild | Solid | Unbearable
  deriving (Generic, Show, Eq)

instance Options Groan where
  options =
    documentedOptions
      "How much the audience groans"
      [option Mild "A polite smile", option Solid "An audible groan", option Unbearable "People get up and leave"]

deriving via Enumeration Groan instance Contract Groan

newtype Rating = Rating Int
  deriving (Show, Eq)

instance Contract Rating where
  contract = mapCodec Rating (\(Rating n) -> n) (between 1 10 contract)

main :: IO ()
main = do
  _ <- loadDotEnv
  apiKey <- lookupEnv "OPENAI_API_KEY"
  hspec $ Test.Hspec.describe "OpenAI, live" $ case apiKey of
    Nothing -> it "needs OPENAI_API_KEY" (pendingWith "set OPENAI_API_KEY in .env to run the live tests")
    Just _ -> do
      let fast = openai & effort Low
          withModel = pure runtime >>= withSystemTwo fast

      it "drafts a record" $ do
        rt <- withModel
        j <- interpret rt (draft @Joke "a joke please") ()
        T.null j.punchline `shouldBe` False

      it "drafts a list, which has to be wrapped for structured outputs" $ do
        rt <- withModel
        names <- interpret rt (draft @[Text] "Name exactly 3 dinosaurs") ()
        length names `shouldBe` 3

      it "drafts a sum type from typed input" $ do
        rt <- withModel
        j <- interpret rt (draft @BetterJoke "Convert this knock-knock joke") (Joke "knock-knock" "Knock knock. Who's there? Boo." "Don't cry, it's only a joke!")
        case j of
          KnockKnock {} -> pure ()
          other -> expectationFailure ("expected a KnockKnock, got " <> show other)

      it "runs a tool loop" $ do
        calls <- newIORef (0 :: Int)
        let lookupFossil = tool @Text @Text "fossil_count" "How many fossils the museum holds of a dinosaur" $
              act (\_ -> modifyIORef calls (+ 1) >> pure "The museum holds 17 fossils of it.")
        rt <- withModel
        n <- interpret rt (draftWith @Int [lookupFossil] "How many Stegosaurus fossils does the museum hold? Use the tool.") ()
        n `shouldBe` 17
        readIORef calls >>= (`shouldSatisfy` (>= 1))

      it "keeps within a contract's checks" $ do
        rt <- withModel
        Rating n <- interpret rt (draft @Rating "Rate this joke") (Joke "pun" "Why was the scarecrow promoted?" "He was outstanding in his field.")
        n `shouldSatisfy` (\x -> x >= 1 && x <= 10)

      it "drafts an Options type, whose schema is described constants" $ do
        rt <- withModel
        g <- interpret rt (draft @Groan "How much will the audience groan at this pun?") ("I'm reading a book about anti-gravity. It's impossible to put down." :: Text)
        g `shouldSatisfy` (`elem` [Mild, Solid, Unbearable])

      it "stands in as System One" $ do
        rt <- pure runtime >>= withSystemOne fast
        kept <- interpret rt (keep 0.5 (yesNo "Is this text a joke?")) ["Why was the scarecrow promoted? He was outstanding in his field.", "The meeting is at 3pm in room 4." :: Text]
        length kept `shouldBe` 1
