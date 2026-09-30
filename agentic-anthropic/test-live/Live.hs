module Main (main) where

import Agentic
import Agentic.Anthropic (anthropic)
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
  = DadJoke {setup' :: Text, punchline' :: Text}
  | OneLiner {line :: Text}
  | KnockKnock {whosThere :: Text, punchline' :: Text}
  deriving (Generic, Show, Contract)

newtype Rating = Rating Int
  deriving (Show, Eq)

instance Contract Rating where
  contract = mapCodec Rating (\(Rating n) -> n) (between 1 10 contract)

main :: IO ()
main = do
  _ <- loadDotEnv
  key <- lookupEnv "ANTHROPIC_API_KEY"
  hspec $ Test.Hspec.describe "Anthropic, live" $ case key of
    Nothing -> it "needs ANTHROPIC_API_KEY" (pendingWith "set ANTHROPIC_API_KEY in .env to run the live tests")
    Just _ -> do
      let fast = anthropic & effort Low
          withClaude = pure runtime >>= withSystemTwo fast

      it "drafts a record" $ do
        rt <- withClaude
        j <- interpret rt (draft @Joke "a joke please") ()
        T.null j.punchline `shouldBe` False

      it "drafts a list, which has to be wrapped for structured outputs" $ do
        rt <- withClaude
        names <- interpret rt (draft @[Text] "Name exactly 3 dinosaurs") ()
        length names `shouldBe` 3

      it "drafts a sum type from typed input" $ do
        rt <- withClaude
        j <- interpret rt (draft @BetterJoke "Convert this knock-knock joke") (Joke "knock-knock" "Knock knock. Who's there? Boo." "Don't cry, it's only a joke!")
        case j of
          KnockKnock {} -> pure ()
          other -> expectationFailure ("expected a KnockKnock, got " <> show other)

      it "runs a tool loop" $ do
        calls <- newIORef (0 :: Int)
        let lookupFossil = tool @Text @Text "fossil_count" "How many fossils the museum holds of a dinosaur" $
              act (\_ -> modifyIORef calls (+ 1) >> pure "The museum holds 17 fossils of it.")
        rt <- withClaude
        n <- interpret rt (draftWith @Int [lookupFossil] "How many Stegosaurus fossils does the museum hold? Use the tool.") ()
        n `shouldBe` 17
        readIORef calls >>= (`shouldSatisfy` (>= 1))

      it "keeps within a contract's checks" $ do
        rt <- withClaude
        Rating n <- interpret rt (draft @Rating "Rate this joke") (Joke "pun" "Why was the scarecrow promoted?" "He was outstanding in his field.")
        n `shouldSatisfy` (\x -> x >= 1 && x <= 10)

      it "stands in as System One" $ do
        rt <- pure runtime >>= withSystemOne fast
        kept <- interpret rt (keep 0.5 (yesNo "Is this text a joke?")) ["Why was the scarecrow promoted? He was outstanding in his field.", "The meeting is at 3pm in room 4." :: Text]
        length kept `shouldBe` 1
