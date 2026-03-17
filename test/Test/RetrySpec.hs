module Test.RetrySpec (spec) where

import Agentic
  ( extract, runEventsNoop, SchemaError(..)
  )
import Data.Text (Text)
import Effectful (runEff)
import Effectful.Error.Static (runError)
import Test.Hspec
import Test.MockLLM (runLLMMock, staticResponse)

spec :: Spec
spec = describe "extract retry" $ do

  it "succeeds on first attempt when response is valid Dhall Text" $ do
    -- A valid Dhall Text literal
    let validDhall = "\"hello world\""
    result <- runEff
      . runLLMMock (staticResponse validDhall)
      . runEventsNoop
      . runError @SchemaError
      $ extract @Text "say hello"
    case result of
      Left (_, err) -> expectationFailure $ "Expected success, got: " ++ show err
      Right txt     -> txt `shouldBe` "hello world"

  it "returns Left SchemaError after max retries on unparseable Dhall" $ do
    -- This is not valid Dhall
    let badDhall = "THIS IS NOT VALID DHALL !!!"
    result <- runEff
      . runLLMMock (staticResponse badDhall)
      . runEventsNoop
      . runError @SchemaError
      $ extract @Text "say hello"
    case result of
      Left (_, DhallParseError _) -> pure ()  -- expected
      Left (_, err)               -> expectationFailure $ "Expected DhallParseError, got: " ++ show err
      Right _                     -> expectationFailure "Expected failure, got success"
