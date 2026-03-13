module Agentic.RetrySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Agentic.Retry (RetryConfig(..), defaultRetry, withRetry)
import Agentic.Error (AgenticError(..))
import Data.IORef

tests :: TestTree
tests = testGroup "Agentic.Retry"
    [ testCase "withRetry calls action once on immediate success" $ do
        callCount <- newIORef (0 :: Int)
        let action = do
                modifyIORef' callCount (+1)
                pure (Right "ok" :: Either AgenticError String)
        result <- withRetry defaultRetry action
        result @?= Right "ok"
        count <- readIORef callCount
        count @?= 1

    , testCase "withRetry retries and succeeds on third attempt" $ do
        callCount <- newIORef (0 :: Int)
        let action = do
                n <- readIORef callCount
                modifyIORef' callCount (+1)
                if n < 2
                    then pure (Left $ ParseError "fail" "" :: Either AgenticError String)
                    else pure (Right "ok")
        result <- withRetry (RetryConfig { maxAttempts = 3 }) action
        result @?= Right "ok"
        count <- readIORef callCount
        count @?= 3

    , testCase "withRetry returns Left after exhausting all attempts" $ do
        callCount <- newIORef (0 :: Int)
        let action = do
                modifyIORef' callCount (+1)
                pure (Left $ ParseError "always fails" "" :: Either AgenticError String)
        result <- withRetry (RetryConfig { maxAttempts = 3 }) action
        case result of
            Left (ParseError msg _) -> msg @?= "No attempts remaining"
            Left other              -> fail $ "Expected ParseError, got: " <> show other
            Right _                 -> fail "Expected failure"
        count <- readIORef callCount
        count @?= 3

    , testCase "defaultRetry has maxAttempts = 2" $ do
        defaultRetry @?= RetryConfig { maxAttempts = 2 }
    ]
