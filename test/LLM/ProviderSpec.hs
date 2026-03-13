module LLM.ProviderSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import LLM.Provider (LLMProvider (..), LLMConfig (..), defaultProvider, defaultConfig, providerModel)

tests :: TestTree
tests = testGroup "LLM.Provider"
    [ testCase "defaultProvider is Anthropic" $
        defaultProvider @?= Anthropic

    , testCase "providerModel Anthropic returns non-empty model name" $
        (length (providerModel Anthropic) > 0) @?= True

    , testCase "defaultConfig uses Anthropic provider" $
        defaultConfig.provider @?= Anthropic

    , testCase "defaultConfig maxTokens is 10240" $
        defaultConfig.maxTokens @?= 10240

    , testCase "defaultConfig apiKey starts empty" $
        defaultConfig.apiKey @?= ""
    ]
