module Main where

import Test.Tasty (defaultMain, testGroup)
import qualified Agentic.RetrySpec
import qualified LLM.ProviderSpec
import qualified Protocol.DhallSchema.MarshalSpec
import qualified Protocol.JSONSchema.MarshalSpec

main :: IO ()
main = defaultMain $ testGroup "haskell-agentic"
    [ Protocol.DhallSchema.MarshalSpec.tests
    , Protocol.JSONSchema.MarshalSpec.tests
    , Agentic.RetrySpec.tests
    , LLM.ProviderSpec.tests
    ]
