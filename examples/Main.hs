module Main where

import Agentic
import Agentic.Provider.Anthropic (runAnthropic, defaultAnthropicConfig)
import Data.Text (Text)
import Effectful.Error.Static (runError)
import GHC.Generics (Generic)
import Dhall (FromDhall, ToDhall)

data Joke = Joke
  { genre     :: Text
  , setup     :: Text
  , punchline :: Text
  }
  deriving (Generic, Show, FromDhall, ToDhall)

main :: IO ()
main = do
  result <- runEff
    . runAnthropic defaultAnthropicConfig
    . runEventsNoop
    . runError @SchemaError
    $ extract @Joke "Tell me a programming joke"
  case result of
    Left err  -> putStrLn $ "Schema error: " ++ show err
    Right (Joke g s p) -> do
      putStrLn $ "Genre: "     ++ show g
      putStrLn $ "Setup: "     ++ show s
      putStrLn $ "Punchline: " ++ show p
