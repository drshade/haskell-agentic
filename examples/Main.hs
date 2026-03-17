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

-- Note: runAgentConfig and runAgentSession are not needed here because
-- 'extract' only requires LLM, IOE, AgentEvents, and Error SchemaError effects.
-- Those effects are used for stateful multi-turn conversations (future feature).
main :: IO ()
main = do
  result <- runEff
    . runAnthropic defaultAnthropicConfig
    . runEventsNoop
    . runError @LLMError
    . runError @SchemaError
    $ extract @Joke "Tell me a programming joke"
  case result of
    Left (_, llmErr) -> putStrLn $ "LLM error: " ++ show llmErr
    Right (Left (_, schemaErr)) -> putStrLn $ "Schema error: " ++ show schemaErr
    Right (Right joke) -> do
      let (Joke g s p) = joke
      putStrLn $ "Genre: "     ++ show g
      putStrLn $ "Setup: "     ++ show s
      putStrLn $ "Punchline: " ++ show p
