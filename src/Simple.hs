-- | Simplified entry point for the @haskell-agentic@ library.
--
-- This module provides the recommended API for building and running agents.
-- It defaults to Dhall schema for type-safe LLM I\/O.
--
-- === Quick Start
--
-- @
-- data Joke = Joke { setup :: Text, punchline :: Text }
--     deriving (Generic, Show, FromDhall, ToDhall)
--
-- result <- run (prompt >>> extract \@Joke) "tell me a joke"
-- @
module Simple (prompt, extractWith, injectWith, extract, inject, Dhall, Json, run, Text, (>>>)) where

import           Agentic                      hiding (run)
import           Control.Arrow                (Kleisli, (>>>))
import           Control.Monad.RWS            (RWST)
import           Data.Text
import           Dhall                        (FromDhall, ToDhall)
import           Progress
import           Protocol.Class
import           Protocol.DhallSchema.Marshal
import           Protocol.JSONSchema.Marshal

-- | Extract a typed value from the LLM using Dhall schema.
-- The LLM is instructed to produce Dhall-encoded output matching the type @s@.
extract :: forall s m. (FromDhall s, ToDhall s) => Agentic m Prompt s
extract = extractWith @Dhall @s

-- | Inject a typed value into the prompt as Dhall.
-- The serialised value is appended to the user's prompt.
inject :: forall s m. (FromDhall s, ToDhall s) => s -> Agentic m Prompt Prompt
inject = injectWith @Dhall @s

-- | Run an agent pipeline, displaying a progress spinner.
-- Reads @ANTHROPIC_KEY@ from the environment.
run :: Kleisli (RWST Environment Events State IO) Text b -> Text -> IO b
run = runWithProgress

