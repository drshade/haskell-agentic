-- | Public API for haskell-agentic.
-- Import this module for everything you need to build agents.
module Agentic
  ( -- * Agent type
    Agent
  , fanoutMap
  , sequential
    -- * Core primitives
  , prompt
  , extract
  , extractDhall
  , extractJson
  , extractWith
  , inject
  , injectJson
    -- * Schema format
  , SchemaFormat(..)
    -- * Effects
  , LLM
  , AgentEvents
  , AgentConfig
  , AgentSession
    -- * LLM request/response types
  , LLMRequest(..)
  , LLMResponse(..)
  , LLMMessage(..)
    -- * Effect smart constructors
  , call
  , emit
    -- * Event types
  , Event(..)
    -- * Effect interpreters
  , runEventsNoop
  , runEventsStdout
  , runAgentConfig
  , runAgentSession
    -- * Errors
  , SchemaError(..)
  , LLMError(..)
  , ToolError(..)
    -- * Tools
  , Tool(..)
  , ToolName(..)
  , mkTool
  , dispatchTool
    -- * Re-exports from effectful
  , Eff
  , runEff
  , (:>)
  , IOE
  , liftIO
    -- * Concurrent effect
  , Concurrent
  , runConcurrent
  ) where

import Agentic.Core (Agent, fanoutMap, sequential)
import Agentic.Effects
  ( LLM, AgentEvents, AgentConfig, AgentSession
  , LLMRequest(..), LLMResponse(..), LLMMessage(..)
  , call, emit
  , Event(..)
  , runEventsNoop, runEventsStdout, runAgentConfig, runAgentSession
  )
import Agentic.Error (SchemaError(..), LLMError(..), ToolError(..))
import Agentic.Prompt (prompt, extract, extractDhall, extractJson, extractWith, inject, injectJson)
import Agentic.Schema (SchemaFormat(..))
import Agentic.Tools (Tool(..), ToolName(..), mkTool, dispatchTool)
import Effectful (Eff, runEff, (:>), IOE, liftIO)
import Effectful.Concurrent (Concurrent, runConcurrent)
