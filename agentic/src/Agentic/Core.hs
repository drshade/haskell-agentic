{-# LANGUAGE PatternSynonyms #-}

-- | Flows: typed, inspectable descriptions of agentic work.
module Agentic.Core
  ( -- * Flows
    Agentic (..)
  , Step (..)
  , Tool
  , ToolTuple
  , toolName
  , toolDescription
  , Note (..)
  , Instruction (..)
    -- * Steps
  , draft
  , draftWith
  , judge
  , act
    -- * Tools
  , tool
  , recursiveTool
  , recursiveTools
    -- * Structure
  , each
  , repeatUntil
  , note
  , named
    -- * Plumbing
  , takeFirst
  , takeSecond
  , (:/\)
  , pattern (:/\)
    -- * Judgement helpers
  , keep
  , gate
    -- * Re-exports
  , module Control.Arrow
  ) where

import Agentic.Internal.Core
import Control.Arrow
