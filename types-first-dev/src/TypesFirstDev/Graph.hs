{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Graph definition for the types-first development workflow.
--
-- Sequential execution graph (no parallel agents):
-- @
-- Entry(StackSpec)
--     ↓
-- types (ClaudeCode 'Sonnet) - writes type signatures
--     ↓
-- Exit(TypeDefinitions)
-- @
--
-- This validates the basic ClaudeCode execution path.
-- Future iterations will add fork/merge/testLoop for parallel execution.
module TypesFirstDev.Graph
  ( TypesFirstGraph(..)
  ) where

import GHC.Generics (Generic)

import Tidepool.Graph.Types
  ( type (:@)
  , Schema
  , Template
  , UsesEffects
  , Exit
  , ClaudeCode
  , ModelChoice(..)
  )
import qualified Tidepool.Graph.Types as Types (Input)
import Tidepool.Graph.Generic (GraphMode(..))
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Goto (Goto)

import TypesFirstDev.Types (StackSpec, TypeDefinitions)
import TypesFirstDev.Templates (TypesTpl)


-- | Types-first development workflow graph (sequential version).
--
-- This simplified graph validates:
-- 1. ClaudeCode handler execution
-- 2. Template rendering
-- 3. Structured output parsing
data TypesFirstGraph mode = TypesFirstGraph
  { -- | Entry point - receives Stack specification
    entry :: mode :- G.Entry StackSpec

    -- | Types node - uses Claude Code to write type signatures
    --
    -- Output: TypeDefinitions (data type + signatures)
    -- Exits directly with the type definitions
  , types :: mode :- G.LLMNode
      :@ Types.Input StackSpec
      :@ Template TypesTpl
      :@ Schema TypeDefinitions
      :@ UsesEffects '[Goto Exit TypeDefinitions]
      :@ ClaudeCode 'Sonnet 'Nothing

    -- | Exit point - returns type definitions
  , exit :: mode :- G.Exit TypeDefinitions
  }
  deriving Generic
