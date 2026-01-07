{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Graph definition for the types-first development workflow.
--
-- Parallel execution graph:
-- @
-- Entry(StackSpec)
--     ↓
-- types (ClaudeCode 'Sonnet) - writes type signatures
--     ↓
-- fork (LogicNode) - creates worktrees, spawns parallel agents
--     ├── tests-worktree: tests agent → TestDefinitions
--     └── impl-worktree: impl agent → ImplementationCode
--     ↓
-- merge (LogicNode) - collects results, cleans up worktrees
--     ↓
-- Exit(ParallelResults)
-- @
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

import TypesFirstDev.Types (StackSpec, TypeDefinitions, ForkInput, ParallelResults)
import TypesFirstDev.Templates (TypesTpl)


-- | Types-first development workflow graph (parallel version).
--
-- Nodes:
-- 1. types: Claude Code writes type signatures
-- 2. fork: Creates worktrees and spawns parallel agents
-- 3. merge: Collects results and cleans up
data TypesFirstGraph mode = TypesFirstGraph
  { -- | Entry point - receives Stack specification
    entry :: mode :- G.Entry StackSpec

    -- | Types node - uses Claude Code to write type signatures
    --
    -- Output: TypeDefinitions (data type + signatures)
    -- Routes to fork node with session ID for parallel agents
  , types :: mode :- G.LLMNode
      :@ Types.Input StackSpec
      :@ Template TypesTpl
      :@ Schema TypeDefinitions
      :@ UsesEffects '[Goto "fork" ForkInput]
      :@ ClaudeCode 'Sonnet 'Nothing

    -- | Fork node - creates worktrees and spawns parallel agents
    --
    -- Input: ForkInput (session ID + type definitions)
    -- Output: ParallelResults (worktree paths + agent outputs)
  , fork :: mode :- G.LogicNode
      :@ Types.Input ForkInput
      :@ UsesEffects '[Goto "merge" ParallelResults]

    -- | Merge node - collects results and cleans up worktrees
    --
    -- Input: ParallelResults from fork
    -- Output: ParallelResults to exit (unchanged, just cleanup)
  , merge :: mode :- G.LogicNode
      :@ Types.Input ParallelResults
      :@ UsesEffects '[Goto Exit ParallelResults]

    -- | Exit point - returns parallel results
  , exit :: mode :- G.Exit ParallelResults
  }
  deriving Generic
