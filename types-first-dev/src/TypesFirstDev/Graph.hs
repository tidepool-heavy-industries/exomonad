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
-- skeleton (LogicNode) - generates impl/test skeleton files
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
    -- * TDD Graph (Sequential)
  , TypesFirstGraphTDD(..)
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

import TypesFirstDev.Types
  ( StackSpec, TypeDefinitions, ForkInput, SkeletonGenerated, ParallelResults
  -- TDD types
  , SkeletonState, TestsWritten, TestsVerified, ImplWritten
  , ValidationFailure, FixResult, TDDResult, TestsResult, ImplResult
  )
import TypesFirstDev.Templates (TypesTpl, TestsTpl, ImplTpl, FixTpl)


-- | Types-first development workflow graph (parallel version).
--
-- Nodes:
-- 1. types: Claude Code writes type signatures
-- 2. skeleton: Generate impl/test skeleton files
-- 3. fork: Creates worktrees and spawns parallel agents
-- 4. merge: Collects results and cleans up
data TypesFirstGraph mode = TypesFirstGraph
  { -- | Entry point - receives Stack specification
    entry :: mode :- G.Entry StackSpec

    -- | Types node - uses Claude Code to write type signatures
    --
    -- Output: TypeDefinitions (data type + signatures + test priorities)
    -- Routes to skeleton node for file generation
  , types :: mode :- G.LLMNode
      :@ Types.Input StackSpec
      :@ Template TypesTpl
      :@ Schema TypeDefinitions
      :@ UsesEffects '[Goto "skeleton" ForkInput]
      :@ ClaudeCode 'Sonnet 'Nothing

    -- | Skeleton node - generates impl/test skeleton files
    --
    -- Input: ForkInput (session ID + type definitions)
    -- Output: SkeletonGenerated (paths to generated files)
    -- Renders templates, writes files, verifies compilation
  , skeleton :: mode :- G.LogicNode
      :@ Types.Input ForkInput
      :@ UsesEffects '[Goto "fork" SkeletonGenerated]

    -- | Fork node - creates worktrees and spawns parallel agents
    --
    -- Input: SkeletonGenerated (paths + type definitions)
    -- Output: ParallelResults (worktree paths + agent outputs)
  , fork :: mode :- G.LogicNode
      :@ Types.Input SkeletonGenerated
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


-- ════════════════════════════════════════════════════════════════════════════
-- TDD GRAPH (Sequential - tests first, validation loop)
-- ════════════════════════════════════════════════════════════════════════════

-- | Sequential TDD graph - tests written before implementation.
--
-- This graph implements true TDD:
-- 1. Write type signatures (types node)
-- 2. Generate skeleton files (skeleton node)
-- 3. Write tests (tests node) - LLM writes QuickCheck properties
-- 4. Verify tests fail (verifyTestsFail node) - proves tests are meaningful
-- 5. Implement functions (impl node) - LLM implements to make tests pass
-- 6. Validate tests pass (validate node) - runs cabal test
-- 7. If tests fail, fix implementation (fix node) and loop back to validate
--
-- @
-- Entry(StackSpec)
--     ↓
-- types (ClaudeCode) → TypeDefinitions
--     ↓
-- skeleton (LogicNode) → SkeletonState
--     ↓
-- tests (ClaudeCode) → TestsWritten
--     ↓
-- verifyTestsFail (LogicNode) → TestsVerified
--     ↓
-- impl (ClaudeCode) → ImplWritten
--     ↓
-- ┌─────────────────────────────────┐
-- │     VALIDATION LOOP             │
-- │                                 │
-- │ validate (LogicNode)            │
-- │     ↓ [pass]    ↓ [fail]       │
-- │   Exit      fix (ClaudeCode)   │
-- │              ↓                 │
-- │            validate (loop)     │
-- └─────────────────────────────────┘
--     ↓
-- Exit(TDDResult)
-- @
data TypesFirstGraphTDD mode = TypesFirstGraphTDD
  { -- | Entry point - receives Stack specification
    tddEntry :: mode :- G.Entry StackSpec

    -- | Phase 1: Design types
    --
    -- Uses Claude Code to design the data type and function signatures.
    -- Output: TypeDefinitions with data type, signatures, and test priorities.
  , tddTypes :: mode :- G.LLMNode
      :@ Types.Input StackSpec
      :@ Template TypesTpl
      :@ Schema TypeDefinitions
      :@ UsesEffects '[Goto "tddSkeleton" TypeDefinitions]
      :@ ClaudeCode 'Haiku 'Nothing

    -- | Phase 2: Generate skeleton files
    --
    -- Generates impl skeleton with undefined stubs and test skeleton.
    -- Validates that the skeleton compiles.
  , tddSkeleton :: mode :- G.LogicNode
      :@ Types.Input TypeDefinitions
      :@ UsesEffects '[Goto "tddTests" SkeletonState]

    -- | Phase 3: Write tests (TDD step 1)
    --
    -- Uses Claude Code to write QuickCheck property tests.
    -- Tests should be written against the skeleton (will fail until impl).
  , tddTests :: mode :- G.LLMNode
      :@ Types.Input SkeletonState
      :@ Template TestsTpl
      :@ Schema TestsResult
      :@ UsesEffects '[Goto "tddVerifyTestsFail" TestsWritten]
      :@ ClaudeCode 'Haiku 'Nothing

    -- | Phase 4: Verify tests fail (TDD step 2)
    --
    -- Runs `cabal test` and verifies that tests FAIL.
    -- This proves the tests are meaningful (not trivially passing).
    -- If tests pass, the tests are likely wrong.
  , tddVerifyTestsFail :: mode :- G.LogicNode
      :@ Types.Input TestsWritten
      :@ UsesEffects '[Goto "tddImpl" TestsVerified]

    -- | Phase 5: Implement functions (TDD step 3)
    --
    -- Uses Claude Code to implement functions to make tests pass.
    -- Replaces undefined stubs with working implementations.
  , tddImpl :: mode :- G.LLMNode
      :@ Types.Input TestsVerified
      :@ Template ImplTpl
      :@ Schema ImplResult
      :@ UsesEffects '[Goto "tddValidate" ImplWritten]
      :@ ClaudeCode 'Haiku 'Nothing

    -- | Phase 6: Validate tests pass
    --
    -- Runs `cabal test` to verify implementation passes all tests.
    -- On success: exit with TDDResult.
    -- On failure: route to fix node for iteration.
  , tddValidate :: mode :- G.LogicNode
      :@ Types.Input ImplWritten
      :@ UsesEffects '[Goto "tddFix" ValidationFailure, Goto Exit TDDResult]

    -- | Phase 7: Fix loop
    --
    -- If tests fail, uses Claude Code to analyze failures and fix impl.
    -- Routes back to validate for re-testing.
    -- Maximum 5 attempts before hard failure.
  , tddFix :: mode :- G.LLMNode
      :@ Types.Input ValidationFailure
      :@ Template FixTpl
      :@ Schema FixResult
      :@ UsesEffects '[Goto "tddValidate" ImplWritten]
      :@ ClaudeCode 'Haiku 'Nothing

    -- | Exit point - returns TDD result
  , tddExit :: mode :- G.Exit TDDResult
  }
  deriving Generic
