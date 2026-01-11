{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | V3 Graph Definition
--
-- 7-node TDD graph with tree coordination via Subgraph effect.
-- Based on types-first-dev/templates/v3/SPEC.md
--
-- NOTE: This module provides explicit HasField instances because GHC can't
-- derive HasField for records with type family-computed field types (mode :- NodeDef).
-- These are required for the DispatchGoto typeclass to look up handlers.
--
-- @
-- Entry(Spec)
--     ↓
-- ┌──────────┐
-- │ Scaffold │──[Subgraph]──▶ Child Graphs (spawnSelf per childSpec)
-- └──────────┘
--     ↓ TDDWriteTestsInput
-- ┌───────────────┐
-- │ TDDWriteTests │
-- └───────────────┘
--     ↓ TestsReadyPayload
-- ┌─────────────┐
-- │ ImplBarrier │◄──[Subgraph.awaitAny]── Child MergeComplete results
-- └─────────────┘
--     ↓ ImplInput
-- ┌────────┐
-- │  Impl  │◄───────────────────────┐
-- └────────┘                        │
--     │                             │
--     ├── TestsPassed ──▶ TDDReviewImpl
--     │                        │
--     │                        ├── Approved ──▶ Merger ──▶ Exit(MergeComplete)
--     │                        │
--     │                        └── MoreTests ──▶ TDDWriteTests
--     │
--     └── RequestRetry ─────────────┘ (max 5 attempts)
-- @
module TypesFirstDev.Graph
  ( TDDGraph(..)
  ) where

import GHC.Generics (Generic)

import Tidepool.Graph.Types
  ( type (:@)
  , Schema
  , Template
  , UsesEffects
  , Exit
  , Memory
  , ClaudeCode
  , ModelChoice(..)
  )
import qualified Tidepool.Graph.Types as Types (Input)
import Tidepool.Graph.Generic (GraphMode(..))
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Goto (Goto)

import TypesFirstDev.Types
import TypesFirstDev.Templates
  ( ScaffoldTpl
  , TDDWriteTestsTpl
  , TDDReviewImplTpl
  , ImplTpl
  , MergerTpl
  , RebaserTpl
  )

-- | TDD workflow graph.
--
-- 7 nodes organized into phases:
--
-- * Phase 1: Scaffold (decomposes spec, spawns children via Subgraph)
-- * Phase 2: TDDWriteTests (writes failing tests)
-- * Phase 3: ImplBarrier (collects child results via Subgraph.awaitAny)
-- * Phase 4: Impl (makes tests pass, self-loop retry)
-- * Phase 5: TDDReviewImpl (reviews impl, routes to Merger or MoreTests)
-- * Phase 6: Merger (files MR to parent)
-- * Phase 7: Rebaser (adapts to sibling changes)
--
-- Key features:
-- * Subgraph effect for child spawning (Scaffold) and collection (ImplBarrier)
-- * Memory effect for threaded conversation context
-- * Goto Self for Impl retry loop
-- * Linear flow: Scaffold → TDDWriteTests → ImplBarrier → Impl → ...
data TDDGraph mode = TDDGraph
  { --------------------------------------------------------------------------
    -- ENTRY
    --------------------------------------------------------------------------
    v3Entry :: mode :- G.Entry ScaffoldInput

    --------------------------------------------------------------------------
    -- PHASE 1: SCAFFOLD
    --------------------------------------------------------------------------

    -- | Scaffold: Analyze spec, create interface + contract suite + test plan.
    -- Spawns child graphs via Subgraph effect for decomposed specs.
    -- Routes to TDDWriteTests with payload.
  , v3Scaffold :: mode :- G.LLMNode
      :@ Types.Input ScaffoldInput
      :@ Template ScaffoldTpl
      :@ Schema ScaffoldExit
      :@ UsesEffects '[ Goto "v3TDDWriteTests" TDDWriteTestsInput
                      , Goto Exit ScaffoldExit  -- ClarificationNeeded exits graph
                      ]
      :@ ClaudeCode 'Sonnet

    --------------------------------------------------------------------------
    -- PHASE 2: TDD WRITE TESTS
    --------------------------------------------------------------------------

    -- | TDDWriteTests: Write failing tests for all criteria.
    -- Batch model - writes all tests upfront, commits once.
    -- Shares Memory with TDDReviewImpl for conversation context.
  , v3TDDWriteTests :: mode :- G.LLMNode
      :@ Types.Input TDDWriteTestsInput
      :@ Template TDDWriteTestsTpl
      :@ Schema TDDWriteTestsExit
      :@ Memory TDDMem
      :@ UsesEffects '[ Goto "v3ImplBarrier" TestsReadyPayload
                      , Goto "v3Scaffold" ScaffoldInput  -- InvalidScaffold
                      ]
      :@ ClaudeCode 'Sonnet

    --------------------------------------------------------------------------
    -- PHASE 3: IMPL BARRIER (collect children via Subgraph)
    --------------------------------------------------------------------------

    -- | ImplBarrier: Collect child MergeComplete results via Subgraph.awaitAny.
    -- Receives TestsReadyPayload from TDDWriteTests.
    -- Routes to Impl once all children collected.
    -- On fatal error (e.g., no scaffold context), exits with MergeFailed.
    --
    -- This is a LogicNode (not BarrierNode) because:
    -- - Child collection uses Subgraph effect (in V3Effects), not Fork/Barrier
    -- - Simpler handler type: TestsReadyPayload -> Eff es (GotoChoice '[...])
  , v3ImplBarrier :: mode :- G.LogicNode
      :@ Types.Input TestsReadyPayload
      :@ UsesEffects '[ Goto "v3Impl" ImplInput
                      , Goto Exit MergeComplete     -- Fatal errors exit with MergeFailed
                      ]

    --------------------------------------------------------------------------
    -- PHASE 4: IMPL (make tests pass)
    --------------------------------------------------------------------------

    -- | Impl: Make all failing tests pass.
    -- Self-loop retry via Goto Self (max 5 attempts).
    -- Shares Memory with ImplBarrier for conversation context.
  , v3Impl :: mode :- G.LLMNode
      :@ Types.Input ImplInput
      :@ Template ImplTpl
      :@ Schema ImplExit
      :@ Memory ImplMem
      :@ UsesEffects '[ Goto "v3TDDReviewImpl" TDDReviewImplInput  -- TestsPassed
                      , Goto "v3Impl" ImplInput                    -- RequestRetry
                      , Goto "v3Scaffold" ScaffoldInput            -- BlockedDependency, SpecAmbiguity
                      , Goto Exit ImplExit                         -- Stuck
                      ]
      :@ ClaudeCode 'Sonnet

    --------------------------------------------------------------------------
    -- PHASE 5: TDD REVIEW IMPL
    --------------------------------------------------------------------------

    -- | TDDReviewImpl: Review Impl's work.
    -- Approves → Merger, MoreTests → TDDWriteTests, Reject → Exit.
    -- Shares Memory with TDDWriteTests for conversation context.
  , v3TDDReviewImpl :: mode :- G.LLMNode
      :@ Types.Input TDDReviewImplInput
      :@ Template TDDReviewImplTpl
      :@ Schema TDDReviewImplExit
      :@ Memory TDDMem
      :@ UsesEffects '[ Goto "v3Merger" MergerInput    -- Approved
                      , Goto "v3TDDWriteTests" TDDWriteTestsInput  -- MoreTests
                      , Goto Exit TDDReviewImplExit   -- Reject
                      ]
      :@ ClaudeCode 'Sonnet

    --------------------------------------------------------------------------
    -- PHASE 6: MERGER
    --------------------------------------------------------------------------

    -- | Merger: File MR to parent after TDD approval.
    -- On success, broadcasts MergeComplete to parent and siblings.
  , v3Merger :: mode :- G.LLMNode
      :@ Types.Input MergerInput
      :@ Template MergerTpl
      :@ Schema MergerExit
      :@ UsesEffects '[ Goto Exit MergeComplete        -- MergeComplete (success)
                      , Goto "v3Impl" ImplInput        -- MergeRejected
                      ]
      :@ ClaudeCode 'Sonnet

    --------------------------------------------------------------------------
    -- PHASE 7: REBASER
    --------------------------------------------------------------------------

    -- | Rebaser: Adapt to sibling changes.
    -- Triggered by sibling MergeComplete broadcast (outside graph).
    -- Routes back to TDDWriteTests after adaptation.
    -- On fatal error (missing context), exits with RebaserConflict.
  , v3Rebaser :: mode :- G.LLMNode
      :@ Types.Input RebaserInput
      :@ Template RebaserTpl
      :@ Schema RebaserExit
      :@ UsesEffects '[ Goto "v3TDDWriteTests" TDDWriteTestsInput  -- Clean/Adapted
                      , Goto "v3Scaffold" ScaffoldInput            -- Conflict
                      , Goto Exit RebaserExit                      -- Fatal errors
                      ]
      :@ ClaudeCode 'Sonnet

    --------------------------------------------------------------------------
    -- EXIT
    --------------------------------------------------------------------------
  , v3Exit :: mode :- G.Exit MergeComplete
  }
  deriving Generic
