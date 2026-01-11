{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | V3 Graph Definition
--
-- 8-node TDD graph with parallel spawn, tree coordination, and typed DSL mapping.
-- Based on types-first-dev/templates/v3/SPEC.md
--
-- @
-- Entry(Spec)
--     ↓
-- ┌──────────┐
-- │ Scaffold │──[Subgraph]──▶ Child Graphs (spawnSelf per childSpec)
-- └──────────┘
--     ↓ InitWork
-- ┌──────────┐
-- │ ForkNode │ ─────────────────────────────────────────────────────┐
-- └──────────┘                                                      │
--     │                                                             │
--     ├──[Spawn]──▶ TDDWriteTests ──TestsReady──┐                  │
--     │                                          │                  │
--     └──[Spawn]──▶ ImplBarrier ◄────────────────┤                  │
--                       │                        │                  │
--                       │ Awaits: TestsReady     │                  │
--                       │       + [MergeComplete]◄──────────────────┘
--                       ▼                        (children via Subgraph)
--                  ┌────────┐
--                  │  Impl  │
--                  └────────┘
--                       │
--     ┌─────────────────┴─────────────────┐
--     │                                   │
--     ▼ TestsPassed                       ▼ RequestRetry (self-loop)
-- ┌───────────────┐                  ┌────────┐
-- │ TDDReviewImpl │                  │  Impl  │ (max 5 attempts)
-- └───────────────┘                  └────────┘
--     │
--     ├── Approved ──▶ Merger ──MergeComplete──▶ Rebaser (siblings)
--     │                   │
--     │                   └──▶ Parent (broadcast)
--     │
--     └── MoreTests ──▶ TDDWriteTests (write additional tests)
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
    -- Fork/Barrier annotations
  , Spawn
  , Barrier
  , Awaits
  , Arrive
  )
import qualified Tidepool.Graph.Types as Types (Input)
import Tidepool.Graph.Generic (GraphMode(..))
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Goto (Goto, To)

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
-- 8 nodes organized into phases:
--
-- * Phase 1: Scaffold (decomposes spec, spawns children)
-- * Phase 2: Fork (spawns TDDWriteTests + ImplBarrier in parallel)
-- * Phase 3: TDDWriteTests (writes failing tests)
-- * Phase 4: ImplBarrier (waits for tests + children)
-- * Phase 5: Impl (makes tests pass, self-loop retry)
-- * Phase 6: TDDReviewImpl (reviews impl, routes to Merger or MoreTests)
-- * Phase 7: Merger (files MR to parent)
-- * Phase 8: Rebaser (adapts to sibling changes)
--
-- Key features:
-- * Subgraph effect in Scaffold for child spawning
-- * ForkNode/BarrierNode for parallel TDD + Impl
-- * Memory effect for threaded conversation context
-- * Goto Self for Impl retry loop
data TDDGraph mode = TDDGraph
  { --------------------------------------------------------------------------
    -- ENTRY
    --------------------------------------------------------------------------
    v3Entry :: mode :- G.Entry ScaffoldInput

    --------------------------------------------------------------------------
    -- PHASE 1: SCAFFOLD
    --------------------------------------------------------------------------

    -- | Scaffold: Analyze spec, create interface + contract suite + test plan.
    -- Optionally spawns child graphs via Subgraph effect.
    -- Routes to Fork with InitWorkPayload.
  , v3Scaffold :: mode :- G.LLMNode
      :@ Types.Input ScaffoldInput
      :@ Template ScaffoldTpl
      :@ Schema ScaffoldExit
      :@ UsesEffects '[ Goto "v3Fork" InitWorkPayload
                      , Goto Exit ScaffoldExit  -- ClarificationNeeded exits graph
                      ]

    --------------------------------------------------------------------------
    -- PHASE 2: FORK (parallel spawn)
    --------------------------------------------------------------------------

    -- | Fork: Spawn TDDWriteTests and ImplBarrier in parallel.
    -- Both receive InitWorkPayload.
  , v3Fork :: mode :- G.ForkNode
      :@ Types.Input InitWorkPayload
      :@ Spawn '[To "v3TDDWriteTests" TDDWriteTestsInput, To "v3ImplBarrier" InitWorkPayload]
      :@ Barrier "v3ImplBarrier"

    --------------------------------------------------------------------------
    -- PHASE 3: TDD WRITE TESTS
    --------------------------------------------------------------------------

    -- | TDDWriteTests: Write failing tests for all criteria.
    -- Batch model - writes all tests upfront, commits once.
    -- Shares Memory with TDDReviewImpl for conversation context.
  , v3TDDWriteTests :: mode :- G.LLMNode
      :@ Types.Input TDDWriteTestsInput
      :@ Template TDDWriteTestsTpl
      :@ Schema TDDWriteTestsExit
      :@ Memory TDDMem
      :@ UsesEffects '[ Arrive "v3ImplBarrier" TestsReadyPayload
                      , Goto "v3Scaffold" ScaffoldInput  -- InvalidScaffold
                      ]

    --------------------------------------------------------------------------
    -- PHASE 4: IMPL BARRIER (wait for tests + children)
    --------------------------------------------------------------------------

    -- | ImplBarrier: Wait for TDDWriteTests + all children's MergeComplete.
    -- Uses Subgraph.awaitAny to collect remaining children.
    -- Routes to Impl once all dependencies satisfied.
  , v3ImplBarrier :: mode :- G.BarrierNode
      :@ Awaits '[TestsReadyPayload]  -- TDD result; children via Subgraph effect
      :@ UsesEffects '[Goto "v3Impl" ImplInput]

    --------------------------------------------------------------------------
    -- PHASE 5: IMPL (make tests pass)
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

    --------------------------------------------------------------------------
    -- PHASE 6: TDD REVIEW IMPL
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

    --------------------------------------------------------------------------
    -- PHASE 7: MERGER
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

    --------------------------------------------------------------------------
    -- PHASE 8: REBASER
    --------------------------------------------------------------------------

    -- | Rebaser: Adapt to sibling changes.
    -- Triggered by sibling MergeComplete broadcast (outside graph).
    -- Routes back to TDDWriteTests after adaptation.
  , v3Rebaser :: mode :- G.LLMNode
      :@ Types.Input RebaserInput
      :@ Template RebaserTpl
      :@ Schema RebaserExit
      :@ UsesEffects '[ Goto "v3TDDWriteTests" TDDWriteTestsInput  -- Clean/Adapted
                      , Goto "v3Scaffold" ScaffoldInput            -- Conflict
                      ]

    --------------------------------------------------------------------------
    -- EXIT
    --------------------------------------------------------------------------
  , v3Exit :: mode :- G.Exit MergeComplete
  }
  deriving Generic
