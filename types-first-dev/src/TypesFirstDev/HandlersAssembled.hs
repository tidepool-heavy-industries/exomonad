{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Assembled handler record for V3 TDD graph.
--
-- This module wires all 8 node handlers into a complete TDDGraph record
-- for execution via the effect interpreter stack.
--
-- Architecture:
-- * Handlers declare effect constraints (Member Effect es)
-- * Handler record is instantiated with concrete V3Effects list
-- * GHC validates constraints at compile time
-- * Effect interpreter chain at runtime (Session → Memory → Subgraph → IO)
module TypesFirstDev.HandlersAssembled
  ( v3Handlers
  ) where

import Tidepool.Graph.Generic (AsHandler)
import Tidepool.Graph.Goto (ClaudeCodeLLMHandler(..), ClaudeCodeResult(..))
import Tidepool.Graph.Types (Exit, Self, ModelChoice(..))
import Tidepool.Effect.Session (SessionId)

import TypesFirstDev.Graph (TDDGraph(..))
import TypesFirstDev.V3.Interpreters (V3Effects)
import TypesFirstDev.Templates
  ( ScaffoldTpl, scaffoldCompiled
  , TDDWriteTestsTpl, tddWriteTestsCompiled
  , ImplTpl, implCompiled
  , TDDReviewImplTpl, tddReviewImplCompiled
  , MergerTpl, mergerCompiled
  , RebaserTpl, rebaserCompiled
  )

import TypesFirstDev.Handlers.Scaffold
  ( ScaffoldInput, ScaffoldExit, scaffoldBefore, scaffoldAfter )
import TypesFirstDev.Handlers.TDDWriteTests
  ( TDDWriteTestsInput, TDDWriteTestsExit, tddWriteTestsBefore, tddWriteTestsAfter )
import TypesFirstDev.Handlers.ImplReviewMerge
  ( ImplInput, ImplExit, implBefore, implAfter
  , TDDReviewImplInput, TDDReviewImplExit, tddReviewImplBefore, tddReviewImplAfter
  , MergerInput, MergerExit, mergerBefore, mergerAfter
  )
import TypesFirstDev.Handlers.Rebaser
  ( RebaserInput, RebaserExit, rebaserBefore, rebaserAfter )
import TypesFirstDev.Handlers.Fork (forkHandler)
import TypesFirstDev.Handlers.ImplBarrier (implBarrierHandler)
import TypesFirstDev.Types.Payloads (InitWorkPayload, TestsReadyPayload, MergeComplete)

-- | Wired handler graph for V3 TDD protocol.
--
-- All 8 node handlers are wired into this record with:
-- * Correct effect constraints validated by GHC
-- * Template compilation via TH
-- * Decision tools for sum type exits (via ClaudeCodeSchema)
-- * Proper routing via Goto and GotoChoice
--
-- The effect interpreter chain provides:
-- 1. Session effect - Claude Code conversation management
-- 2. Memory (TDDMem, ImplMem) - Node state threading
-- 3. Subgraph - Child graph spawning
-- 4. IO - System operations
v3Handlers :: TDDGraph (AsHandler V3Effects)
v3Handlers = TDDGraph
  { -- Entry node (placeholder - no handler needed)
    v3Entry = undefined

    -- ════════════════════════════════════════════════════════════════
    -- PHASE 1: SCAFFOLD
    -- ════════════════════════════════════════════════════════════════

  , v3Scaffold = ClaudeCodeLLMHandler @'Sonnet
      Nothing
      scaffoldCompiled
      scaffoldBefore
      (\(ClaudeCodeResult output _, sid) -> scaffoldAfter (output, sid))

    -- ════════════════════════════════════════════════════════════════
    -- PHASE 2: FORK NODE (pure handler for spawn payload distribution)
    -- ════════════════════════════════════════════════════════════════

  , v3Fork = forkHandler

    -- ════════════════════════════════════════════════════════════════
    -- PHASE 3: TDD WRITE TESTS
    -- ════════════════════════════════════════════════════════════════

  , v3TDDWriteTests = ClaudeCodeLLMHandler @'Sonnet
      Nothing
      tddWriteTestsCompiled
      tddWriteTestsBefore
      (\(ClaudeCodeResult output _, sid) -> tddWriteTestsAfter (output, sid))

    -- ════════════════════════════════════════════════════════════════
    -- PHASE 4: IMPL BARRIER (Subgraph effect for child collection)
    -- ════════════════════════════════════════════════════════════════

  , v3ImplBarrier = implBarrierHandler

    -- ════════════════════════════════════════════════════════════════
    -- PHASE 5: IMPL (with self-loop retry)
    -- ════════════════════════════════════════════════════════════════

  , v3Impl = ClaudeCodeLLMHandler @'Sonnet
      Nothing
      implCompiled
      implBefore
      (\(ClaudeCodeResult output _, sid) -> implAfter (output, sid))

    -- ════════════════════════════════════════════════════════════════
    -- PHASE 6: TDD REVIEW IMPL (decision tools)
    -- ════════════════════════════════════════════════════════════════

  , v3TDDReviewImpl = ClaudeCodeLLMHandler @'Sonnet
      Nothing
      tddReviewImplCompiled
      tddReviewImplBefore
      (\(ClaudeCodeResult output _, sid) -> tddReviewImplAfter (output, sid))

    -- ════════════════════════════════════════════════════════════════
    -- PHASE 7: MERGER
    -- ════════════════════════════════════════════════════════════════

  , v3Merger = ClaudeCodeLLMHandler @'Sonnet
      Nothing
      mergerCompiled
      mergerBefore
      (\(ClaudeCodeResult output _, sid) -> mergerAfter (output, sid))

    -- ════════════════════════════════════════════════════════════════
    -- PHASE 8: REBASER
    -- ════════════════════════════════════════════════════════════════

  , v3Rebaser = ClaudeCodeLLMHandler @'Sonnet
      Nothing
      rebaserCompiled
      rebaserBefore
      (\(ClaudeCodeResult output _, sid) -> rebaserAfter (output, sid))

    -- ════════════════════════════════════════════════════════════════
    -- EXIT
    -- ════════════════════════════════════════════════════════════════

  , v3Exit = undefined  -- Exit node (no handler)
  }
