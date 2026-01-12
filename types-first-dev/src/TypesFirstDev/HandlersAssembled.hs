{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Assembled handler record for V3 TDD graph.
--
-- This module wires all 7 node handlers into a complete TDDGraph record
-- for execution via the effect interpreter stack.
--
-- Linear flow: Scaffold → TDDWriteTests → ImplBarrier → Impl → ...
-- Child spawning/collection via Subgraph effect (not Fork/Barrier nodes).
--
-- Architecture:
-- * Handlers declare effect constraints (Member Effect es)
-- * Handler record is instantiated with concrete V3Effects list
-- * GHC validates constraints at compile time
-- * Effect interpreter chain at runtime (Session → Memory → Subgraph → IO)
module TypesFirstDev.HandlersAssembled
  ( v3Handlers
  ) where

import Data.Proxy (Proxy(..))
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
import TypesFirstDev.Handlers.ImplBarrier (implBarrierHandler)

-- | Wired handler graph for V3 TDD protocol.
--
-- All 7 node handlers are wired into this record with:
-- * Correct effect constraints validated by GHC
-- * Template compilation via TH
-- * Decision tools for sum type exits (via ClaudeCodeSchema)
-- * Proper routing via Goto and GotoChoice
--
-- The effect interpreter chain provides:
-- 1. Session effect - Claude Code conversation management
-- 2. Memory (TDDMem, ImplMem) - Node state threading
-- 3. Subgraph - Child graph spawning (via effect, not Fork node)
-- 4. IO - System operations
v3Handlers :: TDDGraph (AsHandler V3Effects)
v3Handlers = TDDGraph
  { -- Entry node marker (Proxy indicates entry point type)
    v3Entry = Proxy

    -- ════════════════════════════════════════════════════════════════
    -- SCAFFOLD
    -- ════════════════════════════════════════════════════════════════

  , v3Scaffold = ClaudeCodeLLMHandler @'Sonnet
      Nothing
      scaffoldCompiled
      scaffoldBefore
      (\(ClaudeCodeResult output _, sid) -> scaffoldAfter (output, sid))

    -- ════════════════════════════════════════════════════════════════
    -- TDD WRITE TESTS
    -- ════════════════════════════════════════════════════════════════

  , v3TDDWriteTests = ClaudeCodeLLMHandler @'Sonnet
      Nothing
      tddWriteTestsCompiled
      tddWriteTestsBefore
      (\(ClaudeCodeResult output _, sid) -> tddWriteTestsAfter (output, sid))

    -- ════════════════════════════════════════════════════════════════
    -- IMPL BARRIER (Subgraph effect for child collection)
    -- ════════════════════════════════════════════════════════════════

  , v3ImplBarrier = implBarrierHandler

    -- ════════════════════════════════════════════════════════════════
    -- IMPL (with self-loop retry)
    -- ════════════════════════════════════════════════════════════════

  , v3Impl = ClaudeCodeLLMHandler @'Sonnet
      Nothing
      implCompiled
      implBefore
      (\(ClaudeCodeResult output _, sid) -> implAfter (output, sid))

    -- ════════════════════════════════════════════════════════════════
    -- TDD REVIEW IMPL (decision tools)
    -- ════════════════════════════════════════════════════════════════

  , v3TDDReviewImpl = ClaudeCodeLLMHandler @'Sonnet
      Nothing
      tddReviewImplCompiled
      tddReviewImplBefore
      (\(ClaudeCodeResult output _, sid) -> tddReviewImplAfter (output, sid))

    -- ════════════════════════════════════════════════════════════════
    -- MERGER
    -- ════════════════════════════════════════════════════════════════

  , v3Merger = ClaudeCodeLLMHandler @'Sonnet
      Nothing
      mergerCompiled
      mergerBefore
      (\(ClaudeCodeResult output _, sid) -> mergerAfter (output, sid))

    -- ════════════════════════════════════════════════════════════════
    -- REBASER
    -- ════════════════════════════════════════════════════════════════

  , v3Rebaser = ClaudeCodeLLMHandler @'Sonnet
      Nothing
      rebaserCompiled
      rebaserBefore
      (\(ClaudeCodeResult output _, sid) -> rebaserAfter (output, sid))

    -- ════════════════════════════════════════════════════════════════
    -- EXIT
    -- ════════════════════════════════════════════════════════════════

  , v3Exit = Proxy  -- Exit node marker (Proxy indicates exit type)
  }
