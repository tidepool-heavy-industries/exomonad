{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Pattern exhaustiveness checker doesn't understand GADT constraints for OneOf
-- The remaining patterns are all uninhabited (OneOf '[])

-- | V3 TDD Executor - Direct Handler Chaining
--
-- This module executes the V3 TDD graph by calling handlers directly in
-- sequence, without using the generic dispatch mechanism that requires
-- HasField (which GHC can't derive for records with type-family-computed
-- field types).
--
-- The graph shape is known at compile time:
--
-- @
-- Scaffold → TDDWriteTests → ImplBarrier → Impl → TDDReviewImpl → Merger → Exit
--                                            ↑______|  (retry loop)
--                              ↑____________|  (more tests)
-- @
--
-- Each run*Node function:
-- 1. Calls the handler via 'callHandler' (works for both LLM and Logic nodes)
-- 2. Pattern matches on GotoChoice to extract the next payload
-- 3. Calls the next node handler directly
module TypesFirstDev.Executor
  ( runV3
  ) where

import Control.Monad.Freer (Eff)
import Tidepool.Graph.Execute (CallHandler(..))
import Tidepool.Graph.Goto.Internal (GotoChoice(..), OneOf(..))

import TypesFirstDev.HandlersAssembled (v3Handlers)
import TypesFirstDev.Graph (TDDGraph(..))
import TypesFirstDev.V3.Interpreters (V3Effects)

import TypesFirstDev.Types.Nodes
  ( ScaffoldInput
  , TDDWriteTestsInput
  , ImplInput
  , TDDReviewImplInput
  , MergerInput
  , RebaserInput
  )
import TypesFirstDev.Types.Payloads (MergeComplete, TestsReadyPayload)

-- ════════════════════════════════════════════════════════════════════════════
-- MAIN ENTRY POINT
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the V3 TDD graph from a ScaffoldInput to MergeComplete.
--
-- This is the main entry point for executing the V3 TDD workflow.
-- It chains handlers directly without using generic dispatch.
runV3 :: ScaffoldInput -> Eff V3Effects MergeComplete
runV3 = runScaffoldNode

-- ════════════════════════════════════════════════════════════════════════════
-- PHASE 1: SCAFFOLD
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the Scaffold node.
--
-- Routes to TDDWriteTests (normal) or exits (ClarificationNeeded).
runScaffoldNode :: ScaffoldInput -> Eff V3Effects MergeComplete
runScaffoldNode input = do
  choice <- callHandler (v3Scaffold v3Handlers) input
  -- GotoChoice '[To "v3TDDWriteTests" TDDWriteTestsInput, To Exit ScaffoldExit]
  case choice of
    GotoChoice (Here tddInput) -> runTDDWriteTestsNode tddInput
    GotoChoice (There (Here _scaffoldExit)) ->
      -- ClarificationNeeded exits the graph
      error "Scaffold exited with ClarificationNeeded - not yet supported"

-- ════════════════════════════════════════════════════════════════════════════
-- PHASE 2: TDD WRITE TESTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the TDDWriteTests node.
--
-- Routes to ImplBarrier (tests ready) or back to Scaffold (invalid scaffold).
runTDDWriteTestsNode :: TDDWriteTestsInput -> Eff V3Effects MergeComplete
runTDDWriteTestsNode input = do
  choice <- callHandler (v3TDDWriteTests v3Handlers) input
  -- GotoChoice '[To "v3ImplBarrier" TestsReadyPayload, To "v3Scaffold" ScaffoldInput]
  case choice of
    GotoChoice (Here testsReady) -> runImplBarrierNode testsReady
    GotoChoice (There (Here scaffoldInput)) -> runScaffoldNode scaffoldInput

-- ════════════════════════════════════════════════════════════════════════════
-- PHASE 3: IMPL BARRIER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the ImplBarrier node (LogicNode, not LLM).
--
-- Collects child results via Subgraph effect, then routes to Impl.
runImplBarrierNode :: TestsReadyPayload -> Eff V3Effects MergeComplete
runImplBarrierNode testsPayload = do
  choice <- callHandler (v3ImplBarrier v3Handlers) testsPayload
  -- GotoChoice '[To "v3Impl" ImplInput]
  case choice of
    GotoChoice (Here implInput) -> runImplLoop implInput

-- ════════════════════════════════════════════════════════════════════════════
-- PHASE 4: IMPL (with self-loop retry)
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the Impl node with retry loop support.
--
-- This node can loop back to itself (RequestRetry) up to 5 times.
-- Routes to TDDReviewImpl (tests passed) or errors out (Stuck).
runImplLoop :: ImplInput -> Eff V3Effects MergeComplete
runImplLoop input = do
  choice <- callHandler (v3Impl v3Handlers) input
  -- GotoChoice '[To "v3TDDReviewImpl" TDDReviewImplInput, To "v3Impl" ImplInput,
  --              To "v3Scaffold" ScaffoldInput, To Exit ImplExit]
  case choice of
    GotoChoice (Here reviewInput) -> runTDDReviewImplNode reviewInput
    GotoChoice (There (Here retryInput)) -> runImplLoop retryInput  -- Self-loop
    GotoChoice (There (There (Here scaffoldInput))) -> runScaffoldNode scaffoldInput
    GotoChoice (There (There (There (Here _implExit)))) ->
      -- Impl gave up (Stuck)
      error "Impl exited with Stuck - implementation blocked"

-- ════════════════════════════════════════════════════════════════════════════
-- PHASE 5: TDD REVIEW IMPL
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the TDDReviewImpl node.
--
-- Routes to Merger (approved), TDDWriteTests (more tests needed), or exits (rejected).
runTDDReviewImplNode :: TDDReviewImplInput -> Eff V3Effects MergeComplete
runTDDReviewImplNode input = do
  choice <- callHandler (v3TDDReviewImpl v3Handlers) input
  -- GotoChoice '[To "v3Merger" MergerInput, To "v3TDDWriteTests" TDDWriteTestsInput,
  --              To Exit TDDReviewImplExit]
  case choice of
    GotoChoice (Here mergerInput) -> runMergerNode mergerInput
    GotoChoice (There (Here tddInput)) -> runTDDWriteTestsNode tddInput  -- More tests
    GotoChoice (There (There (Here _reviewExit))) ->
      -- Review rejected the implementation
      error "TDDReviewImpl exited with rejection - implementation not acceptable"

-- ════════════════════════════════════════════════════════════════════════════
-- PHASE 6: MERGER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the Merger node.
--
-- Returns MergeComplete on success, or routes back to Impl (rejected).
runMergerNode :: MergerInput -> Eff V3Effects MergeComplete
runMergerNode input = do
  choice <- callHandler (v3Merger v3Handlers) input
  -- GotoChoice '[To Exit MergeComplete, To "v3Impl" ImplInput]
  case choice of
    GotoChoice (Here mergeComplete) -> pure mergeComplete  -- Done!
    GotoChoice (There (Here implInput)) -> runImplLoop implInput  -- Merge rejected

-- ════════════════════════════════════════════════════════════════════════════
-- PHASE 7: REBASER (currently not in main flow)
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the Rebaser node.
--
-- Triggered by sibling MergeComplete broadcast (outside normal flow).
-- Routes back to TDDWriteTests (adapted) or Scaffold (conflict).
_runRebaserNode :: RebaserInput -> Eff V3Effects MergeComplete
_runRebaserNode input = do
  choice <- callHandler (v3Rebaser v3Handlers) input
  -- GotoChoice '[To "v3TDDWriteTests" TDDWriteTestsInput, To "v3Scaffold" ScaffoldInput]
  case choice of
    GotoChoice (Here tddInput) -> runTDDWriteTestsNode tddInput
    GotoChoice (There (Here scaffoldInput)) -> runScaffoldNode scaffoldInput
