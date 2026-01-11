{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

-- | ImplBarrier node - collect child results via Subgraph effect.
--
-- Now a LogicNode (not BarrierNode) with linear flow:
-- Scaffold → TDDWriteTests → ImplBarrier → Impl
--
-- Child collection happens asynchronously via Subgraph.awaitAny.
module TypesFirstDev.Handlers.ImplBarrier
  ( implBarrierHandler
  ) where

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Reader (Reader, ask)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice, gotoExit)
import Tidepool.Graph.Types (Exit)
import Tidepool.Effect.Subgraph (Subgraph, awaitAny, getPending)

import TypesFirstDev.Types.Nodes (ImplInput(..), ScaffoldInput)
import TypesFirstDev.Types.Payloads (TestsReadyPayload(..), MergeComplete(..), ChildFailure(..))
import TypesFirstDev.V3.Interpreters (ExecutionContext(..))

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | ImplBarrier handler: collect child results and merge with tests.
--
-- This handler:
-- 1. Reads Spec and Scaffold from ExecutionContext
-- 2. Awaits all spawned children via Subgraph.awaitAny
-- 3. Separates successful MergeComplete results from failures
-- 4. On failure: currently collects all; scope-based cancellation handles abort
-- 5. Routes to Impl with consolidated ImplInput (successes only)
--
-- Handler type: TestsReadyPayload -> Eff es (GotoChoice '[To "v3Impl" ImplInput, To Exit MergeComplete])
-- (LogicNode - takes input directly, no HList)
implBarrierHandler
  :: ( Member (Subgraph ScaffoldInput MergeComplete) es
     , Member (Reader ExecutionContext) es
     )
  => TestsReadyPayload
  -> Eff es (GotoChoice '[To "v3Impl" ImplInput, To Exit MergeComplete])
implBarrierHandler testsPayload = do
  -- Read context for Spec and Scaffold
  ctx <- ask @ExecutionContext

  -- Collect all pending children via awaitAny
  childResults <- collectAllChildren []

  -- Separate successes from failures
  let (successes, failures) = partitionResults childResults

  -- On failure: scope-based cancellation (ki) handles sibling cleanup
  -- When parent exits scope, ki auto-cancels any remaining children
  -- Log failures for debugging (could emit to observability here)
  let _failureCount = length failures

  -- Extract scaffold from context (required after scaffold completes)
  case ctx.ecScaffold of
    Nothing ->
      -- Can't proceed without scaffold - exit with failure
      let failure = ChildFailure
            { cfReason = "ImplBarrier: No scaffold in execution context"
            , cfBranch = "unknown"
            , cfAttempts = 0
            , cfPartialCommit = Nothing
            , cfFilesCreated = []
            }
      in pure $ gotoExit (MergeFailed failure)

    Just scaffold -> do
      -- Build ImplInput with merged dependencies (successes only)
      let implInput = ImplInput
            { iiSpec = ctx.ecSpec
            , iiScaffold = scaffold
            , iiTestsReady = testsPayload
            , iiChildMerges = if null successes then Nothing else Just (map wrapSuccess successes)
            , iiAttemptCount = 0
            , iiCritiqueList = Nothing
            }
      pure $ gotoChoice @"v3Impl" implInput

-- | Partition results into successes and failures.
partitionResults :: [MergeComplete] -> ([MergeComplete], [ChildFailure])
partitionResults = foldr go ([], [])
  where
    go result (successes, failures) = case result of
      s@MergeSuccess{} -> (s : successes, failures)
      MergeFailed f    -> (successes, f : failures)

-- | Wrap success back to MergeComplete for ImplInput.
wrapSuccess :: MergeComplete -> MergeComplete
wrapSuccess = id  -- Already a MergeComplete

-- | Recursively collect results from all spawned children.
--
-- Uses awaitAny to block until each child completes, accumulating results.
-- When no children remain pending, returns the accumulated list.
collectAllChildren
  :: (Member (Subgraph ScaffoldInput MergeComplete) es)
  => [MergeComplete]
  -> Eff es [MergeComplete]
collectAllChildren acc = do
  pending <- getPending @ScaffoldInput @MergeComplete
  if null pending
    then pure acc
    else do
      (_childId, result) <- awaitAny @ScaffoldInput @MergeComplete
      collectAllChildren (result : acc)
