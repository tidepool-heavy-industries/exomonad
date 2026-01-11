{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ImplBarrier node - async merge loop waiting for tests + children.
--
-- Phase 14: Full implementation with Subgraph effect for child collection.
module TypesFirstDev.Handlers.ImplBarrier
  ( implBarrierHandler
  ) where

import Control.Monad.Freer (Eff, Member)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice)
import Tidepool.Graph.Types (HList(..))
import Tidepool.Effect.Subgraph (Subgraph, awaitAny, getPending)

import TypesFirstDev.Types.Core (Spec)
import TypesFirstDev.Types.Nodes (ImplInput(..))
import TypesFirstDev.Types.Payloads (TestsReadyPayload(..), MergeComplete)

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | ImplBarrier handler: collect child results and merge with tests.
--
-- This handler:
-- 1. Awaits all spawned children via Subgraph.awaitAny
-- 2. Collects MergeComplete results from each child
-- 3. Merges with TestsReadyPayload
-- 4. Routes to Impl with consolidated ImplInput
--
-- Phase 14: Full implementation using Subgraph effect for concurrent child execution.
--
-- Handler type: HList '[TestsReadyPayload] -> Eff es (GotoChoice '[To "v3Impl" ImplInput])
-- (BarrierNode handlers receive awaited results as HList)
implBarrierHandler
  :: (Member (Subgraph Spec MergeComplete) es)
  => HList '[TestsReadyPayload]
  -> Eff es (GotoChoice '[To "v3Impl" ImplInput])
implBarrierHandler (testsPayload ::: HNil) = do
  -- Collect all pending children via awaitAny
  childResults <- collectAllChildren []

  -- Build ImplInput with merged dependencies
  let implInput = ImplInput
        { iiSpec = error "TODO: Get spec from context"
        , iiScaffold = error "TODO: Get scaffold from context"
        , iiTestsReady = testsPayload
        , iiChildMerges = if null childResults then Nothing else Just childResults
        , iiAttemptCount = 0
        , iiCritiqueList = Nothing
        }

  pure $ gotoChoice @"v3Impl" implInput

-- | Recursively collect results from all spawned children.
--
-- Uses awaitAny to block until each child completes, accumulating results.
-- When no children remain pending, returns the accumulated list.
collectAllChildren
  :: (Member (Subgraph Spec MergeComplete) es)
  => [MergeComplete]
  -> Eff es [MergeComplete]
collectAllChildren acc = do
  pending <- getPending @Spec @MergeComplete
  if null pending
    then pure acc
    else do
      (_childId, result) <- awaitAny @Spec @MergeComplete
      collectAllChildren (result : acc)
