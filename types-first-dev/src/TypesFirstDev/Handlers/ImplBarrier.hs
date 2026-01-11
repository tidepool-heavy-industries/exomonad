{-# LANGUAGE FlexibleContexts #-}

-- | ImplBarrier node - async merge loop waiting for tests + children.
module TypesFirstDev.Handlers.ImplBarrier
  ( implBarrierHandler
  ) where

import Tidepool.Effect.Freer (Eff, Member)
import Tidepool.Effect.Subgraph (Subgraph, getPending, awaitAny)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice)

import TypesFirstDev.Types.Core (Spec)
import TypesFirstDev.Types.Payloads (InitWorkPayload, TestsReadyPayload, MergeComplete)
import TypesFirstDev.Handlers.Impl (ImplInput(..))

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | ImplBarrier handler: async merge loop.
-- Awaits tests + children, merges children, routes to Impl when all complete.
implBarrierHandler
  :: (Member (Subgraph Spec) es)
  => TestsReadyPayload
  -> Eff es (GotoChoice '[To "v3Impl" ImplInput])
implBarrierHandler testsPayload = do
  -- Collect all pending children via awaitAny loop
  children <- collectChildren []

  -- Construct ImplInput with merged children
  let implInput = ImplInput
        { iiSpec = error "TODO: get spec from context"
        , iiScaffold = error "TODO: get scaffold from context"
        , iiTestsReady = testsPayload
        , iiChildMerges = if null children then Nothing else Just children
        , iiAttemptCount = 1
        , iiCritiqueList = Nothing
        }

  pure $ gotoChoice @"v3Impl" implInput

-- | Collect all child MergeComplete results via awaitAny.
collectChildren
  :: (Member (Subgraph Spec) es)
  => [MergeComplete]
  -> Eff es [MergeComplete]
collectChildren acc = do
  pending <- getPending
  case pending of
    [] -> pure (reverse acc)
    _  -> do
      -- Await next child completion
      (_childId, childResult) <- awaitAny
      -- Merge and recurse
      collectChildren (childResult : acc)
