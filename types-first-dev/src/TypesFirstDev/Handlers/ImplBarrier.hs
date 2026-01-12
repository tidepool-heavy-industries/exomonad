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
import Data.Text (Text)
import qualified Data.Text as T
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice, gotoExit)
import Tidepool.Graph.Types (Exit)
import Tidepool.Effect.Subgraph (Subgraph, ChildId, awaitAny, getPending)

import TypesFirstDev.Types.Nodes (ImplInput(..), ScaffoldInput)
import TypesFirstDev.Types.Payloads (TestsReadyPayload(..), MergeComplete(..), ChildFailure(..))
import TypesFirstDev.Types.Shared (ChildFailureFeedback(..), NodeInfo(..))
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

  -- Collect all pending children via awaitAny (tracks childId with each result)
  childResults <- collectAllChildren []

  -- Separate successes from failures (keeping childIds for diagnostics)
  let (successes, failures) = partitionResults childResults

  -- Extract scaffold from context (required after scaffold completes)
  case ctx.ecScaffold of
    Nothing ->
      -- Can't proceed without scaffold - exit with failure
      let branch = maybe "unknown" (\(ni :: NodeInfo) -> ni.niBranch) ctx.ecNodeInfo
          failure = ChildFailure
                { cfReason = "ImplBarrier: No scaffold in execution context"
                , cfBranch = branch
                , cfAttempts = 0
                , cfPartialCommit = Nothing
                , cfFilesCreated = []
                }
      in pure $ gotoExit (MergeFailed failure)

    Just scaffold -> do
      -- Build ImplInput with merged dependencies (successes only)
      -- Child failure feedback is included so Impl knows what went wrong
      let implInput = ImplInput
            { iiSpec = ctx.ecSpec
            , iiScaffold = scaffold
            , iiTestsReady = testsPayload
            , iiChildMerges = if null successes then Nothing else Just (map (wrapSuccess . snd) successes)
            , iiAttemptCount = 0
            , iiRetryFeedback = Nothing
            , iiChildFailures = buildFailureCritiques failures
            , iiMergeRejections = Nothing
            , iiCodeReviews = Nothing
            }
      pure $ gotoChoice @"v3Impl" implInput

-- | Partition results into successes and failures, preserving childIds.
partitionResults :: [(ChildId, MergeComplete)] -> ([(ChildId, MergeComplete)], [(ChildId, ChildFailure)])
partitionResults = foldr go ([], [])
  where
    go (childId, result) (successes, failures) = case result of
      s@MergeSuccess{} -> ((childId, s) : successes, failures)
      MergeFailed f    -> (successes, (childId, f) : failures)

-- | Wrap success back to MergeComplete for ImplInput.
wrapSuccess :: MergeComplete -> MergeComplete
wrapSuccess = id  -- Already a MergeComplete

-- | Build child failure feedback from failed children so Impl knows what went wrong.
buildFailureCritiques :: [(ChildId, ChildFailure)] -> Maybe [ChildFailureFeedback]
buildFailureCritiques failures
  | null failures = Nothing
  | otherwise = Just $ map mkFeedback failures
  where
    mkFeedback (childId, cf) = ChildFailureFeedback
      { cffChildId = T.pack (show childId)
      , cffReason = cf.cfReason
      , cffAttempts = cf.cfAttempts
      , cffPartialCommit = cf.cfPartialCommit
      }

-- | Recursively collect results from all spawned children.
--
-- Uses awaitAny to block until each child completes, accumulating results.
-- When no children remain pending, returns the accumulated list.
-- Returns (ChildId, MergeComplete) pairs to track which child produced each result.
collectAllChildren
  :: (Member (Subgraph ScaffoldInput MergeComplete) es)
  => [(ChildId, MergeComplete)]
  -> Eff es [(ChildId, MergeComplete)]
collectAllChildren acc = do
  pending <- getPending @ScaffoldInput @MergeComplete
  if null pending
    then pure acc
    else do
      (childId, result) <- awaitAny @ScaffoldInput @MergeComplete
      collectAllChildren ((childId, result) : acc)
