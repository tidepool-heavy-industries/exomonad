{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Impl → ReviewImpl → Merger routing loop consolidated into single module.
--
-- These three handlers form a cycle in the Goto routing graph:
--   Impl → TDDReviewImpl → Merger → Impl (on rejection)
--
-- By consolidating them into a single module, we eliminate circular imports
-- and keep all the interdependent types in one place.
module TypesFirstDev.Handlers.ImplReviewMerge
  ( -- * Impl types and handlers
    ImplInput(..)
  , ImplExit(..)
  , implBefore
  , implAfter

    -- * TDD ReviewImpl types and handlers
  , TDDReviewImplInput(..)
  , TDDReviewImplExit(..)
  , tddReviewImplBefore
  , tddReviewImplAfter

    -- * Merger types and handlers
  , MergerInput(..)
  , MergerExit(..)
  , MergeRejectedReason(..)
  , mergerBefore
  , mergerAfter
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.Freer (Eff, Member)
import Tidepool.Effect.Session (Session, SessionOperation(..), SessionId)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice, gotoExit)
import Tidepool.Graph.Types (Exit)
import Tidepool.Graph.Memory (Memory, getMem, updateMem)

import TypesFirstDev.Context (ImplTemplateCtx(..), TDDReviewImplTemplateCtx(..), MergerTemplateCtx(..))
import TypesFirstDev.Types.Nodes
  ( ImplInput(..), ImplExit(..)
  , TDDWriteTestsInput(..)
  , TDDReviewImplInput(..), TDDReviewImplExit(..)
  , MergerInput(..), MergerExit(..), MergeRejectedReason(..)
  , ScaffoldInput(..)
  )
import TypesFirstDev.Types.Payloads (InitWorkPayload, TestsReadyPayload, ImplResult(..), MergeComplete(..), TDDApproval)
import TypesFirstDev.Types.Memory (ImplMem(..), TDDMem(..))

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Impl before handler: build context, manage private memory.
--
-- Returns tuple of (context, SessionOperation) for ClaudeCodeLLMHandler.
-- For self-loop retries, stores and reuses session ID from attempt to attempt.
implBefore
  :: (Member Session es, Member (Memory ImplMem) es)
  => ImplInput
  -> Eff es (ImplTemplateCtx, SessionOperation)
implBefore input = do
  mem <- getMem @ImplMem
  -- Store input fields in memory for use in after handler
  updateMem @ImplMem $ \m -> m
    { imImplInput = Just input  -- Store full input for routing
    }
  let ctx = ImplTemplateCtx
        { spec = input.iiSpec
        , scaffold = input.iiScaffold
        , testsReady = input.iiTestsReady
        , childMerges = input.iiChildMerges
        , attemptCount = input.iiAttemptCount
        , critiqueList = input.iiCritiqueList
        }
  -- For retry loops, continue same session to preserve context
  let sessionOp = case mem.imSessionId of
        Just sid -> ContinueFrom sid
        Nothing -> StartFresh "v3/impl"
  pure (ctx, sessionOp)

-- | Impl after handler: self-loop on retry, route to review or exit.
-- Max 5 retry attempts.
implAfter
  :: (Member Session es, Member (Memory ImplMem) es)
  => (ImplExit, SessionId)
  -> Eff es (GotoChoice '[To "v3TDDReviewImpl" TDDReviewImplInput, To "v3Impl" ImplInput, To "v3Scaffold" ScaffoldInput, To Exit ImplExit])
implAfter (exit, sid) = do
  -- Retrieve input from memory
  mem <- getMem @ImplMem
  let inputMaybe = case mem of
        ImplMem { imImplInput = Just inp } -> inp
        _ -> error "BUG: ImplInput not found in memory during implAfter"

  -- Store SessionId in memory for session continuation on retry
  updateMem @ImplMem $ \m -> m { imSessionId = Just sid }
  case exit of
    ImplTestsPassed commit iterations tests -> do
      let reviewInput = TDDReviewImplInput
            { triSpec = inputMaybe.iiSpec
            , triScaffold = inputMaybe.iiScaffold
            , triImplResult = ImplResult commit iterations tests
            , triDiff = ""
            }
      pure $ gotoChoice @"v3TDDReviewImpl" reviewInput

    ImplRequestRetry diagnosis _from _to _failing
      | inputMaybe.iiAttemptCount >= 5 ->
          pure $ gotoExit (ImplStuck diagnosis "Max retries exceeded" 5)
      | otherwise -> do
          let retryInput = inputMaybe { iiAttemptCount = inputMaybe.iiAttemptCount + 1 }
          pure $ gotoChoice @"v3Impl" retryInput

    ImplBlockedDependency _missing _path ->
      pure $ gotoChoice @"v3Scaffold" (error "TODO: Retrieve original ScaffoldInput from ImplMem for clarification" :: ScaffoldInput)

    ImplSpecAmbiguity _spec _contradiction _q ->
      pure $ gotoChoice @"v3Scaffold" (error "TODO: Retrieve original ScaffoldInput from ImplMem for clarification" :: ScaffoldInput)

    ImplStuck diagnosis recommendation attempts ->
      pure $ gotoExit (ImplStuck diagnosis recommendation attempts)

-- | TDD ReviewImpl before handler: build context, manage shared memory.
--
-- Returns tuple of (context, SessionOperation) for ClaudeCodeLLMHandler.
tddReviewImplBefore
  :: (Member Session es, Member (Memory TDDMem) es)
  => TDDReviewImplInput
  -> Eff es (TDDReviewImplTemplateCtx, SessionOperation)
tddReviewImplBefore input = do
  -- Store input for after-handler routing (needed to construct Merger or TDDWriteTests inputs)
  updateMem @TDDMem $ \m -> m { tmConversationId = T.pack (show input.triImplResult) }
  let ctx = TDDReviewImplTemplateCtx
        { spec = input.triSpec
        , scaffold = input.triScaffold
        , implResult = input.triImplResult
        , diff = input.triDiff
        }
  pure (ctx, StartFresh "v3/tdd-review-impl")

-- | TDD ReviewImpl after handler: route based on decision tools.
tddReviewImplAfter
  :: (Member Session es, Member (Memory TDDMem) es)
  => (TDDReviewImplExit, SessionId)
  -> Eff es (GotoChoice '[To "v3Merger" MergerInput, To "v3TDDWriteTests" TDDWriteTestsInput, To Exit TDDReviewImplExit])
tddReviewImplAfter (exit, sid) = do
  updateMem @TDDMem $ \m -> m { tmConversationId = T.pack (show sid) }
  case exit of
    TDDApproved _signoff _coverage -> do
      -- Route to Merger with approval
      -- MergerInput requires parent/child NodeInfo from executor context
      -- For now, use defaults - real implementation gets these from graph executor
      pure $ gotoChoice @"v3Merger" (error "TODO: Executor should provide parent/child NodeInfo to construct MergerInput" :: MergerInput)

    TDDMoreTests _critiques _tests -> do
      -- Route back to TDDWriteTests to write more tests
      pure $ gotoChoice @"v3TDDWriteTests" (error "TODO: Reconstruct TDDWriteTestsInput with new criteria from TDDMem" :: TDDWriteTestsInput)

    TDDReject _reason _missing ->
      pure $ gotoExit exit

-- | Merger before handler: build context.
--
-- Returns tuple of (context, SessionOperation) for ClaudeCodeLLMHandler.
mergerBefore
  :: (Member Session es)
  => MergerInput
  -> Eff es (MergerTemplateCtx, SessionOperation)
mergerBefore input = do
  let ctx = MergerTemplateCtx
        { parentNode = input.miParentNode
        , childNode = input.miChildNode
        , tddApproval = input.miTddApproval
        , contractSuite = input.miContractSuite
        }
  pure (ctx, StartFresh "v3/merger")

-- | Merger after handler: file MR or reject.
mergerAfter
  :: (Member Session es)
  => (MergerExit, SessionId)
  -> Eff es (GotoChoice '[To Exit MergeComplete, To "v3Impl" ImplInput])
mergerAfter (exit, _sid) = case exit of
  MergerComplete commit author impact changes -> do
    let mergeResult = MergeComplete
          { mcCommit = commit
          , mcAuthor = author
          , mcImpactLevel = impact
          , mcChanges = changes
          }
    pure $ gotoExit mergeResult

  MergerRejected _reason _details _failing ->
    -- Route back to Impl for retry
    -- ImplInput requires original spec/scaffold which Impl memory should have
    -- Real implementation retrieves from ImplMem context
    pure $ gotoChoice @"v3Impl" (error "TODO: Retrieve original ImplInput from ImplMem to retry" :: ImplInput)
