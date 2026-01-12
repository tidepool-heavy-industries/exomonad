{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

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
import Control.Monad.Freer.Reader (Reader, ask)
import Tidepool.Effect.Session (Session, SessionOperation(..), SessionId(..))
import Tidepool.Effect.GraphContext (GraphContext, getEntry)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice, gotoExit)
import Tidepool.Graph.Types (Exit)
import Tidepool.Graph.Memory (Memory, getMem, updateMem)

import TypesFirstDev.Context (ImplTemplateCtx(..), TDDReviewImplTemplateCtx(..), MergerTemplateCtx(..))
import TypesFirstDev.Types.Nodes
  ( ImplInput(..), ImplExit(..)
  , RetryFeedback(..)
  , TDDWriteTestsInput(..)
  , TDDReviewImplInput(..), TDDReviewImplExit(..)
  , MergerInput(..), MergerExit(..), MergeRejectedReason(..)
  , ScaffoldInput(..)
  )
import TypesFirstDev.Types.Payloads (ImplResult(..), MergeComplete(..), TDDApproval(..), ChildFailure(..))
import TypesFirstDev.Types.Memory (ImplMem(..), TDDMem(..))
import TypesFirstDev.Types.Shared (NodeInfo(..), ClarificationRequest(..), ClarificationType(..), Critique(..), MergeRejectionFeedback(..))
import TypesFirstDev.V3.Interpreters (ExecutionContext(..))

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
        , mergeRejections = input.iiMergeRejections
        , childFailures = input.iiChildFailures
        , codeReviews = input.iiCodeReviews
        }
  -- For retry loops, continue same session to preserve context
  let sessionOp = case mem.imSessionId of
        Just sid -> ContinueFrom sid
        Nothing -> StartFresh "v3/impl"
  pure (ctx, sessionOp)

-- | Impl after handler: self-loop on retry, route to review or exit.
-- Max 5 retry attempts.
implAfter
  :: ( Member Session es
     , Member (Memory ImplMem) es
     , Member (GraphContext ScaffoldInput) es
     )
  => (ImplExit, SessionId)
  -> Eff es (GotoChoice '[To "v3TDDReviewImpl" TDDReviewImplInput, To "v3Impl" ImplInput, To "v3Scaffold" ScaffoldInput, To Exit ImplExit])
implAfter (exit, sid) = do
  -- Retrieve input from memory
  mem <- getMem @ImplMem
  -- Store SessionId in memory for session continuation on retry
  updateMem @ImplMem $ \m -> m { imSessionId = Just sid }

  case mem.imImplInput of
    Nothing ->
      -- No stored input - cannot route properly, convert all exits to ImplStuck
      -- This is a defensive error case indicating memory initialization failure
      case exit of
        ImplStuck diagnosis recommendation attempts ->
          pure $ gotoExit (ImplStuck diagnosis recommendation attempts)
        ImplTestsPassed commit iterations passedTests ->
          -- Tests passed but we can't route to TDDReviewImpl without original input
          let diagnosis = "Internal error: tests passed (commit: " <> commit <> ", "
                       <> T.pack (show iterations) <> " iterations, "
                       <> T.pack (show (length passedTests)) <> " tests) but no input context"
          in pure $ gotoExit (ImplStuck diagnosis "Check memory initialization - imImplInput was not stored in before-handler" 0)
        ImplRequestRetry diagnosis strategyFrom strategyTo failingTests ->
          let fullDiagnosis = diagnosis <> " (strategy: " <> strategyFrom <> " → " <> strategyTo
                           <> ", " <> T.pack (show (length failingTests)) <> " failing tests)"
          in pure $ gotoExit (ImplStuck fullDiagnosis "Cannot retry: no input context stored" 0)
        ImplBlockedDependency missing path ->
          pure $ gotoExit (ImplStuck ("Blocked on dependency: " <> missing <> " at " <> T.pack path) "Cannot escalate: no input context" 0)
        ImplSpecAmbiguity spec contradictionTrace question ->
          pure $ gotoExit (ImplStuck ("Spec ambiguity in: " <> spec <> " - " <> question <> " [trace: " <> contradictionTrace <> "]") "Cannot escalate: no input context" 0)

    Just originalInput ->
      case exit of
        ImplTestsPassed commit iterations tests -> do
          let reviewInput = TDDReviewImplInput
                { triSpec = originalInput.iiSpec
                , triScaffold = originalInput.iiScaffold
                , triImplResult = ImplResult commit iterations tests
                }
          pure $ gotoChoice @"v3TDDReviewImpl" reviewInput

        ImplRequestRetry diagnosis strategyFrom strategyTo failingTests
          | originalInput.iiAttemptCount >= 5 ->
              pure $ gotoExit (ImplStuck diagnosis ("Max retries exceeded. Strategy: " <> strategyFrom <> " → " <> strategyTo) 5)
          | otherwise -> do
              -- Build retry feedback for next attempt
              let nextAttempt = originalInput.iiAttemptCount + 1
              let feedback = RetryFeedback
                    { rfAttemptNumber = nextAttempt
                    , rfStrategyFailed = strategyFrom
                    , rfStrategyNext = strategyTo
                    , rfDiagnosis = diagnosis
                    , rfFailingTests = failingTests
                    }
              let retryInput = originalInput
                    { iiAttemptCount = nextAttempt
                    , iiRetryFeedback = Just feedback
                    }
              pure $ gotoChoice @"v3Impl" retryInput

        ImplBlockedDependency missingSymbol expectedPath -> do
          -- Back-route to Scaffold with info about missing dependency
          entry <- getEntry @ScaffoldInput
          let clarification = ClarificationRequest
                { crType = BlockedDependency
                , crDetails = "Cannot find '" <> missingSymbol <> "' expected at " <> T.pack expectedPath
                , crQuestion = "Please ensure " <> missingSymbol <> " is defined and exported from the interface."
                }
          let updatedEntry = entry { siClarificationNeeded = Just clarification }
          pure $ gotoChoice @"v3Scaffold" updatedEntry

        ImplSpecAmbiguity specSentence contradictionTrace question -> do
          -- Back-route to Scaffold with info about spec ambiguity
          entry <- getEntry @ScaffoldInput
          let clarification = ClarificationRequest
                { crType = SpecAmbiguity
                , crDetails = "Ambiguous spec: \"" <> specSentence <> "\"\nContradiction: " <> contradictionTrace
                , crQuestion = question
                }
          let updatedEntry = entry { siClarificationNeeded = Just clarification }
          pure $ gotoChoice @"v3Scaffold" updatedEntry

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
  -- Note: after-handler uses ExecutionContext for routing, not Memory
  -- TDDMem.tmConversationId is for session tracking, updated in after-handler
  let ctx = TDDReviewImplTemplateCtx
        { spec = input.triSpec
        , scaffold = input.triScaffold
        , implResult = input.triImplResult
        }
  pure (ctx, StartFresh "v3/tdd-review-impl")

-- | TDD ReviewImpl after handler: route based on decision tools.
--
-- On approval: routes to Merger with NodeInfo from ExecutionContext.
-- On more tests needed: routes back to TDDWriteTests.
-- On rejection: exits with rejection.
tddReviewImplAfter
  :: ( Member Session es
     , Member (Memory TDDMem) es
     , Member (Reader ExecutionContext) es
     )
  => (TDDReviewImplExit, SessionId)
  -> Eff es (GotoChoice '[To "v3Merger" MergerInput, To "v3TDDWriteTests" TDDWriteTestsInput, To Exit TDDReviewImplExit])
tddReviewImplAfter (exit, sid) = do
  updateMem @TDDMem $ \m -> m { tmConversationId = sid.unSessionId }
  ctx <- ask @ExecutionContext
  case exit of
    TDDApproved signoff coverage -> do
      -- Route to Merger with approval
      -- NodeInfo from ExecutionContext (set by executor when creating context)
      case ctx.ecNodeInfo of
        Nothing ->
          -- No NodeInfo means executor didn't properly initialize context
          -- This is a configuration error - exit with rejection rather than use
          -- a potentially incorrect default
          pure $ gotoExit (TDDReject "Internal error: no NodeInfo in execution context" ["Node context initialization"])

        Just nodeInfo -> do
          -- Construct TDDApproval from exit data
          let tddApproval = TDDApproval
                { taSignOff = signoff
                , taCoverageReport = coverage
                }
          let mergerInput = MergerInput
                { miParentNode = nodeInfo
                , miChildNode = nodeInfo  -- Same for non-recursive case
                , miTddApproval = tddApproval
                , miContractSuite = ""     -- TODO: set from scaffold when available
                }
          pure $ gotoChoice @"v3Merger" mergerInput

    TDDMoreTests critiques additionalTests -> do
      -- Route back to TDDWriteTests to write more tests
      -- Store critiques in TDDMem so TDDWriteTests can access them
      updateMem @TDDMem $ \m -> m
        { tmPendingTests = map (\t -> T.pack (show t)) additionalTests  -- Planned tests to write
        , tmReviewCritiques = map (\c -> c.cqIssue <> " (fix: " <> c.cqRequiredFix <> ")") critiques
        }
      -- Reconstruct TDDWriteTestsInput from ExecutionContext
      case ctx.ecScaffold of
        Just scaffold -> do
          let tddInput = TDDWriteTestsInput
                { twiSpec = ctx.ecSpec
                , twiScaffold = scaffold
                }
          pure $ gotoChoice @"v3TDDWriteTests" tddInput
        Nothing ->
          -- Can't continue without scaffold - exit with rejection
          pure $ gotoExit (TDDReject "Cannot continue: no scaffold in execution context" [])

    TDDReject reason missingCriteria ->
      -- Exit preserves the reason and missing criteria
      pure $ gotoExit (TDDReject reason missingCriteria)

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
  :: (Member Session es, Member (Memory ImplMem) es, Member (Reader ExecutionContext) es)
  => (MergerExit, SessionId)
  -> Eff es (GotoChoice '[To Exit MergeComplete, To "v3Impl" ImplInput])
mergerAfter (exit, _sid) = do
  -- Note: SessionId not stored here. On success, exits (no further routing).
  -- On rejection, routes back to Impl (which stores its own SessionId for continuation).
  ctx <- ask @ExecutionContext
  case exit of
    MergerComplete commit author impact changes -> do
      let mergeResult = MergeSuccess
            { mcCommit = commit
            , mcAuthor = author
            , mcImpactLevel = impact
            , mcChanges = changes
            }
      pure $ gotoExit mergeResult

    MergerRejected reason details failingTests -> do
      -- Route back to Impl for retry - retrieve stored ImplInput from memory
      mem <- getMem @ImplMem
      case mem.imImplInput of
        Just originalInput -> do
          -- Create merge rejection feedback from rejection info
          let mergeRejectionFeedback = MergeRejectionFeedback
                { mrfReason = reasonToText reason
                , mrfDetails = details
                , mrfAttemptNumber = originalInput.iiAttemptCount + 1
                , mrfFailingTests = failingTests
                }
          let retryInput = originalInput
                { iiAttemptCount = originalInput.iiAttemptCount + 1
                , iiMergeRejections = Just [mergeRejectionFeedback]
                , iiCodeReviews = Nothing
                , iiChildFailures = Nothing
                }
          pure $ gotoChoice @"v3Impl" retryInput
        Nothing ->
          -- Can't retry without input - exit with failure
          let branch = maybe "unknown" (\(ni :: NodeInfo) -> ni.niBranch) ctx.ecNodeInfo
              failure = ChildFailure
                    { cfReason = "Merger rejected: " <> details <> " (reason: " <> reasonToText reason <> ")"
                    , cfBranch = branch
                    , cfAttempts = 0
                    , cfPartialCommit = Nothing
                    , cfFilesCreated = []
                    }
          in pure $ gotoExit (MergeFailed failure)
  where
    reasonToText :: MergeRejectedReason -> Text
    reasonToText ContractViolation = "contract violation"
    reasonToText BuildFailure = "build failure"
    reasonToText IntegrationFailure = "integration failure"
