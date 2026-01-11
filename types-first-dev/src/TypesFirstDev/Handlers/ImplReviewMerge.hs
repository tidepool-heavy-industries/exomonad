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
import Tidepool.Effect.Session (Session, SessionOperation(..), SessionId)
import Tidepool.Effect.GraphContext (GraphContext, getEntry)
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
import TypesFirstDev.Types.Payloads (ImplResult(..), MergeComplete(..), TDDApproval(..), ChildFailure(..))
import TypesFirstDev.Types.Memory (ImplMem(..), TDDMem(..))
import TypesFirstDev.Types.Shared (NodeInfo(..), ClarificationRequest(..), ClarificationType(..), Critique(..))
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
      -- No stored input - can only exit with current exit data
      case exit of
        ImplStuck diagnosis recommendation attempts ->
          pure $ gotoExit (ImplStuck diagnosis recommendation attempts)
        ImplTestsPassed _ _ _ ->
          pure $ gotoExit (ImplStuck "Internal error: tests passed but no input context" "Check memory initialization" 0)
        ImplRequestRetry diagnosis _ _ _ ->
          pure $ gotoExit (ImplStuck diagnosis "Cannot retry: no input context stored" 0)
        ImplBlockedDependency missing path ->
          pure $ gotoExit (ImplStuck ("Blocked on dependency: " <> missing <> " at " <> T.pack path) "Cannot escalate: no input context" 0)
        ImplSpecAmbiguity spec _ question ->
          pure $ gotoExit (ImplStuck ("Spec ambiguity in: " <> spec <> " - " <> question) "Cannot escalate: no input context" 0)

    Just originalInput ->
      case exit of
        ImplTestsPassed commit iterations tests -> do
          let reviewInput = TDDReviewImplInput
                { triSpec = originalInput.iiSpec
                , triScaffold = originalInput.iiScaffold
                , triImplResult = ImplResult commit iterations tests
                , triDiff = ""
                }
          pure $ gotoChoice @"v3TDDReviewImpl" reviewInput

        ImplRequestRetry diagnosis strategyFrom strategyTo failingTests
          | originalInput.iiAttemptCount >= 5 ->
              pure $ gotoExit (ImplStuck diagnosis ("Max retries exceeded. Strategy: " <> strategyFrom <> " → " <> strategyTo) 5)
          | otherwise -> do
              -- Build critiques from retry info so template shows useful feedback
              let strategyCritique = Critique
                    { cqFile = "strategy"
                    , cqLine = originalInput.iiAttemptCount
                    , cqIssue = diagnosis <> " (was: " <> strategyFrom <> ")"
                    , cqRequiredFix = strategyTo
                    }
              let testCritiques = map (\testName -> Critique
                    { cqFile = "test"
                    , cqLine = 0
                    , cqIssue = "Test failed: " <> testName
                    , cqRequiredFix = "Apply strategy: " <> strategyTo
                    }) failingTests
              let allCritiques = strategyCritique : testCritiques
              let retryInput = originalInput
                    { iiAttemptCount = originalInput.iiAttemptCount + 1
                    , iiCritiqueList = Just allCritiques
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
  updateMem @TDDMem $ \m -> m { tmConversationId = T.pack (show sid) }
  ctx <- ask @ExecutionContext
  case exit of
    TDDApproved signoff coverage -> do
      -- Route to Merger with approval
      -- NodeInfo from ExecutionContext (set by executor when creating context)
      let nodeInfo = case ctx.ecNodeInfo of
            Just ni -> ni
            Nothing -> NodeInfo { niId = "root", niBranch = "main" }  -- Default for root
      -- Construct TDDApproval from exit data
      let tddApproval = TDDApproval
            { taSignOff = signoff
            , taCoverageReport = coverage
            }
      let mergerInput = MergerInput
            { miParentNode = nodeInfo
            , miChildNode = nodeInfo  -- Same for non-recursive case
            , miTddApproval = tddApproval
            , miContractSuite = ""     -- Will be set from scaffold when available
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
  :: (Member Session es, Member (Memory ImplMem) es)
  => (MergerExit, SessionId)
  -> Eff es (GotoChoice '[To Exit MergeComplete, To "v3Impl" ImplInput])
mergerAfter (exit, _sid) = case exit of
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
        -- Build critiques from merge rejection so Impl can see what went wrong
        let reasonCritique = Critique
              { cqFile = "merge"
              , cqLine = originalInput.iiAttemptCount
              , cqIssue = "Merge rejected (" <> reasonToText reason <> "): " <> details
              , cqRequiredFix = "Fix the " <> reasonToText reason <> " before merge can proceed"
              }
        let testCritiques = map (\testName -> Critique
              { cqFile = "merge-test"
              , cqLine = 0
              , cqIssue = "Merge test failed: " <> testName
              , cqRequiredFix = "Ensure " <> testName <> " passes after implementation"
              }) failingTests
        let allCritiques = reasonCritique : testCritiques
        let retryInput = originalInput
              { iiAttemptCount = originalInput.iiAttemptCount + 1
              , iiCritiqueList = Just allCritiques
              }
        pure $ gotoChoice @"v3Impl" retryInput
      Nothing ->
        -- Can't retry without input - exit with failure
        let failure = ChildFailure
              { cfReason = "Merger rejected: " <> details <> " (reason: " <> reasonToText reason <> ")"
              , cfBranch = "unknown"  -- We don't have branch info here
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
