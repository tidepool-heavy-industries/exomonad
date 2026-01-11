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

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Control.Monad.Freer (Eff, Member)
import Tidepool.Effect.Session (Session, SessionOperation(..), SessionId)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice, gotoExit, gotoSelf)
import Tidepool.Graph.Types (Exit, Self)
import Tidepool.Graph.Memory (Memory, getMem, updateMem)
import Tidepool.StructuredOutput (StructuredOutput)
import Tidepool.StructuredOutput.ClaudeCodeSchema (ClaudeCodeSchema(..))
import Tidepool.StructuredOutput.DecisionTools (ToDecisionTools(..))

import TypesFirstDev.Context (ImplTemplateCtx(..), TDDReviewImplTemplateCtx(..), MergerTemplateCtx(..))
import TypesFirstDev.Types.Core (Spec)
import TypesFirstDev.Types.Shared (Critique, CoverageReport, PlannedTest, NodeInfo, ImpactLevel, ChangeEntry)
import TypesFirstDev.Types.Payloads (InitWorkPayload, TestsReadyPayload, ImplResult(..), MergeComplete(..), TDDApproval)
import TypesFirstDev.Types.Memory (ImplMem(..), TDDMem(..))

-- ════════════════════════════════════════════════════════════════════════════
-- IMPL TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Impl node input.
data ImplInput = ImplInput
  { iiSpec :: Spec
    -- ^ Work specification
  , iiScaffold :: InitWorkPayload
    -- ^ Scaffold output (interface + contract suite)
  , iiTestsReady :: TestsReadyPayload
    -- ^ TDD write tests result
  , iiChildMerges :: Maybe [MergeComplete]
    -- ^ Merged child results if any
  , iiAttemptCount :: Int
    -- ^ Current retry attempt (1-5)
  , iiCritiqueList :: Maybe [Critique]
    -- ^ TDD review critiques to address
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Impl node output (oneOf).
data ImplExit
  = ImplTestsPassed
      { ieCommitHash :: Text
        -- ^ Commit hash after all tests pass
      , ieIterations :: Int
        -- ^ Number of iterations to reach passing state
      , iePassedTests :: [Text]
        -- ^ Tests that now pass
      }
  | ImplRequestRetry
      { ieDiagnosis :: Text
        -- ^ Why tests are still failing
      , ieStrategyFrom :: Text
        -- ^ Current approach
      , ieStrategyTo :: Text
        -- ^ Suggested new approach
      , ieFailingTests :: [Text]
        -- ^ Which tests still fail
      }
  | ImplBlockedDependency
      { ieMissingSymbol :: Text
        -- ^ Symbol that needs to be imported
      , ieExpectedImportPath :: FilePath
        -- ^ Where we expect to find it
      }
  | ImplSpecAmbiguity
      { ieSpecSentence :: Text
        -- ^ Confusing sentence from spec
      , ieContradictionTrace :: Text
        -- ^ How it conflicts with other parts
      , ieQuestion :: Text
        -- ^ What clarification is needed
      }
  | ImplStuck
      { ieStuckDiagnosis :: Text
        -- ^ Why we're stuck
      , ieStuckRecommendation :: Text
        -- ^ Suggested next step
      , ieStuckAttempts :: Int
        -- ^ How many attempts made
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput, ToDecisionTools)

instance ClaudeCodeSchema ImplExit where
  ccDecisionTools = Just (toDecisionTools @ImplExit)
  ccParseToolCall = parseToolCall @ImplExit

-- ════════════════════════════════════════════════════════════════════════════
-- TDD REVIEWIMPL TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | TDD ReviewImpl node input.
data TDDReviewImplInput = TDDReviewImplInput
  { triSpec :: Spec
    -- ^ Work specification
  , triScaffold :: InitWorkPayload
    -- ^ Scaffold output
  , triImplResult :: ImplResult
    -- ^ Implementation result to review
  , triDiff :: Text
    -- ^ Git diff of implementation changes
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | TDD ReviewImpl node output (oneOf).
data TDDReviewImplExit
  = TDDApproved
      { treSignOff :: Text
        -- ^ Approval message
      , treCoverageReport :: CoverageReport
        -- ^ Code coverage details
      }
  | TDDMoreTests
      { treCritiques :: [Critique]
        -- ^ What needs improvement
      , treAdditionalTests :: [PlannedTest]
        -- ^ Additional tests to write
      }
  | TDDReject
      { treReason :: Text
        -- ^ Why implementation rejected
      , treMissingCriteria :: [Text]
        -- ^ Criteria that still fail
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput, ToDecisionTools)

instance ClaudeCodeSchema TDDReviewImplExit where
  ccDecisionTools = Just (toDecisionTools @TDDReviewImplExit)
  ccParseToolCall = parseToolCall @TDDReviewImplExit

-- ════════════════════════════════════════════════════════════════════════════
-- MERGER TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Merger node input.
data MergerInput = MergerInput
  { miParentNode :: NodeInfo
    -- ^ Parent node information
  , miChildNode :: NodeInfo
    -- ^ Child node being merged
  , miTddApproval :: TDDApproval
    -- ^ TDD approval from child
  , miContractSuite :: FilePath
    -- ^ Path to contract suite for verification
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Merger node output (oneOf).
data MergerExit
  = MergerComplete
      { mexCommit :: Text
        -- ^ Commit hash of merged work
      , mexAuthor :: Text
        -- ^ Author of merge commit
      , mexImpactLevel :: ImpactLevel
        -- ^ Impact level (minor/moderate/major)
      , mexChanges :: [ChangeEntry]
        -- ^ Summary of changes made
      }
  | MergerRejected
      { mexReason :: MergeRejectedReason
        -- ^ Why merge was rejected
      , mexDetails :: Text
        -- ^ Details of rejection
      , mexFailingTests :: [Text]
        -- ^ Tests that fail
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput, ToDecisionTools)

instance ClaudeCodeSchema MergerExit where
  ccDecisionTools = Just (toDecisionTools @MergerExit)
  ccParseToolCall = parseToolCall @MergerExit

-- | Reason for merge rejection.
data MergeRejectedReason
  = ContractViolation
  | BuildFailure
  | IntegrationFailure
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

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
  => ImplInput
  -> (ImplExit, SessionId)
  -> Eff es (GotoChoice '[To Self ImplInput, To "v3TDDReviewImpl" TDDReviewImplInput, To Exit ImplExit])
implAfter input (exit, sid) = do
  -- Store SessionId in memory for session continuation on retry
  updateMem @ImplMem $ \m -> m { imSessionId = Just sid }
  case exit of
    ImplTestsPassed commit iterations tests -> do
      let reviewInput = TDDReviewImplInput
            { triSpec = input.iiSpec
            , triScaffold = input.iiScaffold
            , triImplResult = ImplResult commit iterations tests
            , triDiff = ""
            }
      pure $ gotoChoice @"v3TDDReviewImpl" reviewInput

    ImplRequestRetry diagnosis _from _to _failing
      | input.iiAttemptCount >= 5 ->
          pure $ gotoExit (ImplStuck diagnosis "Max retries exceeded" 5)
      | otherwise -> do
          let retryInput = input { iiAttemptCount = input.iiAttemptCount + 1 }
          pure $ gotoSelf retryInput

    ImplBlockedDependency _missing _path ->
      error "TODO: Route back to Scaffold for clarification"

    ImplSpecAmbiguity _spec _contradiction _q ->
      error "TODO: Route back to Scaffold for clarification"

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
  -> Eff es (GotoChoice '[To "v3Merger" MergerInput, To "v3TDDWriteTests" TDDReviewImplInput, To Exit TDDReviewImplExit])
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
      pure $ gotoChoice @"v3TDDWriteTests" (error "TODO: Reconstruct TDDWriteTestsInput with new criteria from TDDMem" :: TDDReviewImplInput)

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
  => MergerExit
  -> Eff es (GotoChoice '[To Exit MergeComplete, To "v3Impl" ImplInput])
mergerAfter exit = case exit of
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
