{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | TDD ReviewImpl node - reviews implementation, routes via decision tools.
module TypesFirstDev.Handlers.TDDReviewImpl
  ( TDDReviewImplInput(..)
  , TDDReviewImplExit(..)
  , tddReviewImplBefore
  , tddReviewImplAfter
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effect.Freer (Eff, Member)
import Tidepool.Effect.Session (Session, SessionOperation(..), SessionId)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice, gotoExit)
import Tidepool.Graph.Memory (Memory, getMem, updateMem)
import Tidepool.StructuredOutput (StructuredOutput)
import Tidepool.StructuredOutput.ClaudeCodeSchema (ClaudeCodeSchema(..))
import Tidepool.StructuredOutput.DecisionTools (ToDecisionTools(..))

import TypesFirstDev.Context (TDDReviewImplTemplateCtx(..))
import TypesFirstDev.Types.Core (Spec)
import TypesFirstDev.Types.Shared (Critique, CoverageReport)
import TypesFirstDev.Types.Payloads (InitWorkPayload, ImplResult)
import TypesFirstDev.Types.Memory (TDDMem(..))
import TypesFirstDev.Handlers.TDDWriteTests (TDDWriteTestsInput)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
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
-- HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Before handler: build context, manage shared memory.
tddReviewImplBefore
  :: (Member Session es, Member (Memory TDDMem) es)
  => TDDReviewImplInput
  -> Eff es (TDDReviewImplTemplateCtx, SessionOperation)
tddReviewImplBefore input = do
  mem <- getMem @TDDMem
  let ctx = TDDReviewImplTemplateCtx
        { triSpec = input.triSpec
        , triScaffold = input.triScaffold
        , triImplResult = input.triImplResult
        , triDiff = input.triDiff
        }
  let sessionOp = case mem.tddConversationId of
        Just sid -> ContinueFrom sid
        Nothing -> StartFresh "v3/tdd-review-impl"
  pure (ctx, sessionOp)

-- | After handler: route based on decision tools.
tddReviewImplAfter
  :: (Member Session es, Member (Memory TDDMem) es)
  => (TDDReviewImplExit, SessionId)
  -> Eff es (GotoChoice '[To "v3Merger" MergerInput, To "v3TDDWriteTests" TDDWriteTestsInput, Goto Exit TDDReviewImplExit])
tddReviewImplAfter (exit, sid) = do
  updateMem @TDDMem $ \m -> m { tddConversationId = Just sid }
  case exit of
    TDDApproved _signoff _coverage -> do
      let mergerInput = error "TODO: construct MergerInput"
      pure $ gotoChoice @"v3Merger" mergerInput

    TDDMoreTests _critiques _tests -> do
      let testsInput = error "TODO: construct TDDWriteTestsInput"
      pure $ gotoChoice @"v3TDDWriteTests" testsInput

    TDDReject _reason _missing ->
      pure $ gotoExit exit
