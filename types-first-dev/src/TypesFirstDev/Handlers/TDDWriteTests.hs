{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | TDD WriteTests node - writes failing tests for all criteria.
module TypesFirstDev.Handlers.TDDWriteTests
  ( TDDWriteTestsInput(..)
  , TDDWriteTestsExit(..)
  , tddWriteTestsBefore
  , tddWriteTestsAfter
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effect.Freer (Eff, Member)
import Tidepool.Effect.Session (Session, SessionOperation(..))
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice, gotoExit)
import Tidepool.Graph.Memory (Memory, getMem, updateMem)
import Tidepool.StructuredOutput (StructuredOutput)
import Tidepool.StructuredOutput.ClaudeCodeSchema (ClaudeCodeSchema(..))
import Tidepool.StructuredOutput.DecisionTools (ToDecisionTools(..))

import TypesFirstDev.Context (TDDWriteTestsTemplateCtx(..))
import TypesFirstDev.Types.Core (Spec)
import TypesFirstDev.Types.Shared (PlannedTest)
import TypesFirstDev.Types.Payloads (InitWorkPayload, TestsReadyPayload(..))
import TypesFirstDev.Types.Memory (TDDMem(..))

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | TDD WriteTests node input.
data TDDWriteTestsInput = TDDWriteTestsInput
  { twiSpec :: Spec
    -- ^ Work specification
  , twiScaffold :: InitWorkPayload
    -- ^ Scaffold output (interface + contract suite)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | TDD WriteTests node output (oneOf).
data TDDWriteTestsExit
  = TDDTestsReady
      { tweTestsCommit :: Text
        -- ^ Commit hash after tests are written
      , tweTestFiles :: [FilePath]
        -- ^ Test files created
      , twePendingCriteria :: [Text]
        -- ^ Criteria not yet covered
      }
  | TDDInvalidScaffold
      { tweMissingType :: Text
        -- ^ Type that scaffold should have defined but didn't
      , tweExpectedLocation :: FilePath
        -- ^ Where we expected to find the type
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput, ToDecisionTools)

instance ClaudeCodeSchema TDDWriteTestsExit where
  ccDecisionTools = Just (toDecisionTools @TDDWriteTestsExit)
  ccParseToolCall = parseToolCall @TDDWriteTestsExit

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Before handler: build context, manage shared memory.
tddWriteTestsBefore
  :: (Member Session es, Member (Memory TDDMem) es)
  => TDDWriteTestsInput
  -> Eff es (TDDWriteTestsTemplateCtx, SessionOperation)
tddWriteTestsBefore input = do
  mem <- getMem @TDDMem
  let ctx = TDDWriteTestsTemplateCtx
        { twiSpec = input.twiSpec
        , twiScaffold = input.twiScaffold
        }
  let sessionOp = case mem.tddConversationId of
        Just sid -> ContinueFrom sid
        Nothing -> StartFresh "v3/tdd-write-tests"
  pure (ctx, sessionOp)

-- | After handler: route based on exit type.
tddWriteTestsAfter
  :: (Member Session es, Member (Memory TDDMem) es)
  => (TDDWriteTestsExit, SessionId)
  -> Eff es (GotoChoice '[To "v3Impl" TestsReadyPayload, To "v3Scaffold" ScaffoldInput])
tddWriteTestsAfter (exit, sid) = do
  updateMem @TDDMem $ \m -> m { tddConversationId = Just sid }
  case exit of
    TDDTestsReady commit files criteria -> do
      let payload = TestsReadyPayload
            { trpCommit = commit
            , trpTestFiles = files
            , trpPendingCriteria = criteria
            }
      pure $ gotoChoice @"v3Impl" payload

    TDDInvalidScaffold _missing _location ->
      -- Route back to Scaffold for clarification
      error "TODO: construct ScaffoldInput for clarification"
