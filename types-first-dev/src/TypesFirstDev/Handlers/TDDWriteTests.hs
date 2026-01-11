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
import qualified Data.Text as T
import GHC.Generics (Generic)

import Control.Monad.Freer (Eff, Member)
import Tidepool.Effect.Session (Session, SessionOperation(..), SessionId)
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
import TypesFirstDev.Handlers.Scaffold (ScaffoldInput)

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
--
-- Returns tuple of (context, SessionOperation) for ClaudeCodeLLMHandler.
-- Uses StartFresh for now - session continuation will be implemented later.
tddWriteTestsBefore
  :: (Member Session es, Member (Memory TDDMem) es)
  => TDDWriteTestsInput
  -> Eff es (TDDWriteTestsTemplateCtx, SessionOperation)
tddWriteTestsBefore input = do
  mem <- getMem @TDDMem
  let ctx = TDDWriteTestsTemplateCtx
        { spec = input.twiSpec
        , scaffold = input.twiScaffold
        , coveredCriteria = mem.tmCoveredCriteria
        }
  pure (ctx, StartFresh "v3/tdd-write-tests")

-- | After handler: route based on exit type.
tddWriteTestsAfter
  :: (Member Session es, Member (Memory TDDMem) es)
  => (TDDWriteTestsExit, SessionId)
  -> Eff es (GotoChoice '[To "v3Impl" TestsReadyPayload, To "v3Scaffold" ScaffoldInput])
tddWriteTestsAfter (exit, sid) = do
  updateMem @TDDMem $ \m -> m { tmConversationId = T.pack (show sid) }
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
      -- ScaffoldInput requires original spec from Scaffold - needs executor context
      pure $ gotoChoice @"v3Scaffold" (error "TODO: Retrieve original ScaffoldInput from TDDMem for clarification" :: ScaffoldInput)
