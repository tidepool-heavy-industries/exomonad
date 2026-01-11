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

import qualified Data.Text as T

import Control.Monad.Freer (Eff, Member)
import Tidepool.Effect.Session (Session, SessionOperation(..), SessionId)
import Tidepool.Effect.GraphContext (GraphContext, getEntry)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice)
import Tidepool.Graph.Memory (Memory, getMem, updateMem)

import TypesFirstDev.Context (TDDWriteTestsTemplateCtx(..))
import TypesFirstDev.Types.Core (Criterion(..), Spec(..))
import TypesFirstDev.Types.Nodes (TDDWriteTestsInput(..), TDDWriteTestsExit(..), ScaffoldInput(..))
import TypesFirstDev.Types.Payloads (TestsReadyPayload(..))
import TypesFirstDev.Types.Memory (TDDMem(..))
import TypesFirstDev.Types.Shared (ClarificationRequest(..), ClarificationType(..))

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
        , reviewCritiques = mem.tmReviewCritiques
        }
  pure (ctx, StartFresh "v3/tdd-write-tests")

-- | After handler: route based on exit type.
--
-- Linear flow: TDDWriteTests → ImplBarrier (via Goto, not Arrive).
-- ImplBarrier is now a LogicNode, not a BarrierNode.
tddWriteTestsAfter
  :: ( Member Session es
     , Member (Memory TDDMem) es
     , Member (GraphContext ScaffoldInput) es
     )
  => (TDDWriteTestsExit, SessionId)
  -> Eff es (GotoChoice '[To "v3ImplBarrier" TestsReadyPayload, To "v3Scaffold" ScaffoldInput])
tddWriteTestsAfter (exit, sid) = do
  updateMem @TDDMem $ \m -> m { tmConversationId = T.pack (show sid) }
  case exit of
    TDDTestsReady commit files pendingCriteria -> do
      -- Update covered criteria = all criteria - pending criteria
      entry <- getEntry @ScaffoldInput
      let allCriteria :: [Criterion]
          allCriteria = entry.siSpec.sAcceptanceCriteria
      let allCriteriaIds = map (\c -> c.cId) allCriteria
      let coveredIds = filter (`notElem` pendingCriteria) allCriteriaIds
      updateMem @TDDMem $ \m -> m { tmCoveredCriteria = coveredIds }
      let payload = TestsReadyPayload
            { trpCommit = commit
            , trpTestFiles = files
            , trpPendingCriteria = pendingCriteria
            }
      pure $ gotoChoice @"v3ImplBarrier" payload

    TDDInvalidScaffold missingType expectedLocation -> do
      -- Route back to Scaffold with clarification about what's missing
      entry <- getEntry @ScaffoldInput
      let clarification = ClarificationRequest
            { crType = InvalidScaffold
            , crDetails = "Missing " <> missingType <> " at " <> T.pack expectedLocation
            , crQuestion = "Please provide the " <> missingType <> " in the scaffold output."
            }
      let updatedEntry = entry { siClarificationNeeded = Just clarification }
      pure $ gotoChoice @"v3Scaffold" updatedEntry
