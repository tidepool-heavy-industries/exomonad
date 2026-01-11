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

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.Freer (Eff, Member)
import Tidepool.Effect.Session (Session, SessionOperation(..), SessionId)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice, gotoExit, gotoArrive)
import Tidepool.Graph.Types (Arrive)
import Tidepool.Graph.Memory (Memory, getMem, updateMem)

import TypesFirstDev.Context (TDDWriteTestsTemplateCtx(..))
import TypesFirstDev.Types.Nodes (TDDWriteTestsInput(..), TDDWriteTestsExit(..), ScaffoldInput(..))
import TypesFirstDev.Types.Payloads (InitWorkPayload, TestsReadyPayload(..))
import TypesFirstDev.Types.Memory (TDDMem(..))

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
  -> Eff es (GotoChoice '[To (Arrive "v3ImplBarrier") TestsReadyPayload, To "v3Scaffold" ScaffoldInput])
tddWriteTestsAfter (exit, sid) = do
  updateMem @TDDMem $ \m -> m { tmConversationId = T.pack (show sid) }
  case exit of
    TDDTestsReady commit files criteria -> do
      let payload = TestsReadyPayload
            { trpCommit = commit
            , trpTestFiles = files
            , trpPendingCriteria = criteria
            }
      pure $ gotoArrive @"v3ImplBarrier" payload

    TDDInvalidScaffold _missing _location ->
      -- Route back to Scaffold for clarification
      -- ScaffoldInput requires original spec from Scaffold - needs executor context
      pure $ gotoChoice @"v3Scaffold" (error "TODO: Retrieve original ScaffoldInput from TDDMem for clarification" :: ScaffoldInput)
