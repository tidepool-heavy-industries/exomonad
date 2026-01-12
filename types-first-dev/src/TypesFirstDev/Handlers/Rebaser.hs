{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

-- | Rebaser node - adapts to sibling changes after their merge.
module TypesFirstDev.Handlers.Rebaser
  ( RebaserInput(..)
  , RebaserExit(..)
  , rebaserBefore
  , rebaserAfter
  ) where

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Reader (Reader, ask)
import qualified Data.Text as T
import Tidepool.Effect.Session (Session, SessionOperation(..), SessionId)
import Tidepool.Effect.GraphContext (GraphContext, getEntry)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice, gotoExit)
import Tidepool.Graph.Types (Exit)

import TypesFirstDev.Context (RebaserTemplateCtx(..))
import TypesFirstDev.Types.Nodes (RebaserInput(..), RebaserExit(..), TDDWriteTestsInput(..), ScaffoldInput(..))
import TypesFirstDev.Types.Shared (ClarificationRequest(..), ClarificationType(..))
import TypesFirstDev.V3.Interpreters (ExecutionContext(..))

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Before handler: build context.
--
-- Returns tuple of (context, SessionOperation) for ClaudeCodeLLMHandler.
rebaserBefore
  :: (Member Session es)
  => RebaserInput
  -> Eff es (RebaserTemplateCtx, SessionOperation)
rebaserBefore input = do
  let ctx = RebaserTemplateCtx
        { node = input.riNode
        , parentBranch = input.riParentBranch
        , newParentHead = input.riNewParentHead
        , mergeEvent = input.riMergeEvent
        }
  pure (ctx, StartFresh "v3/rebaser")

-- | After handler: route based on rebase result.
--
-- On success (clean or adapted): routes to TDDWriteTests to continue TDD cycle.
-- On conflict: routes to Scaffold for human review/clarification.
rebaserAfter
  :: ( Member Session es
     , Member (Reader ExecutionContext) es
     , Member (GraphContext ScaffoldInput) es
     )
  => (RebaserExit, SessionId)
  -> Eff es (GotoChoice '[To "v3TDDWriteTests" TDDWriteTestsInput, To "v3Scaffold" ScaffoldInput, To Exit RebaserExit])
rebaserAfter (exit, _sid) = do
  -- Note: SessionId not stored here because Rebaser has no self-retry loop.
  -- On success, routes to TDDWriteTests (new node); on conflict, exits.
  ctx <- ask @ExecutionContext
  case exit of
    RebaserClean _newBase -> do
      -- Rebase succeeded cleanly - continue TDD with updated base
      case ctx.ecScaffold of
        Just scaffold -> do
          let tddInput = TDDWriteTestsInput
                { twiSpec = ctx.ecSpec
                , twiScaffold = scaffold
                }
          pure $ gotoChoice @"v3TDDWriteTests" tddInput
        Nothing ->
          -- Can't continue without scaffold - this shouldn't happen after rebase
          pure $ gotoExit (RebaserConflict "internal" "Missing scaffold context" "" "Cannot continue: no scaffold in execution context")

    RebaserAdapted _newBase _adaptations -> do
      -- Rebase required adaptations - continue TDD (adaptations already applied)
      case ctx.ecScaffold of
        Just scaffold -> do
          let tddInput = TDDWriteTestsInput
                { twiSpec = ctx.ecSpec
                , twiScaffold = scaffold
                }
          pure $ gotoChoice @"v3TDDWriteTests" tddInput
        Nothing ->
          pure $ gotoExit (RebaserConflict "internal" "Missing scaffold context" "" "Cannot continue: no scaffold in execution context")

    RebaserConflict conflictFile ourChange theirChange whyUnresolvable -> do
      -- Unresolvable conflict - escalate back to Scaffold with conflict details
      entry <- getEntry @ScaffoldInput
      let clarification = ClarificationRequest
            { crType = MergeConflict
            , crDetails = "Conflict in " <> T.pack conflictFile <> ":\n"
                <> "Our change: " <> ourChange <> "\n"
                <> "Their change: " <> theirChange
            , crQuestion = whyUnresolvable
            }
      let updatedEntry = entry { siClarificationNeeded = Just clarification }
      pure $ gotoChoice @"v3Scaffold" updatedEntry
