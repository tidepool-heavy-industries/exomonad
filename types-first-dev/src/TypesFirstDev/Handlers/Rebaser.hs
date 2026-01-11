{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Rebaser node - adapts to sibling changes after their merge.
module TypesFirstDev.Handlers.Rebaser
  ( RebaserInput(..)
  , RebaserExit(..)
  , rebaserBefore
  , rebaserAfter
  ) where

import Data.Text (Text)

import Control.Monad.Freer (Eff, Member)
import Tidepool.Effect.Session (Session, SessionOperation(..), SessionId)
import Tidepool.Graph.Goto (To, GotoChoice, gotoChoice)

import TypesFirstDev.Context (RebaserTemplateCtx(..))
import TypesFirstDev.Types.Nodes (RebaserInput(..), RebaserExit(..), TDDWriteTestsInput(..), ScaffoldInput(..))

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
rebaserAfter
  :: (Member Session es)
  => (RebaserExit, SessionId)
  -> Eff es (GotoChoice '[To "v3TDDWriteTests" TDDWriteTestsInput, To "v3Scaffold" ScaffoldInput])
rebaserAfter (exit, _sid) = case exit of
  RebaserClean _newBase ->
    pure $ gotoChoice @"v3TDDWriteTests" (error "TODO: Get TDDWriteTestsInput from memory with updated parent branch" :: TDDWriteTestsInput)

  RebaserAdapted _newBase _adaptations ->
    pure $ gotoChoice @"v3TDDWriteTests" (error "TODO: Get TDDWriteTestsInput with adaptations from memory" :: TDDWriteTestsInput)

  RebaserConflict _file _ours _theirs _why ->
    -- Escalate unresolvable conflict back to Scaffold for human review
    pure $ gotoChoice @"v3Scaffold" (error "TODO: Get ScaffoldInput from memory for conflict escalation" :: ScaffoldInput)
