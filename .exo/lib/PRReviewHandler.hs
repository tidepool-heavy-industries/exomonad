{-# LANGUAGE OverloadedStrings #-}

module PRReviewHandler
  ( prReviewEventHandlers,
    siblingMergedHandler,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import ExoMonad.Effects.Log qualified as Log
import ExoMonad.Guest.Events (CIStatusEvent (..), EventAction (..), EventHandlerConfig (..), PRReviewEvent (..), SiblingMergedEvent (..), defaultEventHandlers)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect_)
import ExoMonad.Guest.Types (HookEffects)

-- | Event handler config with PR review handling.
-- Timeout handlers use defaults (NoAction).
prReviewEventHandlers :: EventHandlerConfig
prReviewEventHandlers =
  defaultEventHandlers
    { onPRReview = prReviewHandler,
      onCIStatus = ciStatusHandler,
      onSiblingMerged = siblingMergedHandler
    }

-- | Handle PR review events for dev/tl roles.
prReviewHandler :: PRReviewEvent -> Eff HookEffects EventAction
prReviewHandler (ReviewReceived n comments_) = do
  void $ suspendEffect_ @Log.LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = TL.fromStrict $
        "[PRReviewHandler] Review received on PR #" <> T.pack (show n)
    , Log.infoRequestFields = ""
    }
  let msg = "## Copilot Review on PR #" <> T.pack (show n) <> "\n\n"
         <> comments_
         <> "\n\nAddress these comments and push fixes."
  pure (InjectMessage msg)

prReviewHandler (ReviewApproved n) = do
  void $ suspendEffect_ @Log.LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = TL.fromStrict $
        "[PRReviewHandler] PR #" <> T.pack (show n) <> " approved by Copilot"
    , Log.infoRequestFields = ""
    }
  let msg = "PR #" <> T.pack (show n) <> " approved by Copilot review"
  pure (NotifyParentAction msg n)

prReviewHandler (ReviewTimeout n mins) = do
  void $ suspendEffect_ @Log.LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = TL.fromStrict $
        "[PRReviewHandler] PR #" <> T.pack (show n)
        <> " timed out after " <> T.pack (show mins) <> " minutes"
    , Log.infoRequestFields = ""
    }
  let msg = "PR #" <> T.pack (show n)
         <> " — no Copilot review after " <> T.pack (show mins)
         <> " minutes, proceeding with success"
  pure (NotifyParentAction msg n)

-- | Handle sibling merged events.
siblingMergedHandler :: SiblingMergedEvent -> Eff HookEffects EventAction
siblingMergedHandler (SiblingMergedEvent merged parent _prNum) = do
  void $ suspendEffect_ @Log.LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = TL.fromStrict $
        "[PRReviewHandler] Sibling branch merged: " <> merged
    , Log.infoRequestFields = ""
    }
  let msg = "[Sibling Merged] PR on branch " <> merged
         <> " was merged into " <> parent
         <> ". Rebase your branch to pick up the changes: git fetch origin && git rebase origin/" <> parent
  pure (InjectMessage msg)

-- | Handle CI status events.
ciStatusHandler :: CIStatusEvent -> Eff HookEffects EventAction
ciStatusHandler (CIStatusEvent n status_ branch_) = do
  void $ suspendEffect_ @Log.LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = TL.fromStrict $
        "[PRReviewHandler] CI status changed on PR #" <> T.pack (show n)
        <> ": " <> status_
    , Log.infoRequestFields = ""
    }
  let msg = "[CI Status] PR #" <> T.pack (show n) <> " on branch " <> branch_
         <> ": " <> status_
         <> case status_ of
              "success" -> "\n\nCI passed."
              "failure" -> "\n\nCI failed. Check the logs and fix the issue before proceeding."
              _ -> ""
  pure (InjectMessage msg)
