{-# LANGUAGE OverloadedStrings #-}

module PRReviewHandler
  ( prReviewEventHandlers,
    siblingMergedHandler,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import ExoMonad.Effects.Log qualified as Log
import ExoMonad.Guest.Events (CIStatusEvent (..), EventAction (..), EventHandlerConfig (..), PRReviewEvent (..), SiblingMergedEvent (..), defaultEventHandlers)
import ExoMonad.Guest.Events.Templates qualified as Tpl
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
  pure (InjectMessage (Tpl.copilotReviewReceived n comments_))

prReviewHandler (ReviewApproved n) = do
  void $ suspendEffect_ @Log.LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = TL.fromStrict $
        "[PRReviewHandler] PR #" <> T.pack (show n) <> " approved by Copilot"
    , Log.infoRequestFields = ""
    }
  pure (NotifyParentAction (Tpl.prReady n) n)

prReviewHandler (ReviewTimeout n mins) = do
  void $ suspendEffect_ @Log.LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = TL.fromStrict $
        "[PRReviewHandler] PR #" <> T.pack (show n)
        <> " timed out after " <> T.pack (show mins) <> " minutes"
    , Log.infoRequestFields = ""
    }
  pure (NotifyParentAction (Tpl.reviewTimeout n mins) n)

prReviewHandler (FixesPushed n ci) = do
  void $ suspendEffect_ @Log.LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = TL.fromStrict $
        "[PRReviewHandler] Fixes pushed on PR #" <> T.pack (show n)
        <> ", CI: " <> ci
    , Log.infoRequestFields = ""
    }
  pure (NotifyParentAction (Tpl.fixesPushed n ci) n)

-- | Handle sibling merged events.
siblingMergedHandler :: SiblingMergedEvent -> Eff HookEffects EventAction
siblingMergedHandler (SiblingMergedEvent merged parent _prNum) = do
  void $ suspendEffect_ @Log.LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = TL.fromStrict $
        "[PRReviewHandler] Sibling branch merged: " <> merged
    , Log.infoRequestFields = ""
    }
  pure (InjectMessage (Tpl.siblingMerged merged parent))

-- | Handle CI status events.
ciStatusHandler :: CIStatusEvent -> Eff HookEffects EventAction
ciStatusHandler (CIStatusEvent n status_ branch_) = do
  void $ suspendEffect_ @Log.LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = TL.fromStrict $
        "[PRReviewHandler] CI status changed on PR #" <> T.pack (show n)
        <> ": " <> status_
    , Log.infoRequestFields = ""
    }
  pure (InjectMessage (Tpl.ciStatus n status_ branch_))
