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
import ExoMonad.Guest.Events.Templates qualified as Tpl
import ExoMonad.Guest.StateMachine (applyEvent)
import ExoMonad.Guest.Effects.StopHook (getCurrentBranch)
import DevPhase (DevPhase(..), DevEvent(..))
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect_)
import ExoMonad.Guest.Types (Effects)

-- | Event handler config with PR review handling.
prReviewEventHandlers :: EventHandlerConfig
prReviewEventHandlers =
  defaultEventHandlers
    { onPRReview = prReviewHandler,
      onCIStatus = ciStatusHandler,
      onSiblingMerged = siblingMergedHandler
    }

-- | Handle PR review events for dev/tl roles.
prReviewHandler :: PRReviewEvent -> Eff Effects EventAction
prReviewHandler (ReviewReceived n comments_) = do
  logHandler $ "Review received on PR #" <> T.pack (show n)
  branch <- getCurrentBranch
  void $ applyEvent @DevPhase @DevEvent branch DevSpawned (ReviewReceivedEv n comments_)
  pure (InjectMessage (Tpl.copilotReviewReceived n comments_))

prReviewHandler (ReviewApproved n) = do
  logHandler $ "PR #" <> T.pack (show n) <> " approved by Copilot"
  branch <- getCurrentBranch
  void $ applyEvent @DevPhase @DevEvent branch DevSpawned (ReviewApprovedEv n)
  pure (NotifyParentAction (Tpl.prReady n) n)

prReviewHandler (ReviewTimeout n mins) = do
  logHandler $ "PR #" <> T.pack (show n) <> " timed out after " <> T.pack (show mins) <> " minutes"
  pure (NotifyParentAction (Tpl.reviewTimeout n mins) n)

prReviewHandler (FixesPushed n ci) = do
  logHandler $ "Fixes pushed on PR #" <> T.pack (show n) <> ", CI: " <> ci
  branch <- getCurrentBranch
  void $ applyEvent @DevPhase @DevEvent branch DevSpawned (FixesPushedEv n ci)
  pure (NotifyParentAction (Tpl.fixesPushed n ci) n)

prReviewHandler (CommitsPushed n ci) = do
  logHandler $ "New commits pushed on PR #" <> T.pack (show n) <> ", CI: " <> ci
  branch <- getCurrentBranch
  void $ applyEvent @DevPhase @DevEvent branch DevSpawned (CommitsPushedEv n ci)
  pure (NotifyParentAction (Tpl.commitsPushed n ci) n)

-- | Handle sibling merged events.
siblingMergedHandler :: SiblingMergedEvent -> Eff Effects EventAction
siblingMergedHandler (SiblingMergedEvent merged parent _prNum) = do
  logHandler $ "Sibling branch merged: " <> merged
  pure (InjectMessage (Tpl.siblingMerged merged parent))

-- | Handle CI status events.
ciStatusHandler :: CIStatusEvent -> Eff Effects EventAction
ciStatusHandler (CIStatusEvent n status_ branch_) = do
  logHandler $ "CI status changed on PR #" <> T.pack (show n) <> ": " <> status_
  pure (InjectMessage (Tpl.ciStatus n status_ branch_))

-- | Helper to log handler entry.
logHandler :: Text -> Eff Effects ()
logHandler msg =
  void $ suspendEffect_ @Log.LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = TL.fromStrict $ "[PRReviewHandler] " <> msg
    , Log.infoRequestFields = ""
    }
