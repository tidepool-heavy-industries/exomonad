{-# LANGUAGE OverloadedStrings #-}

module PRReviewHandler
  ( prReviewHandler,
    EventAction(..),
    PRReviewEvent(..),
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import ExoMonad.Effects.Log qualified as Log
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect_)
import ExoMonad.Types (HookEffects)

-- | Event action types (mirrors what ExoMonad.Guest.Events will define).
-- These are temporary — will be replaced by imports from Events module after Wave 1a.
data EventAction
  = InjectMessage Text
  | NotifyParentAction Text Int  -- message, pr_number
  | NoAction

-- | PR review event types (mirrors what ExoMonad.Guest.Events will define).
data PRReviewEvent
  = ReviewReceived
      { prNumber :: Int,
        comments :: Text
      }
  | ReviewApproved
      { prNumber :: Int
      }
  | ReviewTimeout
      { prNumber :: Int,
        minutesElapsed :: Int
      }

-- | Handle PR review events for dev/tl roles.
prReviewHandler :: PRReviewEvent -> Eff HookEffects EventAction
prReviewHandler (ReviewReceived n comments_) = do
  void $ suspendEffect_ @Log.LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = TL.fromStrict $
        "[PRReviewHandler] Review received on PR #" <> T.pack (show n)
    , Log.infoRequestFields = ""
    }
  -- Inject the review comments into the agent's pane
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
  -- Auto-notify parent on approval
  let msg = "PR #" <> T.pack (show n) <> " approved by Copilot review"
  pure (NotifyParentAction msg n)

prReviewHandler (ReviewTimeout n mins) = do
  void $ suspendEffect_ @Log.LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = TL.fromStrict $
        "[PRReviewHandler] PR #" <> T.pack (show n)
        <> " timed out after " <> T.pack (show mins) <> " minutes"
    , Log.infoRequestFields = ""
    }
  -- Auto-notify parent on timeout (Copilot never reviewed)
  let msg = "PR #" <> T.pack (show n)
         <> " — no Copilot review after " <> T.pack (show mins)
         <> " minutes, proceeding with success"
  pure (NotifyParentAction msg n)
