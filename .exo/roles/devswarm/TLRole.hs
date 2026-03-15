{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | TL role config: spawn, PR, merge tools with stop hook checks.
module TLRole (config, Tools) where

import ExoMonad
import Control.Monad.Freer (Eff)
import ExoMonad.Guest.Lifecycle (getTLPhase)
import TLTransitions qualified
import AgentTransition (StopCheckResult(..))
import PRReviewHandler (prReviewEventHandlers)
import ExoMonad.Guest.Tools.MergePR (MergePR)
import ExoMonad.Guest.Types (StopDecision(..), StopHookOutput(..), blockStopResponse, allowStopResponse, allowResponse)
import ExoMonad.Types (HookConfig (..), Effects, defaultSessionStartHook, teamRegistrationPostToolUse)
import Data.Text qualified as T

data Tools mode = Tools
  { spawn :: SpawnTools mode,
    pr :: FilePRTools mode,
    mergePr :: mode :- MergePR,
    notifyParent :: mode :- NotifyParent,
    sendMessage :: mode :- SendMessage
  }
  deriving (Generic)

tlStopCheck :: Eff Effects StopHookOutput
tlStopCheck = do
  mPhase <- getTLPhase
  case mPhase of
    Just phase -> case TLTransitions.canExit phase of
      MustBlock msg -> pure $ blockStopResponse (T.pack msg)
      ShouldNudge msg -> pure $ StopHookOutput Allow (Just (T.pack msg))
      Clean -> pure allowStopResponse
    Nothing -> pure allowStopResponse

config :: RoleConfig (Tools AsHandler)
config =
  RoleConfig
    { roleName = "tl",
      tools =
        Tools
          { spawn = spawnTools,
            pr = filePRTools,
            mergePr = mkHandler @MergePR,
            notifyParent = mkHandler @NotifyParent,
            sendMessage = mkHandler @SendMessage
          },
      hooks =
        HookConfig
          { preToolUse = \_ -> pure (allowResponse Nothing),
            postToolUse = teamRegistrationPostToolUse,
            onStop = \_ -> tlStopCheck,
            onSubagentStop = \_ -> tlStopCheck,
            onSessionStart = defaultSessionStartHook
          },
      eventHandlers = prReviewEventHandlers
    }
