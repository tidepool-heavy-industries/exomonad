{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | TL role config: spawn, PR, merge tools with stop hook checks.
module TLRole (config, Tools) where

import ExoMonad
import Control.Monad.Freer (Eff)
import ExoMonad.Guest.StateMachine (StopCheckResult(..), checkExit)
import ExoMonad.Guest.Effects.StopHook (checkUncommittedWork, getCurrentBranch)
import PRReviewHandler (prReviewEventHandlers)
import ExoMonad.Guest.Tools.MergePR (MergePR)
import ExoMonad.Guest.Types (StopDecision(..), StopHookOutput(..), blockStopResponse, allowStopResponse, allowResponse)
import ExoMonad.Types (HookConfig (..), Effects, defaultSessionStartHook, teamRegistrationPostToolUse)
import TLPhase (TLPhase(..), TLEvent)

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
  branch <- getCurrentBranch
  if branch `elem` ["main", "master"]
    then pure allowStopResponse
    else do
      result <- checkExit @TLPhase @TLEvent TLPlanning
      case result of
        MustBlock msg -> pure $ blockStopResponse msg
        ShouldNudge msg -> pure $ StopHookOutput Allow (Just msg)
        Clean -> do
          nudge <- checkUncommittedWork branch
          case nudge of
            Just msg -> pure $ StopHookOutput Allow (Just msg)
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
