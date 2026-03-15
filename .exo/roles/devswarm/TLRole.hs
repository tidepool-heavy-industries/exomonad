{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | TL role config: spawn, PR, merge tools with stop hook checks.
module TLRole (config, Tools) where

import ExoMonad
import Control.Monad.Freer (Eff)
import ExoMonad.Guest.Lifecycle (getTLPhase)
import ExoMonad.Guest.Effects.StopHook (checkUncommittedWork, getCurrentBranch)
import TLTransitions qualified
import AgentTransition (StopCheckResult(..))
import PRReviewHandler (prReviewEventHandlers)
import ExoMonad.Guest.Tools.MergePR (MergePR)
import ExoMonad.Guest.Types (StopDecision(..), StopHookOutput(..), blockStopResponse, allowStopResponse, allowResponse)
import ExoMonad.Types (HookConfig (..), Effects, defaultSessionStartHook, teamRegistrationPostToolUse)

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
      mPhase <- getTLPhase
      result <- case mPhase of
        Just phase -> case TLTransitions.canExit phase of
          r@(MustBlock _) -> pure r
          ShouldNudge msg -> pure (ShouldNudge msg)
          Clean -> do
            nudge <- checkUncommittedWork branch
            case nudge of
              Just msg -> pure (ShouldNudge msg)
              Nothing -> pure Clean
        Nothing -> do
          nudge <- checkUncommittedWork branch
          case nudge of
            Just msg -> pure (ShouldNudge msg)
            Nothing -> pure Clean
      case result of
        MustBlock msg -> pure $ blockStopResponse msg
        ShouldNudge msg -> pure $ StopHookOutput Allow (Just msg)
        Clean -> pure allowStopResponse

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
