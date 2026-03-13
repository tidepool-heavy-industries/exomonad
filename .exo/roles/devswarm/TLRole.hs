{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | TL role config: spawn, PR, merge tools with stop hook checks.
module TLRole (config, Tools) where

import ExoMonad
import ExoMonad.Guest.Effects.StopHook (runStopHookChecks)
import PRReviewHandler (prReviewEventHandlers)
import ExoMonad.Guest.Tools.MergePR (MergePR)
import ExoMonad.Guest.Types (allowResponse)
import ExoMonad.Types (HookConfig (..), defaultSessionStartHook, teamRegistrationPostToolUse)

data Tools mode = Tools
  { spawn :: SpawnTools mode,
    pr :: FilePRTools mode,
    mergePr :: mode :- MergePR,
    notifyParent :: mode :- NotifyParent,
    sendMessage :: mode :- SendMessage
  }
  deriving (Generic)

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
            onStop = \_ -> runStopHookChecks,
            onSubagentStop = \_ -> runStopHookChecks,
            onSessionStart = defaultSessionStartHook
          },
      eventHandlers = prReviewEventHandlers
    }
