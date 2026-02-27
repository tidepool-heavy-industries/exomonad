{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | TL role config: spawn, PR, merge, popup tools with stop hook checks.
module TLRole (config, Tools) where

import ExoMonad
import ExoMonad.Guest.Effects.StopHook (runStopHookChecks)
import ExoMonad.Guest.Tools.MergePR (MergePR)
import ExoMonad.Guest.Types (allowResponse)
import ExoMonad.Types (HookConfig (..), defaultSessionStartHook, teamRegistrationPostToolUse)

data Tools mode = Tools
  { spawn :: SpawnTools mode,
    popups :: PopupTools mode,
    pr :: FilePRTools mode,
    mergePr :: mode :- MergePR,
    notifyParent :: mode :- NotifyParent
  }
  deriving (Generic)

config :: RoleConfig (Tools AsHandler)
config =
  RoleConfig
    { roleName = "tl",
      tools =
        Tools
          { spawn = spawnTools,
            popups = popupTools,
            pr = filePRTools,
            mergePr = mkHandler @MergePR,
            notifyParent = mkHandler @NotifyParent
          },
      hooks =
        HookConfig
          { preToolUse = \_ -> pure (allowResponse Nothing),
            postToolUse = teamRegistrationPostToolUse,
            onStop = \_ -> runStopHookChecks,
            onSubagentStop = \_ -> runStopHookChecks,
            onSessionStart = defaultSessionStartHook
          }
    }
