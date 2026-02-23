{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | TL role config: spawn, PR, merge tools with stop hook checks.
-- Popup disabled: blocks WASM plugin lock for up to 30min, preventing all other MCP calls.
module TLRole (config, Tools) where

import ExoMonad
import ExoMonad.Guest.Effects.StopHook (runStopHookChecks)
import ExoMonad.Guest.Tools.MergePR (MergePR)
import ExoMonad.Guest.Types (allowResponse)
import ExoMonad.Types (HookConfig (..), defaultSessionStartHook, teamRegistrationPostToolUse)
import Control.Monad.Freer (send)

data Tools mode = Tools
  { spawn :: SpawnTools mode,
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
            pr = filePRTools,
            mergePr = mkHandler @MergePR,
            notifyParent = mkHandler @NotifyParent
          },
      hooks =
        HookConfig
          { preToolUse = \_ -> pure (allowResponse Nothing),
            postToolUse = teamRegistrationPostToolUse,
            onStop = \_ -> send runStopHookChecks,
            onSubagentStop = \_ -> send runStopHookChecks,
            onSessionStart = defaultSessionStartHook
          }
    }
