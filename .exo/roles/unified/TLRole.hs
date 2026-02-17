{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | TL role config: spawn, PR, merge, popup tools with stop hook checks.
module TLRole (config, Tools) where

import ExoMonad
import ExoMonad.Guest.Effects.StopHook (runStopHookChecks)
import ExoMonad.Guest.Tools.MergePR (MergePR)
import ExoMonad.Guest.Types (allowResponse, postToolUseResponse)
import ExoMonad.Types (HookConfig (..), defaultSessionStartHook)
import Control.Monad.Freer (send)

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
            postToolUse = \_ -> pure (postToolUseResponse Nothing),
            onStop = \_ -> send runStopHookChecks,
            onSubagentStop = \_ -> send runStopHookChecks,
            onSessionStart = defaultSessionStartHook
          }
    }
