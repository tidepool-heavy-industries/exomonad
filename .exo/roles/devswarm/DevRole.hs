{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Dev role config: PR and notify tools with permission cascade and stop hook checks.
module DevRole (config, Tools) where

import ExoMonad
import HttpDevHooks (httpDevHooks)
import PRReviewHandler (prReviewEventHandlers)
import Telemetry (telemetryPostToolUse)

data Tools mode = Tools
  { pr :: FilePRTools mode,
    notifyParent :: mode :- NotifyParent,
    sendMessage :: mode :- SendMessage,
    shutdown :: mode :- Shutdown
  }
  deriving (Generic)

config :: RoleConfig (Tools AsHandler)
config =
  RoleConfig
    { roleName = "dev",
      tools =
        Tools
          { pr = filePRTools,
            notifyParent = mkHandler @NotifyParent,
            sendMessage = mkHandler @SendMessage,
            shutdown = mkHandler @Shutdown
          },
      hooks = httpDevHooks { postToolUse = telemetryPostToolUse },
      eventHandlers = prReviewEventHandlers
    }
