{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Worker role config: notify_parent only, allow-all hooks.
module WorkerRole (config, Tools) where

import ExoMonad
import ExoMonad.Guest.Types (allowResponse, allowStopResponse)
import Telemetry (telemetryPostToolUse)
import ExoMonad.Types (HookConfig (..), defaultSessionStartHook)

data Tools mode = Tools
  { notifyParent :: mode :- NotifyParent,
    sendMessage :: mode :- SendMessage,
    shutdown :: mode :- Shutdown
  }
  deriving (Generic)

config :: RoleConfig (Tools AsHandler)
config =
  RoleConfig
    { roleName = "worker",
      tools =
        Tools
          { notifyParent = mkHandler @NotifyParent,
            sendMessage = mkHandler @SendMessage,
            shutdown = mkHandler @Shutdown
          },
      hooks =
        HookConfig
          { preToolUse = \_ -> pure (allowResponse Nothing),
            postToolUse = telemetryPostToolUse,
            onStop = \_ -> pure allowStopResponse,
            onSubagentStop = \_ -> pure allowStopResponse,
            onSessionStart = defaultSessionStartHook
          },
      eventHandlers = defaultEventHandlers
    }
