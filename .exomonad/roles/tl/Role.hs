{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Role (config, Tools) where

import ExoMonad
import PostToolUseHook (teamCreateHook)

data Tools mode = Tools
  { agents :: AgentTools mode,
    popups :: PopupTools mode,
    messaging :: TLMessagingTools mode
  }
  deriving (Generic)

config :: RoleConfig (Tools AsHandler)
config =
  RoleConfig
    { roleName = "tl",
      tools =
        Tools
          { agents = agentTools,
            popups = popupTools,
            messaging = tlMessagingTools
          },
      hooks = defaultHooks {postToolUse = teamCreateHook}
    }
