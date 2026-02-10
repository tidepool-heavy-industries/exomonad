{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Role (config, Tools) where

import ExoMonad
import PostToolUseHook (teamCreateHook)

data Tools mode = Tools
  { agents :: AgentTools mode,
    popups :: PopupTools mode
  }
  deriving (Generic)

config :: RoleConfig (Tools AsHandler)
config =
  RoleConfig
    { roleName = "tl",
      tools =
        Tools
          { agents = agentTools,
            popups = popupTools
          },
      hooks = defaultHooks {postToolUse = teamCreateHook}
    }
