{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Role (config, Tools) where

import ExoMonad

data Tools mode = Tools
  { agents :: AgentTools mode,
    popups :: PopupTools mode,
    messaging :: TLMessagingTools mode,
    coordination :: CoordinationTools mode
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
            messaging = tlMessagingTools,
            coordination = coordinationTools
          },
      hooks = defaultHooks
    }
