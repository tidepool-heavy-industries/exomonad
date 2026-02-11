{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Role (config, Tools) where

import ExoMonad

data Tools mode = Tools
  { spawn :: SpawnTools mode,
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
          { spawn = spawnTools,
            popups = popupTools,
            messaging = tlMessagingTools,
            coordination = coordinationTools
          },
      hooks = defaultHooks
    }
