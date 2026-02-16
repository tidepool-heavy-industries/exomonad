{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Role (config, Tools) where

import ExoMonad

data Tools mode = Tools
  { spawn :: SpawnTools mode,
    popups :: PopupTools mode,
    messaging :: TLMessagingTools mode,
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
            messaging = tlMessagingTools,
            notifyParent = mkHandler @NotifyParent
          },
      hooks = defaultHooks
    }
