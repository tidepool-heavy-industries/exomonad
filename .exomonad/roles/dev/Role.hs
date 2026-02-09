{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Role (config, Tools) where

import ExoMonad
import StopHook (devHooks)

data Tools mode = Tools
  { pr :: FilePRTools mode,
    messaging :: MessagingTools mode
  }
  deriving (Generic)

config :: RoleConfig (Tools AsHandler)
config =
  RoleConfig
    { roleName = "dev",
      tools =
        Tools
          { pr = filePRTools,
            messaging = messagingTools
          },
      hooks = devHooks
    }
