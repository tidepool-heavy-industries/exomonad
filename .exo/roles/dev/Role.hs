{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Role (config, Tools) where

import ExoMonad
import StopHook (devHooks)

data Tools mode = Tools
  { pr :: FilePRTools mode,
    messaging :: MessagingTools mode,
    notifyParent :: mode :- NotifyParent
  }
  deriving (Generic)

config :: RoleConfig (Tools AsHandler)
config =
  RoleConfig
    { roleName = "dev",
      tools =
        Tools
          { pr = filePRTools,
            messaging = messagingTools,
            notifyParent = mkHandler @NotifyParent
          },
      hooks = devHooks
    }
