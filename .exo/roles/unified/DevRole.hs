{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Dev role config: PR and notify tools with permission cascade and stop hook checks.
module DevRole (config, Tools) where

import ExoMonad
import HttpDevHooks (httpDevHooks)

data Tools mode = Tools
  { pr :: FilePRTools mode,
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
            notifyParent = mkHandler @NotifyParent
          },
      hooks = httpDevHooks
    }
