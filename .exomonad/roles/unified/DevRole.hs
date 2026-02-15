{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Dev role config re-exported under a unique module name for the unified WASM.
--
-- This duplicates the Dev Role.hs content because Cabal does not support
-- importing the same module name from two different source directories.
-- The canonical definition remains in @.exomonad/roles/dev/Role.hs@.
module DevRole (config, Tools) where

import ExoMonad
import HttpDevHooks (httpDevHooks)

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
      hooks = httpDevHooks
    }
