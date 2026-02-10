{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
    teams :: TeamsTools mode
  }
  deriving (Generic)

config :: RoleConfig (Tools AsHandler)
config =
  RoleConfig
    { roleName = "dev",
      tools =
        Tools
          { pr = filePRTools,
            teams = teamsTools
          },
      hooks = httpDevHooks
    }
