{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | TL role config re-exported under a unique module name for the unified WASM.
--
-- This duplicates the TL Role.hs content because Cabal does not support
-- importing the same module name from two different source directories.
-- The canonical definition remains in @.exomonad/roles/tl/Role.hs@.
module TLRole (config, Tools) where

import ExoMonad

data Tools mode = Tools
  { spawn :: SpawnTools mode,
    popups :: PopupTools mode,
    messaging :: TLMessagingTools mode,
    pr :: FilePRTools mode,
    events :: EventTools mode
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
            pr = filePRTools,
            events = eventTools
          },
      hooks = defaultHooks
    }
