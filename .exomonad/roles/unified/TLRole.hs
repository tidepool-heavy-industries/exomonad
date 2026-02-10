{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | TL role config re-exported under a unique module name for the unified WASM.
--
-- This duplicates the TL Role.hs content because Cabal does not support
-- importing the same module name from two different source directories.
-- The canonical definition remains in @.exomonad/roles/tl/Role.hs@.
module TLRole (config, Tools) where

import ExoMonad
import PostToolUseHook (teamCreateHook)

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
      hooks = defaultHooks {postToolUse = teamCreateHook}
    }
