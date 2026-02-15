{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | TL role config re-exported under a unique module name for the unified WASM.
--
-- This duplicates the TL Role.hs content because Cabal does not support
-- importing the same module name from two different source directories.
-- The canonical definition remains in @.exomonad/roles/tl/Role.hs@.
module TLRole (config, Tools) where

import ExoMonad
import ExoMonad.Guest.Effects.StopHook (runStopHookChecks)
import ExoMonad.Guest.Tools.MergePR (MergePR)
import ExoMonad.Guest.Types (allowResponse, postToolUseResponse)
import ExoMonad.Types (HookConfig (..))
import Control.Monad.Freer (send)

data Tools mode = Tools
  { spawn :: SpawnTools mode,
    popups :: PopupTools mode,
    messaging :: TLMessagingTools mode,
    pr :: FilePRTools mode,
    mergePr :: mode :- MergePR,
    events :: EventTools mode,
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
            pr = filePRTools,
            mergePr = mkHandler @MergePR,
            events = eventTools,
            notifyParent = mkHandler @NotifyParent
          },
      hooks =
        HookConfig
          { preToolUse = \_ -> pure (allowResponse Nothing),
            postToolUse = \_ -> pure (postToolUseResponse Nothing),
            onStop = \_ -> send runStopHookChecks,
            onSubagentStop = \_ -> send runStopHookChecks
          }
    }
