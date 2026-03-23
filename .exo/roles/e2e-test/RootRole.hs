{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | E2E test root role: minimal tools with PII rewriting hooks.
-- The Gemini root agent uses httpDevHooks for BeforeModel/AfterModel rewriting.
-- Only send_message tool is needed (Gemini writes files via its native tools).
module RootRole (config, Tools) where

import ExoMonad
import ExoMonad.Guest.Types (allowStopResponse, BeforeModelOutput (..), AfterModelOutput (..))
import ExoMonad.Types (HookConfig (..), defaultSessionStartHook)
import HttpDevHooks (httpDevHooks)

data Tools mode = Tools
  { sendMessage :: mode :- SendMessage
  }
  deriving (Generic)

config :: RoleConfig (Tools AsHandler)
config =
  RoleConfig
    { roleName = "root",
      tools = Tools
        { sendMessage = mkHandler @SendMessage
        },
      hooks = httpDevHooks,
      eventHandlers = defaultEventHandlers
    }
