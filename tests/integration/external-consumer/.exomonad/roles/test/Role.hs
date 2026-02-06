{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Minimal test role to verify external consumer WASM builds work.
-- Uses PingTools to validate the full tool dispatch pipeline.
module Role (config, Tools) where

import ExoMonad.Guest.Records.Ping (PingTools(..), pingToolsHandler)
import ExoMonad.Guest.Tool.Mode (AsHandler)
import ExoMonad.Types (RoleConfig(..), HookConfig, defaultHooks)
import GHC.Generics (Generic)

-- | Test tools record - re-exports PingTools.
-- In a real role, you'd compose multiple tool records here.
type Tools = PingTools

config :: RoleConfig (Tools AsHandler)
config = RoleConfig
  { roleName = "test"
  , tools = pingToolsHandler
  , hooks = defaultHooks
  }
