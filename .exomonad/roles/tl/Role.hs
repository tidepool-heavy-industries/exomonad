{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Role where

import ExoMonad
import qualified Hooks

-- TL has more tool groups than dev
data Tools mode = Tools
  { git :: GitTools mode
  , github :: GitHubTools mode
  , fs :: FileTools mode
  , agents :: AgentTools mode   -- TL adds agent orchestration
  } deriving Generic

config :: RoleConfig (Tools AsHandler)
config = RoleConfig
  { roleName = "tl"
  , tools = Tools
      { git = gitTools
      , github = githubTools
      , fs = fileTools
      , agents = agentTools
      }
  , hooks = Hooks.tlHooks
  }