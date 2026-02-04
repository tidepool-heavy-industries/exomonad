{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Role (config, Tools) where

import ExoMonad

data Tools mode = Tools
  { agents :: AgentTools mode
  }
  deriving (Generic)

config :: RoleConfig (Tools AsHandler)
config =
  RoleConfig
    { roleName = "tl",
      tools =
        Tools
          { agents = agentTools
          },
      hooks = defaultHooks
    }
