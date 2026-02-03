{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Role (config) where

import ExoMonad

data Tools mode = Tools
  { git :: GitTools mode,
    github :: GitHubTools mode,
    fs :: FileTools mode
  }
  deriving (Generic)

config :: RoleConfig (Tools AsHandler)
config =
  RoleConfig
    { roleName = "dev",
      tools =
        Tools
          { git = gitTools,
            github = githubTools,
            fs = fileTools
          },
      hooks = defaultHooks
    }
