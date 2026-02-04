{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Role (config) where

import ExoMonad

-- User defines their tool record by COMPOSING framework records
data Tools mode = Tools
  { git :: GitTools mode        -- Nested record, NOT individual tools!
  , github :: GitHubTools mode  -- Nested record
  , fs :: FileTools mode        -- Nested record
  } deriving Generic

config :: RoleConfig Tools
config = RoleConfig
  { roleName = "dev"
  , tools = Tools
      { git = gitTools          -- Use pre-built framework value
      , github = githubTools    -- Use pre-built framework value
      , fs = fileTools          -- Use pre-built framework value
      }
  , hooks = defaultHooks
  }