{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | Context effect for "World View" information.
--
-- Unifies access to environment, workspace, and agent identity.
-- Replaces scattered Reader/Env/Worktree lookups.
module ExoMonad.Effect.Context
  ( -- * Effect
    Context (..),
    getWorktreeRoot,
    getUserConfig,
    getAgentId,

    -- * Config Types
    UserConfig (..),
    defaultUserConfig,
  )
where

import Polysemy (Sem, Member, makeSem)
import Data.Text (Text)
import ExoMonad.Path (Path, Abs, Dir)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- CONFIG TYPES
-- ══════════════════════════════════════════════════════════════

-- | Aggregated user configuration.
data UserConfig = UserConfig
  { -- | GitHub username (for PRs)
    ucGitHubUser :: Maybe Text,
    -- | Preferred editor (for opening files)
    ucEditor :: Text,
    -- | Terminal width (for formatting)
    ucTerminalWidth :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

defaultUserConfig :: UserConfig
defaultUserConfig =
  UserConfig
    { ucGitHubUser = Nothing,
      ucEditor = "vim",
      ucTerminalWidth = 80
    }

-- ══════════════════════════════════════════════════════════════
-- EFFECT DEFINITION
-- ══════════════════════════════════════════════════════════════

data Context m a where
  -- | Get the absolute path to the current worktree root.
  GetWorktreeRoot :: Context m (Path Abs Dir)
  -- | Get user configuration.
  GetUserConfig :: Context m UserConfig
  -- | Get the unique ID of the running agent.
  GetAgentId :: Context m Text

makeSem ''Context
