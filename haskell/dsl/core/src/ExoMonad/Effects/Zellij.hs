{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | Zellij effect for terminal multiplexer operations.
--
-- Effect type only - interpreter lives in exomonad-zellij-interpreter.
-- Enables agents to interact with Zellij sessions and tabs.
--
-- = Example Usage
--
-- @
-- import ExoMonad.Effects.Zellij
--
-- spawnHandler :: Member Zellij r => Sem r (Either ZellijError TabId)
-- spawnHandler = do
--   mSession <- checkZellijEnv
--   case mSession of
--     Nothing -> pure (Left ZellijNotRunning)
--     Just _ -> do
--       let cfg = TabConfig
--             { tcName = "worker"
--             , tcLayout = ".zellij/worktree.kdl"
--             , tcCwd = "/path/to/worktree"
--             , tcEnv = [("SUBAGENT_CMD", "claude")]
--             , tcCommand = Nothing
--             }
--       newTab cfg
-- @
--
-- = Design Notes
--
-- The Zellij effect requires running inside a Zellij session.
-- Use 'checkZellijEnv' to verify this before attempting tab operations.
module ExoMonad.Effects.Zellij
  ( -- * Effect
    Zellij (..),

    -- * Smart Constructors
    checkZellijEnv,
    newTab,
    goToTab,
    generateLayout,

    -- * Configuration Types
    TabConfig (..),
    TabId (..),
    LayoutSpec (..),

    -- * Error Types
    ZellijError (..),
  )
where

import Data.Kind (Type)
import Polysemy (Member, Sem, makeSem)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Opaque identifier for a Zellij tab.
--
-- This is typically the tab name as specified in 'tcName'.
newtype TabId = TabId {unTabId :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString, ToJSON, FromJSON)

-- | Configuration for creating a new Zellij tab.
data TabConfig = TabConfig
  { -- | Tab name (displayed in Zellij tab bar)
    tcName :: Text,
    -- | Path to Zellij layout file (.kdl)
    tcLayout :: FilePath,
    -- | Working directory for the tab
    tcCwd :: FilePath,
    -- | Environment variables to set (e.g., SUBAGENT_CMD)
    tcEnv :: [(Text, Text)],
    -- | Optional command to run in the new tab (mutually exclusive with tcLayout in some interpreters)
    tcCommand :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TabConfig

instance FromJSON TabConfig

-- | Specification for generating a Zellij layout via zellij-gen.
data LayoutSpec
  = -- | Generate main layout (TL/PM/Infrastructure tabs)
    MainLayout
  | SubagentLayout
      { -- | Issue ID (e.g., "346")
        lsIssueId :: Text,
        -- | Container ID (e.g., "exomonad-agent-346")
        lsContainerId :: Text
      }
  -- \^ Generate subagent layout with docker attach command baked in
  deriving stock (Show, Eq, Generic)

instance ToJSON LayoutSpec

instance FromJSON LayoutSpec

-- ════════════════════════════════════════════════════════════════════════════
-- ERRORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Errors that can occur during Zellij operations.
data ZellijError
  = -- | Not running inside a Zellij session ($ZELLIJ not set)
    ZellijNotRunning
  | ZellijCommandFailed
      { -- | Zellij command that failed
        zceCommand :: Text,
        -- | Exit code from zellij
        zceExitCode :: Int,
        -- | Stderr output
        zceStderr :: Text
      }
  | -- \^ Zellij command returned non-zero exit code
    ZellijLayoutNotFound
      { -- | Layout file path that wasn't found
        zlnfPath :: FilePath
      }
  -- \^ Layout file does not exist
  deriving stock (Eq, Show, Generic)

instance ToJSON ZellijError

instance FromJSON ZellijError

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Zellij effect for terminal multiplexer operations.
--
-- Operations are designed to work with Zellij's IPC capabilities.
data Zellij m a where
  -- | Check if we're running inside a Zellij session.
  -- Returns the $ZELLIJ environment variable value if set.
  CheckZellijEnv ::
    Zellij m (Maybe Text)
  -- | Create a new tab with the specified configuration.
  -- Returns the TabId on success, or an error if tab creation failed.
  NewTab ::
    TabConfig ->
    Zellij m (Either ZellijError TabId)
  -- | Switch focus to a tab by name.
  -- Returns success or error if tab doesn't exist or command failed.
  GoToTab ::
    TabId ->
    Zellij m (Either ZellijError ())
  -- | Generate a Zellij layout file via zellij-gen.
  -- Returns the path to the generated layout file on success.
  -- This bakes commands into the layout as literals (env vars don't propagate to panes).
  GenerateLayout ::
    LayoutSpec ->
    Zellij m (Either ZellijError FilePath)

makeSem ''Zellij
