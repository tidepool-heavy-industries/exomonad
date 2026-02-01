{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Zellij effect for terminal multiplexer operations.
--
-- Effect type only - interpreter lives in exomonad-zellij-interpreter.
-- Enables graphs to interact with Zellij sessions and tabs.
--
-- = Example Usage
--
-- @
-- import ExoMonad.Effects.Zellij
--
-- spawnHandler :: Member Zellij effs => Eff effs (Either ZellijError TabId)
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

import Control.Monad.Freer (Eff, Member, send)

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
data Zellij r where
  -- | Check if we're running inside a Zellij session.
  -- Returns the $ZELLIJ environment variable value if set.
  CheckZellijEnv ::
    Zellij (Maybe Text)
  -- | Create a new tab with the specified configuration.
  -- Returns the TabId on success, or an error if tab creation failed.
  NewTab ::
    TabConfig ->
    Zellij (Either ZellijError TabId)
  -- | Switch focus to a tab by name.
  -- Returns success or error if tab doesn't exist or command failed.
  GoToTab ::
    TabId ->
    Zellij (Either ZellijError ())
  -- | Generate a Zellij layout file via zellij-gen.
  -- Returns the path to the generated layout file on success.
  -- This bakes commands into the layout as literals (env vars don't propagate to panes).
  GenerateLayout ::
    LayoutSpec ->
    Zellij (Either ZellijError FilePath)

-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if we're running inside a Zellij session.
--
-- Returns 'Just' with the $ZELLIJ value if inside Zellij,
-- 'Nothing' if not running in Zellij.
--
-- @
-- mSession <- checkZellijEnv
-- case mSession of
--   Nothing -> error "Must run inside Zellij"
--   Just _ -> proceed
-- @
checkZellijEnv ::
  (Member Zellij effs) =>
  Eff effs (Maybe Text)
checkZellijEnv = send CheckZellijEnv

-- | Create a new Zellij tab with the specified configuration.
--
-- The tab will be created in the current Zellij session.
-- Requires running inside Zellij (check with 'checkZellijEnv' first).
--
-- @
-- result <- newTab TabConfig
--   { tcName = "worker"
--   , tcLayout = ".zellij/worktree.kdl"
--   , tcCwd = "/path/to/worktree"
--   , tcEnv = [("SUBAGENT_CMD", "claude")]
--   }
-- case result of
--   Right tabId -> -- tab created successfully
--   Left err -> handleError err
-- @
newTab ::
  (Member Zellij effs) =>
  TabConfig ->
  Eff effs (Either ZellijError TabId)
newTab = send . NewTab

-- | Switch focus to a tab by name.
--
-- The tab must already exist in the current Zellij session.
-- Requires running inside Zellij (check with 'checkZellijEnv' first).
--
-- @
-- result <- goToTab (TabId "5dj")
-- case result of
--   Right () -> -- focus switched successfully
--   Left err -> handleError err
-- @
goToTab ::
  (Member Zellij effs) =>
  TabId ->
  Eff effs (Either ZellijError ())
goToTab = send . GoToTab

-- | Generate a Zellij layout file via zellij-gen.
--
-- This generates layouts with commands baked in as literals,
-- which is necessary because environment variables don't propagate
-- to pane processes spawned from layouts.
--
-- @
-- result <- generateLayout (SubagentLayout "346" "exomonad-agent-346")
-- case result of
--   Right layoutPath -> -- use layoutPath in newTab
--   Left err -> handleError err
-- @
generateLayout ::
  (Member Zellij effs) =>
  LayoutSpec ->
  Eff effs (Either ZellijError FilePath)
generateLayout = send . GenerateLayout
