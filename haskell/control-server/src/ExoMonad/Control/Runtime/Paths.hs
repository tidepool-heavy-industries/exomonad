{-# LANGUAGE OverloadedStrings #-}

-- | Canonical path logic for ExoMonad runtime orchestration.
module ExoMonad.Control.Runtime.Paths
  ( -- * Socket Directories
    socketDirectoryFor,
    controlSocketName,
    tuiSocketName,

    -- * Full Socket Paths
    controlSocketPath,
    tuiSocketPath,

    -- * Binary Paths
    runtimeBinDir,
    controlServerBin,
    exomonadAgentBin,
    tuiSidebarBin,
    dockerCtlBin,

    -- * Docker Spawner Paths
    dockerBinDir,
    dockerWorktreesPath,
    dockerSocketsPath,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import System.FilePath ((</>))

-- | Base directory for transient sockets.
-- Uses /tmp to avoid SUN_LEN path limits (~104 bytes) on macOS.
socketDirectoryFor :: Text -> FilePath
socketDirectoryFor shortId = "/tmp/exomonad-" <> T.unpack shortId

-- | Default name for the main control socket.
controlSocketName :: FilePath
controlSocketName = "control.sock"

-- | Default name for the TUI sidebar socket.
tuiSocketName :: FilePath
tuiSocketName = "tui.sock"

-- | Full path to the control socket.
controlSocketPath :: FilePath -> FilePath
controlSocketPath dir = dir </> controlSocketName

-- | Full path to the TUI socket.
tuiSocketPath :: FilePath -> FilePath
tuiSocketPath dir = dir </> tuiSocketName

-- | Canonical bin directory within a project.
runtimeBinDir :: FilePath -> FilePath
runtimeBinDir projectRoot = projectRoot </> "runtime" </> "bin"

-- | Path to the control server binary.
controlServerBin :: FilePath -> FilePath
controlServerBin binDir = binDir </> "exomonad-control-server"

-- | Path to the exomonad binary.
exomonadAgentBin :: FilePath -> FilePath
exomonadAgentBin binDir = binDir </> "exomonad"

-- | Path to the tui-sidebar binary.
tuiSidebarBin :: FilePath -> FilePath
tuiSidebarBin binDir = binDir </> "tui-sidebar"

-- | Path to the docker-ctl binary.
dockerCtlBin :: FilePath -> FilePath
dockerCtlBin binDir = binDir </> "docker-ctl"

-- | Directory for binaries used by Docker Spawner.
dockerBinDir :: IO FilePath
dockerBinDir = fromMaybe "/usr/local/bin" <$> lookupEnv "EXOMONAD_BIN_DIR"

-- | Base path for worktrees managed by Docker Spawner.
dockerWorktreesPath :: IO FilePath
dockerWorktreesPath = fromMaybe "/worktrees" <$> lookupEnv "EXOMONAD_WORKTREES_PATH"

-- | Base path for sockets managed by Docker Spawner.
dockerSocketsPath :: IO FilePath
dockerSocketsPath = fromMaybe "/sockets" <$> lookupEnv "EXOMONAD_SOCKETS_PATH"
