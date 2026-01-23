{-# LANGUAGE OverloadedStrings #-}
-- | Canonical path logic for Tidepool runtime orchestration.
module Tidepool.Control.Runtime.Paths
  ( -- * Socket Directories
    socketDirectoryFor
  , controlSocketName
  , tuiSocketName
  , processComposeSocketName
    -- * Full Socket Paths
  , controlSocketPath
  , tuiSocketPath
  , processComposeSocketPath
    -- * Binary Paths
  , runtimeBinDir
  , controlServerBin
  , mantleAgentBin
  , tuiSidebarBin
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))

-- | Base directory for transient sockets. 
-- Uses /tmp to avoid SUN_LEN path limits (~104 bytes) on macOS.
socketDirectoryFor :: Text -> FilePath
socketDirectoryFor shortId = "/tmp/tidepool-" <> T.unpack shortId

-- | Default name for the main control socket.
controlSocketName :: FilePath
controlSocketName = "control.sock"

-- | Default name for the TUI sidebar socket.
tuiSocketName :: FilePath
tuiSocketName = "tui.sock"

-- | Default name for the process-compose API socket.
processComposeSocketName :: FilePath
processComposeSocketName = "process-compose.sock"

-- | Full path to the control socket.
controlSocketPath :: FilePath -> FilePath
controlSocketPath dir = dir </> controlSocketName

-- | Full path to the TUI socket.
tuiSocketPath :: FilePath -> FilePath
tuiSocketPath dir = dir </> tuiSocketName

-- | Full path to the process-compose socket.
processComposeSocketPath :: FilePath -> FilePath
processComposeSocketPath dir = dir </> processComposeSocketName

-- | Canonical bin directory within a hangar.
runtimeBinDir :: FilePath -> FilePath
runtimeBinDir hangarRoot = hangarRoot </> "runtime" </> "bin"

-- | Path to the control server binary.
controlServerBin :: FilePath -> FilePath
controlServerBin binDir = binDir </> "tidepool-control-server"

-- | Path to the mantle-agent binary.
mantleAgentBin :: FilePath -> FilePath
mantleAgentBin binDir = binDir </> "mantle-agent"

-- | Path to the tui-sidebar binary.
tuiSidebarBin :: FilePath -> FilePath
tuiSidebarBin binDir = binDir </> "tui-sidebar"
