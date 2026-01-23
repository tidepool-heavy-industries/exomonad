{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | Type-safe configuration for process-compose.
module Tidepool.Control.Runtime.ProcessCompose
  ( ProcessComposeConfig(..)
  , ProcessDef(..)
  , ReadinessProbe(..)
  , ProbeExec(..)
  , Availability(..)
  , Shutdown(..)
  , generateSubagentConfig
  ) where

import Data.Aeson (ToJSON(..), Value(Null), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Top-level process-compose.yaml configuration.
data ProcessComposeConfig = ProcessComposeConfig
  { version   :: Text
  , processes :: Map Text ProcessDef
  } deriving (Show, Eq, Generic)

instance ToJSON ProcessComposeConfig where
  toJSON cfg = object
    [ "version"   .= cfg.version
    , "processes" .= cfg.processes
    ]

-- | Definition of a single process.
data ProcessDef = ProcessDef
  { command         :: Text
  , working_dir     :: Maybe Text
  , environment     :: Maybe [Text]
  , readiness_probe :: Maybe ReadinessProbe
  , availability    :: Maybe Availability
  , shutdown        :: Maybe Shutdown
  } deriving (Show, Eq, Generic)

instance ToJSON ProcessDef where
  toJSON p = object $ filter ((/= Null) . snd) 
    [ "command"         .= p.command
    , "working_dir"     .= p.working_dir
    , "environment"     .= p.environment
    , "readiness_probe" .= p.readiness_probe
    , "availability"    .= p.availability
    , "shutdown"        .= p.shutdown
    ]

-- | Readiness probe configuration.
data ReadinessProbe = ReadinessProbe
  { exec                  :: ProbeExec
  , initial_delay_seconds :: Int
  , period_seconds        :: Int
  , failure_threshold     :: Int
  } deriving (Show, Eq, Generic, ToJSON)

-- | Executive part of a probe.
data ProbeExec = ProbeExec
  { command :: Text
  } deriving (Show, Eq, Generic, ToJSON)

-- | Availability/restart policy.
data Availability = Availability
  { restart      :: Text -- e.g. "on_failure"
  , max_restarts :: Int
  } deriving (Show, Eq, Generic, ToJSON)

-- | Shutdown configuration.
data Shutdown = Shutdown
  { command         :: Text
  , timeout_seconds :: Int
  } deriving (Show, Eq, Generic, ToJSON)

-- | Generate the canonical subagent process-compose configuration.
generateSubagentConfig :: FilePath -> FilePath -> FilePath -> FilePath -> ProcessComposeConfig
generateSubagentConfig binPath socketDir controlSocket tuiSocket = ProcessComposeConfig
  { version = "0.5"
  , processes = Map.fromList [("control-server", controlServer)]
  }
  where
    controlServer = ProcessDef
      { command = T.unlines
          [ "# Create socket directory and clean up stale sockets"
          , "mkdir -p \"" <> T.pack socketDir <> "\""
          , "rm -f \"" <> T.pack controlSocket <> "\" \"" <> T.pack tuiSocket <> "\""
          , "\"" <> T.pack binPath <> "\" --no-tui"
          ]
      , working_dir = Just "."
      , environment = Just
          [ "TIDEPOOL_BIN_DIR=" -- Dummy to satisfy old template expectations if any
          , "TIDEPOOL_SOCKET_DIR=" <> T.pack socketDir
          , "TIDEPOOL_CONTROL_SOCKET=" <> T.pack controlSocket
          , "TIDEPOOL_TUI_SOCKET=" <> T.pack tuiSocket
          ]
      , readiness_probe = Just ReadinessProbe
          { exec = ProbeExec { command = "test -S " <> T.pack controlSocket }
          , initial_delay_seconds = 2
          , period_seconds = 3
          , failure_threshold = 10
          }
      , availability = Just Availability
          { restart = "on_failure"
          , max_restarts = 5
          }
      , shutdown = Just Shutdown
          { command = "rm -f " <> T.pack controlSocket <> " " <> T.pack tuiSocket
          , timeout_seconds = 5
          }
      }
