{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module ExoMonad.Effects.DockerSpawner
  ( DockerSpawner (..),
    ContainerId (..),
    ContainerStatus (..),
    SpawnConfig (..),
    DockerError (..),
    ExecResult (..),
    SpawnResponse (..),
    spawnContainer,
    stopContainer,
    getContainerStatus,
    execContainer,
  )
where

import Polysemy (Sem, Member, makeSem)
import Data.Kind (Type)
import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:))
import Data.Aeson.Casing (aesonPrefix, snakeCase)

-- | Container ID extracted from spawn response
newtype ContainerId = ContainerId {unContainerId :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Response from docker-ctl spawn command
-- JSON: {"container_id": "...", "hostname": "..."}
data SpawnResponse = SpawnResponse
  { srContainerId :: Text,
    srHostname :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SpawnResponse where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON SpawnResponse where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Parse ContainerId from spawn response JSON
instance FromJSON ContainerId where
  parseJSON = withObject "SpawnResponse" $ \o -> do
    cid <- o .: "container_id"
    pure $ ContainerId cid

data ContainerStatus
  = Running
  | Stopped
  | NotFound
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Parse ContainerStatus from docker-ctl status response
-- JSON: {"status": "running"} or {"status": "not_found"}
instance FromJSON ContainerStatus where
  parseJSON = withObject "StatusResponse" $ \o -> do
    status <- o .: "status" :: Parser Text
    case status of
      "running" -> pure Running
      "not_found" -> pure NotFound
      _ -> pure Stopped -- Any other Docker status (exited, paused, etc.)

data SpawnConfig = SpawnConfig
  { scIssueId :: Text,
    scWorktreePath :: FilePath,
    scBackend :: Text,
    scUid :: Maybe Int,
    scGid :: Maybe Int,
    scEnv :: [(Text, Text)], -- Environment variables to pass to container

    -- | Optional command override
    scCmd :: Maybe [Text]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SpawnConfig where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON SpawnConfig where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Result of command execution
-- Matches docker-ctl exec output: {"exit_code": ..., "stdout": ..., "stderr": ...}
data ExecResult = ExecResult
  { erExitCode :: Maybe Int,
    erStdout :: Text,
    erStderr :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ExecResult where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON ExecResult where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data DockerError
  = DockerConnectionError Text
  | DockerApiError Int Text
  | ContainerNotFound ContainerId
  | DockerExecError Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DockerSpawner m a where
  SpawnContainer :: SpawnConfig -> DockerSpawner m (Either DockerError ContainerId)
  StopContainer :: ContainerId -> DockerSpawner m (Either DockerError ())
  GetContainerStatus :: ContainerId -> DockerSpawner m (Either DockerError ContainerStatus)
  ExecContainer :: ContainerId -> [Text] -> Maybe FilePath -> Maybe Text -> DockerSpawner m (Either DockerError ExecResult)

makeSem ''DockerSpawner

