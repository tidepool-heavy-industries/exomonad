{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module ExoMonad.Effects.DockerSpawner
  ( DockerSpawner(..)
  , ContainerId(..)
  , ContainerStatus(..)
  , SpawnConfig(..)
  , DockerError(..)
  , ExecResult(..)
  , SpawnResponse(..)
  , spawnContainer
  , stopContainer
  , getContainerStatus
  , execContainer
  ) where

import Data.Text (Text)
import Control.Monad.Freer (Member, Eff, send)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), genericParseJSON, genericToJSON, withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Aeson.Casing (aesonPrefix, snakeCase)

-- | Container ID extracted from spawn response
newtype ContainerId = ContainerId { unContainerId :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Response from docker-ctl spawn command
-- JSON: {"container_id": "...", "hostname": "..."}
data SpawnResponse = SpawnResponse
  { srContainerId :: Text
  , srHostname :: Text
  } deriving stock (Show, Eq, Generic)

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
      _ -> pure Stopped  -- Any other Docker status (exited, paused, etc.)

data SpawnConfig = SpawnConfig
  { scIssueId :: Text
  , scWorktreePath :: FilePath
  , scBackend :: Text  -- "claude" | "gemini"
  , scUid :: Maybe Int
  , scGid :: Maybe Int
  , scEnv :: [(Text, Text)]  -- Environment variables to pass to container
  , scCmd :: Maybe [Text] -- ^ Optional command override
  } deriving stock (Show, Eq, Generic)

instance ToJSON SpawnConfig where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON SpawnConfig where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Result of command execution
-- Matches docker-ctl exec output: {"exit_code": ..., "stdout": ..., "stderr": ...}
data ExecResult = ExecResult
  { erExitCode :: Maybe Int
  , erStdout :: Text
  , erStderr :: Text
  } deriving stock (Show, Eq, Generic)

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

data DockerSpawner r where
  SpawnContainer :: SpawnConfig -> DockerSpawner (Either DockerError ContainerId)
  StopContainer :: ContainerId -> DockerSpawner (Either DockerError ())
  GetContainerStatus :: ContainerId -> DockerSpawner (Either DockerError ContainerStatus)
  ExecContainer :: ContainerId -> [Text] -> Maybe FilePath -> Maybe Text -> DockerSpawner (Either DockerError ExecResult)

-- Smart constructors
spawnContainer :: Member DockerSpawner es => SpawnConfig -> Eff es (Either DockerError ContainerId)
spawnContainer = send . SpawnContainer

stopContainer :: Member DockerSpawner es => ContainerId -> Eff es (Either DockerError ())
stopContainer = send . StopContainer

getContainerStatus :: Member DockerSpawner es => ContainerId -> Eff es (Either DockerError ContainerStatus)
getContainerStatus = send . GetContainerStatus

execContainer :: Member DockerSpawner es => ContainerId -> [Text] -> Maybe FilePath -> Maybe Text -> Eff es (Either DockerError ExecResult)
execContainer cid cmd mWorkdir mUser = send $ ExecContainer cid cmd mWorkdir mUser
