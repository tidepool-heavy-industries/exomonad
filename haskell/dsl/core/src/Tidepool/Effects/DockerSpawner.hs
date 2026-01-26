{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Tidepool.Effects.DockerSpawner
  ( DockerSpawner(..)
  , ContainerId(..)
  , ContainerStatus(..)
  , SpawnConfig(..)
  , DockerError(..)
  , ExecResult(..)
  , spawnContainer
  , stopContainer
  , getContainerStatus
  , execContainer
  ) where

import Data.Text (Text)
import Control.Monad.Freer (Member, Eff, send)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

newtype ContainerId = ContainerId { unContainerId :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ContainerStatus
  = Running
  | Stopped
  | NotFound
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SpawnConfig = SpawnConfig
  { scIssueId :: Text
  , scWorktreePath :: FilePath
  , scBackend :: Text  -- "claude" | "gemini"
  , scUid :: Maybe Int
  , scGid :: Maybe Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ExecResult = ExecResult
  { erExitCode :: Maybe Int
  , erStdout :: Text
  , erStderr :: Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

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
