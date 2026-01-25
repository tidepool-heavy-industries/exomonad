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
  , spawnContainer
  , stopContainer
  , getContainerStatus
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
  { scBeadId :: Text
  , scWorktreePath :: FilePath
  , scBackend :: Text  -- "claude" | "gemini"
  , scUid :: Maybe Int
  , scGid :: Maybe Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DockerError
  = DockerConnectionError Text
  | DockerApiError Int Text
  | ContainerNotFound ContainerId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DockerSpawner r where
  SpawnContainer :: SpawnConfig -> DockerSpawner (Either DockerError ContainerId)
  StopContainer :: ContainerId -> DockerSpawner (Either DockerError ())
  GetContainerStatus :: ContainerId -> DockerSpawner (Either DockerError ContainerStatus)

-- Smart constructors
spawnContainer :: Member DockerSpawner es => SpawnConfig -> Eff es (Either DockerError ContainerId)
spawnContainer = send . SpawnContainer

stopContainer :: Member DockerSpawner es => ContainerId -> Eff es (Either DockerError ())
stopContainer = send . StopContainer

getContainerStatus :: Member DockerSpawner es => ContainerId -> Eff es (Either DockerError ContainerStatus)
getContainerStatus = send . GetContainerStatus
