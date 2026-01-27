{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module ExoMonad.Control.ExoTools.SpawnCleanup
  ( SpawnCleanup(..)
  , Resource(..)
  , SpawnProgress(..)
  , acquireWorktree
  , releaseWorktree
  , acquireContainer
  , releaseContainer
  , runSpawnCleanup
  , emitProgress
  , cleanupAll
  ) where

import Control.Monad (forM_)
import Control.Monad.Freer (Eff, Member, send, reinterpret, type (~>))
import Control.Monad.Freer.State (State, get, put, runState)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import ExoMonad.Effects.DockerSpawner (ContainerId(..), DockerSpawner, stopContainer)
import ExoMonad.Effects.Worktree (WorktreePath(..), Worktree, deleteWorktree)
import ExoMonad.Effects.Zellij (TabId(..))
import ExoMonad.Effect.Log (Log, logInfo, logError)

data Resource
  = RWorktree WorktreePath
  | RContainer ContainerId
  deriving (Show, Eq, Generic)

data SpawnProgress
  = SpawnStarted Text Int        -- ^ issueId, totalSteps
  | WorktreeCreated Text FilePath
  | ContainerSpawned Text ContainerId
  | TabLaunched Text TabId
  | SpawnFailed Text Text        -- ^ issueId, reason
  | SpawnComplete Text           -- ^ issueId
  deriving (Show, Eq, Generic)

data SpawnCleanup r where
  AcquireWorktree :: WorktreePath -> SpawnCleanup ()
  ReleaseWorktree :: WorktreePath -> SpawnCleanup ()
  AcquireContainer :: ContainerId -> SpawnCleanup ()
  ReleaseContainer :: ContainerId -> SpawnCleanup ()
  EmitProgress :: SpawnProgress -> SpawnCleanup ()
  CleanupAll :: SpawnCleanup ()

acquireWorktree :: Member SpawnCleanup es => WorktreePath -> Eff es ()
acquireWorktree = send . AcquireWorktree

releaseWorktree :: Member SpawnCleanup es => WorktreePath -> Eff es ()
releaseWorktree = send . ReleaseWorktree

acquireContainer :: Member SpawnCleanup es => ContainerId -> Eff es ()
acquireContainer = send . AcquireContainer

releaseContainer :: Member SpawnCleanup es => ContainerId -> Eff es ()
releaseContainer = send . ReleaseContainer

emitProgress :: Member SpawnCleanup es => SpawnProgress -> Eff es ()
emitProgress = send . EmitProgress

cleanupAll :: Member SpawnCleanup es => Eff es ()
cleanupAll = send CleanupAll

runSpawnCleanup
  :: (Member DockerSpawner es, Member Worktree es, Member Log es)
  => Eff (SpawnCleanup ': es) a
  -> Eff es (a, [Resource])
runSpawnCleanup = runState [] . reinterpret handler
  where
    handler :: (Member DockerSpawner es, Member Worktree es, Member Log es) => SpawnCleanup ~> Eff (State [Resource] ': es)
    handler = \case
      AcquireWorktree wp -> do
        resources <- get @[Resource]
        put (RWorktree wp : resources)
      ReleaseWorktree wp -> do
        resources <- get @[Resource]
        put (filter (/= RWorktree wp) resources)
      AcquireContainer cid -> do
        resources <- get @[Resource]
        put (RContainer cid : resources)
      ReleaseContainer cid -> do
        resources <- get @[Resource]
        put (filter (/= RContainer cid) resources)
      EmitProgress p -> case p of
        SpawnStarted sid steps -> logInfo $ "[spawn:" <> sid <> "] Started (" <> T.pack (show steps) <> " steps)"
        WorktreeCreated sid path -> logInfo $ "[spawn:" <> sid <> "] Worktree created: " <> T.pack path
        ContainerSpawned sid cid -> logInfo $ "[spawn:" <> sid <> "] Container spawned: " <> cid.unContainerId
        TabLaunched sid tid -> logInfo $ "[spawn:" <> sid <> "] Tab launched: " <> tid.unTabId
        SpawnFailed sid reason -> logError $ "[spawn:" <> sid <> "] FAILED: " <> reason
        SpawnComplete sid -> logInfo $ "[spawn:" <> sid <> "] Complete"
      CleanupAll -> do
        resources <- get @[Resource]
        forM_ resources $ \case
          RContainer cid -> do
             _ <- stopContainer cid
             pure ()
          RWorktree wp -> do
             _ <- deleteWorktree wp
             pure ()
        put @[Resource] []
