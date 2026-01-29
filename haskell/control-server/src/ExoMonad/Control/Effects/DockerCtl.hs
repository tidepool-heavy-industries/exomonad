{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ExoMonad.Control.Effects.DockerCtl
  ( runDockerCtl
  ) where

import Control.Monad.Freer (Eff, interpret, LastMember, sendM)
import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, defaultTimeLocale, formatTime)

import ExoMonad.Effects.DockerSpawner
import ExoMonad.Control.Logging (Logger)
import ExoMonad.Control.Subprocess (runSubprocessJSON, runSubprocess, SubprocessResult(..), SubprocessError(..))
import ExoMonad.Control.Protocol (AgentStatus(..))

runDockerCtl
  :: LastMember IO es
  => Logger
  -> FilePath -- ^ Path to docker-ctl binary
  -> TVar [AgentStatus]
  -> Eff (DockerSpawner ': es) a
  -> Eff es a
runDockerCtl logger binPath agentStore = interpret $ \case
  SpawnContainer cfg -> sendM $ doSpawn logger binPath agentStore cfg
  StopContainer cid -> sendM $ doStop logger binPath agentStore cid
  GetContainerStatus cid -> sendM $ doStatus logger binPath cid
  ExecContainer cid cmd mWorkdir mUser -> sendM $ doExec logger binPath cid cmd mWorkdir mUser

doSpawn :: Logger -> FilePath -> TVar [AgentStatus] -> SpawnConfig -> IO (Either DockerError ContainerId)
doSpawn logger binPath agentStore cfg = do
  let envArgs = concatMap (\(k, v) -> ["-e", T.unpack k <> "=" <> T.unpack v]) (cfg.scEnv)
      args = [ "spawn"
             , "--issue-id", T.unpack (cfg.scIssueId)
             , "--worktree-path", cfg.scWorktreePath
             , "--backend", T.unpack (cfg.scBackend)
             ] ++ maybe [] (\u -> ["--uid", show u]) (cfg.scUid)
               ++ maybe [] (\g -> ["--gid", show g]) (cfg.scGid)
               ++ envArgs
               ++ maybe [] (\c -> "--" : map T.unpack c) (cfg.scCmd)

  runSubprocessJSON logger "[DockerCtl]" binPath args >>= \case
    Right (ContainerId cid) -> do
      now <- getCurrentTime
      let timestamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
      let newAgent = AgentStatus
            { asId = cfg.scIssueId
            , asContainerId = cid
            , asIssueNumber = Nothing -- Simplify for now
            , asStatus = "running"
            , asStartedAt = timestamp
            , asLastActivity = Just timestamp
            , asLastAction = Just "Spawned"
            , asBlocker = Nothing
            }
      atomically $ modifyTVar' agentStore (newAgent :)
      pure $ Right (ContainerId cid)
    Left err -> pure $ Left $ DockerApiError 200 err.stderr

doStop :: Logger -> FilePath -> TVar [AgentStatus] -> ContainerId -> IO (Either DockerError ())
doStop logger binPath agentStore (ContainerId cid) = do
  let args = ["stop", T.unpack cid]
  runSubprocess logger "[DockerCtl]" binPath args >>= \case
    SubprocessSuccess _ _ -> do
      atomically $ modifyTVar' agentStore (filter (\a -> a.asContainerId /= cid))
      pure $ Right ()
    SubprocessFailure err -> pure $ Left $ DockerConnectionError err.stderr

doStatus :: Logger -> FilePath -> ContainerId -> IO (Either DockerError ContainerStatus)
doStatus logger binPath (ContainerId cid) = do
  let args = ["status", T.unpack cid]
  runSubprocessJSON logger "[DockerCtl]" binPath args >>= \case
    Right res -> pure $ Right res
    Left err -> pure $ Left $ DockerApiError 200 err.stderr

doExec :: Logger -> FilePath -> ContainerId -> [Text] -> Maybe FilePath -> Maybe Text -> IO (Either DockerError ExecResult)
doExec logger binPath (ContainerId cid) cmd mWorkdir mUser = do
  let args = ["exec", T.unpack cid]
           ++ maybe [] (\w -> ["--workdir", w]) mWorkdir
           ++ maybe [] (\u -> ["--user", T.unpack u]) mUser
           ++ ["--"] ++ map T.unpack cmd
  
  runSubprocessJSON logger "[DockerCtl]" binPath args >>= \case
    Right res -> pure $ Right res
    Left err -> pure $ Left $ DockerExecError err.stderr