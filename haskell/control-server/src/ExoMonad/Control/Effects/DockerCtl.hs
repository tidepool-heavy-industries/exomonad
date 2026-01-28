{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExoMonad.Control.Effects.DockerCtl
  ( runDockerCtl
  ) where

import Control.Monad.Freer (Eff, interpret, LastMember, sendM)
import Data.Text (Text)
import qualified Data.Text as T

import ExoMonad.Effects.DockerSpawner
import ExoMonad.Control.Logging (Logger)
import ExoMonad.Control.Subprocess (runSubprocessJSON, runSubprocess, SubprocessResult(..), SubprocessError(..))

runDockerCtl
  :: LastMember IO es
  => Logger
  -> FilePath -- ^ Path to docker-ctl binary
  -> Eff (DockerSpawner ': es) a
  -> Eff es a
runDockerCtl logger binPath = interpret $ \case
  SpawnContainer cfg -> sendM $ doSpawn logger binPath cfg
  StopContainer cid -> sendM $ doStop logger binPath cid
  GetContainerStatus cid -> sendM $ doStatus logger binPath cid
  ExecContainer cid cmd mWorkdir mUser -> sendM $ doExec logger binPath cid cmd mWorkdir mUser

doSpawn :: Logger -> FilePath -> SpawnConfig -> IO (Either DockerError ContainerId)
doSpawn logger binPath cfg = do
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
    Right res -> pure $ Right res
    Left err -> pure $ Left $ DockerApiError 200 err.stderr

doStop :: Logger -> FilePath -> ContainerId -> IO (Either DockerError ())
doStop logger binPath (ContainerId cid) = do
  let args = ["stop", T.unpack cid]
  runSubprocess logger "[DockerCtl]" binPath args >>= \case
    SubprocessSuccess _ _ -> pure $ Right ()
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
