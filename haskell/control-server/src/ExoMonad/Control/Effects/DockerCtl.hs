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
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import ExoMonad.Effects.DockerSpawner

runDockerCtl
  :: LastMember IO es
  => FilePath -- ^ Path to docker-ctl binary
  -> Eff (DockerSpawner ': es) a
  -> Eff es a
runDockerCtl binPath = interpret $ \case
  SpawnContainer cfg -> sendM $ doSpawn binPath cfg
  StopContainer cid -> sendM $ doStop binPath cid
  GetContainerStatus cid -> sendM $ doStatus binPath cid
  ExecContainer cid cmd mWorkdir mUser -> sendM $ doExec binPath cid cmd mWorkdir mUser

doSpawn :: FilePath -> SpawnConfig -> IO (Either DockerError ContainerId)
doSpawn binPath cfg = do
  let envArgs = concatMap (\(k, v) -> ["-e", T.unpack k <> "=" <> T.unpack v]) (cfg.scEnv)
      args = [ "spawn"
             , "--issue-id", T.unpack (cfg.scIssueId)
             , "--worktree-path", cfg.scWorktreePath
             , "--backend", T.unpack (cfg.scBackend)
             ] ++ maybe [] (\u -> ["--uid", show u]) (cfg.scUid)
               ++ maybe [] (\g -> ["--gid", show g]) (cfg.scGid)
               ++ envArgs

  (code, stdout, stderr) <- readProcessWithExitCode binPath args ""
  case code of
    ExitSuccess -> case eitherDecode (BL.fromStrict $ TE.encodeUtf8 $ T.pack stdout) of
      Right (res :: ContainerId) -> pure $ Right res
      Left err -> pure $ Left $ DockerApiError 200 (T.pack err)
    ExitFailure _ -> pure $ Left $ DockerConnectionError (T.pack stderr)

doStop :: FilePath -> ContainerId -> IO (Either DockerError ())
doStop binPath (ContainerId cid) = do
  let args = ["stop", T.unpack cid]
  (code, _stdout, stderr) <- readProcessWithExitCode binPath args ""
  case code of
    ExitSuccess -> pure $ Right ()
    ExitFailure _ -> pure $ Left $ DockerConnectionError (T.pack stderr)

doStatus :: FilePath -> ContainerId -> IO (Either DockerError ContainerStatus)
doStatus binPath (ContainerId cid) = do
  let args = ["status", T.unpack cid]
  (code, stdout, stderr) <- readProcessWithExitCode binPath args ""
  case code of
    ExitSuccess -> case eitherDecode (BL.fromStrict $ TE.encodeUtf8 $ T.pack stdout) of
      Right (res :: ContainerStatus) -> pure $ Right res
      Left err -> pure $ Left $ DockerApiError 200 (T.pack err)
    ExitFailure _ -> pure $ Left $ DockerConnectionError (T.pack stderr)

doExec :: FilePath -> ContainerId -> [Text] -> Maybe FilePath -> Maybe Text -> IO (Either DockerError ExecResult)
doExec binPath (ContainerId cid) cmd mWorkdir mUser = do
  let args = ["exec", T.unpack cid]
           ++ maybe [] (\w -> ["--workdir", w]) mWorkdir
           ++ maybe [] (\u -> ["--user", T.unpack u]) mUser
           ++ ["--"] ++ map T.unpack cmd
  
  (code, stdout, stderr) <- readProcessWithExitCode binPath args ""
  case code of
    ExitSuccess -> case eitherDecode (BL.fromStrict $ TE.encodeUtf8 $ T.pack stdout) of
      Right (res :: ExecResult) -> pure $ Right res
      Left err -> pure $ Left $ DockerApiError 200 (T.pack err)
    ExitFailure _ -> pure $ Left $ DockerExecError (T.pack stderr)
