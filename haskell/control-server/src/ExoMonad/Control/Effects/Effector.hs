{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.Effects.Effector
  ( runEffectorViaSsh
  , runEffectorIO
  ) where

import Control.Monad.Freer (Eff, Member, interpret, LastMember, sendM)
import Data.Aeson (eitherDecodeStrict)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import ExoMonad.Control.Effects.SshExec (SshExec, ExecRequest(..), ExecResult(..), execCommand)
import ExoMonad.Effects.Effector (Effector(..), GitStatusResult, GitDiffResult)

-- | Interpreter: uses SshExec to run 'effector' commands on a container
runEffectorViaSsh :: Member SshExec effs => Text -> Eff (Effector ': effs) a -> Eff effs a
runEffectorViaSsh container = interpret $ \case
  RunEffector cmd args -> do
    result <- execCommand $ ExecRequest
      { erContainer = Just container
      , erCommand = "effector"
      , erArgs = cmd : args
      , erWorkingDir = "."
      , erEnv = []
      , erTimeout = 60
      }
    -- Return combined stdout and stderr for now, similar to how it would be used
    pure $ exStdout result <> exStderr result

  EffectorGitStatus cwd -> do
    result <- execCommand $ ExecRequest
      { erContainer = Just container
      , erCommand = "effector"
      , erArgs = ["git", "status", "--cwd", T.pack cwd]
      , erWorkingDir = "."
      , erEnv = []
      , erTimeout = 30
      }
    case eitherDecodeStrict (T.encodeUtf8 (exStdout result)) of
      Left err -> error $ "Failed to parse effector git status JSON: " ++ err
      Right val -> pure val

  EffectorGitDiff cwd staged -> do
    let args = if staged 
               then ["git", "diff", "--staged", "--cwd", T.pack cwd]
               else ["git", "diff", "--cwd", T.pack cwd]
    result <- execCommand $ ExecRequest
      { erContainer = Just container
      , erCommand = "effector"
      , erArgs = args
      , erWorkingDir = "."
      , erEnv = []
      , erTimeout = 30
      }
    case eitherDecodeStrict (T.encodeUtf8 (exStdout result)) of
      Left err -> error $ "Failed to parse effector git diff JSON: " ++ err
      Right val -> pure val

  EffectorGitLsFiles cwd args -> do
    result <- execCommand $ ExecRequest
      { erContainer = Just container
      , erCommand = "effector"
      , erArgs = ["git", "ls-files", "--cwd", T.pack cwd] ++ args
      , erWorkingDir = "."
      , erEnv = []
      , erTimeout = 30
      }
    case eitherDecodeStrict (T.encodeUtf8 (exStdout result)) of
      Left err -> error $ "Failed to parse effector git ls-files JSON: " ++ err
      Right val -> pure val

-- | Interpreter: runs 'effector' binary directly via IO
runEffectorIO :: LastMember IO effs => Eff (Effector ': effs) a -> Eff effs a
runEffectorIO = interpret $ \case
  RunEffector cmd args -> do
    (code, stdout, stderr) <- sendM $ readProcessWithExitCode "effector" (T.unpack cmd : map T.unpack args) ""
    -- We ignore exit code for now because Effector results are often parsed from JSON in stdout even on failure
    pure $ T.pack stdout <> T.pack stderr

  EffectorGitStatus cwd -> do
    (exitCode, stdout, stderr) <- sendM $ readProcessWithExitCode "effector" ["git", "status", "--cwd", cwd] ""
    case exitCode of
      ExitSuccess -> case eitherDecodeStrict (T.encodeUtf8 (T.pack stdout)) of
        Left err -> error $ "Failed to parse effector git status JSON: " ++ err
        Right val -> pure val
      ExitFailure _ -> error $ "effector git status failed: " ++ stderr

  EffectorGitDiff cwd staged -> do
    let args = if staged 
               then ["git", "diff", "--staged", "--cwd", cwd]
               else ["git", "diff", "--cwd", cwd]
    (exitCode, stdout, stderr) <- sendM $ readProcessWithExitCode "effector" args ""
    case exitCode of
      ExitSuccess -> case eitherDecodeStrict (T.encodeUtf8 (T.pack stdout)) of
        Left err -> error $ "Failed to parse effector git diff JSON: " ++ err
        Right val -> pure val
      ExitFailure _ -> error $ "effector git diff failed: " ++ stderr

  EffectorGitLsFiles cwd args -> do
    let cmdArgs = ["git", "ls-files", "--cwd", cwd] ++ map T.unpack args
    (exitCode, stdout, stderr) <- sendM $ readProcessWithExitCode "effector" cmdArgs ""
    case exitCode of
      ExitSuccess -> case eitherDecodeStrict (T.encodeUtf8 (T.pack stdout)) of
        Left err -> error $ "Failed to parse effector git ls-files JSON: " ++ err
        Right val -> pure val
      ExitFailure _ -> error $ "effector git ls-files failed: " ++ stderr