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

import ExoMonad.Control.Effects.SshExec (SshExec, ExecRequest(..), ExecResult(..), execCommand)
import ExoMonad.Effects.Effector (Effector(..), GitStatusResult, GitDiffResult)
import ExoMonad.Control.Logging (Logger)
import ExoMonad.Control.Subprocess (runSubprocess, runSubprocessJSON, SubprocessResult(..), SubprocessError(..))

-- | Interpreter: uses SshExec to run 'effector' commands on a container
runEffectorViaSsh :: Member SshExec effs => Text -> Eff (Effector ': effs) a -> Eff effs a
runEffectorViaSsh container = interpret $ \case
  RunEffector cmd args -> do
    result <- execCommand $ ExecRequest
      { container = Just container
      , command = "effector"
      , args = cmd : args
      , workingDir = "."
      , env = []
      , timeout = 60
      }
    -- Return combined stdout and stderr for now, similar to how it would be used
    pure $ result.stdout <> result.stderr

  EffectorGitStatus cwd -> do
    result <- execCommand $ ExecRequest
      { container = Just container
      , command = "effector"
      , args = ["git", "status", "--cwd", T.pack cwd]
      , workingDir = "."
      , env = []
      , timeout = 30
      }
    case eitherDecodeStrict (T.encodeUtf8 (result.stdout)) of
      Left err -> error $ "Failed to parse effector git status JSON: " ++ err
      Right val -> pure val

  EffectorGitDiff cwd staged -> do
    let args = if staged 
               then ["git", "diff", "--staged", "--cwd", T.pack cwd]
               else ["git", "diff", "--cwd", T.pack cwd]
    result <- execCommand $ ExecRequest
      { container = Just container
      , command = "effector"
      , args = args
      , workingDir = "."
      , env = []
      , timeout = 30
      }
    case eitherDecodeStrict (T.encodeUtf8 (result.stdout)) of
      Left err -> error $ "Failed to parse effector git diff JSON: " ++ err
      Right val -> pure val

  EffectorGitLsFiles cwd args -> do
    result <- execCommand $ ExecRequest
      { container = Just container
      , command = "effector"
      , args = ["git", "ls-files", "--cwd", T.pack cwd] ++ args
      , workingDir = "."
      , env = []
      , timeout = 30
      }
    case eitherDecodeStrict (T.encodeUtf8 (result.stdout)) of
      Left err -> error $ "Failed to parse effector git ls-files JSON: " ++ err
      Right val -> pure val

-- | Interpreter: runs 'effector' binary directly via IO
runEffectorIO :: LastMember IO effs => Logger -> Eff (Effector ': effs) a -> Eff effs a
runEffectorIO logger = interpret $ \case
  RunEffector cmd args -> do
    -- We ignore exit code for now because Effector results are often parsed from JSON in stdout even on failure
    -- or we just want the output.
    sendM (runSubprocess logger "[Effector]" "effector" (T.unpack cmd : map T.unpack args)) >>= \case
      SubprocessSuccess out err -> pure $ out <> err
      SubprocessFailure e -> pure $ e.stderr -- Return stderr on failure as well

  EffectorGitStatus cwd -> do
    sendM (runSubprocessJSON logger "[Effector]" "effector" ["git", "status", "--cwd", cwd]) >>= \case
      Right val -> pure val
      Left err -> error $ "effector git status failed: " ++ T.unpack err.stderr

  EffectorGitDiff cwd staged -> do
    let args = if staged 
               then ["git", "diff", "--staged", "--cwd", cwd]
               else ["git", "diff", "--cwd", cwd]
    sendM (runSubprocessJSON logger "[Effector]" "effector" args) >>= \case
      Right val -> pure val
      Left err -> error $ "effector git diff failed: " ++ T.unpack err.stderr

  EffectorGitLsFiles cwd args -> do
    let cmdArgs = ["git", "ls-files", "--cwd", cwd] ++ map T.unpack args
    sendM (runSubprocessJSON logger "[Effector]" "effector" cmdArgs) >>= \case
      Right val -> pure val
      Left err -> error $ "effector git ls-files failed: " ++ T.unpack err.stderr