{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.Effects.Git
  ( runGitViaSsh
  ) where

import Control.Monad.Freer (Eff, Member, interpret)
import Data.Text (Text)
import Data.Text qualified as T

import Tidepool.Control.Effects.SshExec (SshExec, ExecRequest(..), ExecResult(..), execCommand)
import Tidepool.Effects.Git (Git(..), WorktreeInfo(..))

-- | Interpreter: uses SshExec to run git commands
runGitViaSsh :: Member SshExec effs => Text -> Eff (Git ': effs) a -> Eff effs a
runGitViaSsh container = interpret $ \case
  GetWorktreeInfo -> do
    -- This is a bit complex to implement perfectly via SSH without more logic,
    -- but we can try to get the basic info.
    mPath <- gitCommand container ["rev-parse", "--show-toplevel"]
    case mPath of
      Nothing -> pure Nothing
      Just path -> do
        mBranch <- gitCommand container ["branch", "--show-current"]
        let branch = maybe "HEAD" T.strip mBranch
        
        -- Simplified worktree detection
        mGitDir <- gitCommand container ["rev-parse", "--git-dir"]
        let inWorktree = maybe False ("worktrees" `T.isInfixOf`) mGitDir
        
        pure $ Just WorktreeInfo
          { wiName = if inWorktree then "ssh-worktree" else "main"
          , wiPath = T.unpack (T.strip path)
          , wiBranch = branch
          , wiRepoRoot = T.unpack (T.strip path) -- Simplified
          , wiIsWorktree = inWorktree
          }

  GetDirtyFiles -> do
    result <- execCommand $ ExecRequest
      { erContainer = container
      , erCommand = "git"
      , erArgs = ["status", "--porcelain"]
      , erWorkingDir = "."
      , erEnv = []
      , erTimeout = 30
      }
    pure $ if exExitCode result == 0
      then map (drop 3 . T.unpack) $ T.lines (exStdout result)
      else []

  GetRecentCommits n -> do
    result <- execCommand $ ExecRequest
      { erContainer = container
      , erCommand = "git"
      , erArgs = ["log", "--oneline", "-" <> T.pack (show n), "--format=%s"]
      , erWorkingDir = "."
      , erEnv = []
      , erTimeout = 30
      }
    pure $ if exExitCode result == 0
      then T.lines (exStdout result)
      else []

  GetCurrentBranch -> do
    mBranch <- gitCommand container ["branch", "--show-current"]
    pure $ maybe "HEAD" T.strip mBranch

  GetCommitsAhead ref -> do
    result <- execCommand $ ExecRequest
      { erContainer = container
      , erCommand = "git"
      , erArgs = ["rev-list", "--count", ref <> "..HEAD"]
      , erWorkingDir = "."
      , erEnv = []
      , erTimeout = 30
      }
    pure $ if exExitCode result == 0
      then case reads (T.unpack $ T.strip $ exStdout result) of
        [(n, "")] -> n
        _ -> 0
      else 0

-- | Helper to run a git command via SshExec
gitCommand :: Member SshExec effs => Text -> [Text] -> Eff effs (Maybe Text)
gitCommand container args = do
  result <- execCommand $ ExecRequest
    { erContainer = container
    , erCommand = "git"
    , erArgs = args
    , erWorkingDir = "."
    , erEnv = []
    , erTimeout = 30
    }
  pure $ if exExitCode result == 0
    then Just (exStdout result)
    else Nothing
