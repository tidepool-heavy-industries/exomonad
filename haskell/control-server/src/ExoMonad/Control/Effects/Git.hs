{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.Effects.Git
  ( runGitRemote,
  )
where

import Control.Monad.Freer (Eff, Member, interpret)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Control.Effects.SshExec (ExecRequest (..), ExecResult (..), SshExec, execCommand)
import ExoMonad.Effects.Git (Git (..), WorktreeInfo (..))

-- | Interpreter: uses SshExec to run git commands remotely
-- Takes a container name and a working directory (relative to container root).
runGitRemote :: (Member SshExec effs) => Text -> FilePath -> Eff (Git ': effs) a -> Eff effs a
runGitRemote container workDir = interpret $ \case
  GetWorktreeInfo -> do
    -- This is a bit complex to implement perfectly for remote containers without more logic,
    -- but we can try to get the basic info.
    mPath <- gitCommand container workDir ["rev-parse", "--show-toplevel"]
    case mPath of
      Nothing -> pure Nothing
      Just path -> do
        mBranch <- gitCommand container workDir ["branch", "--show-current"]
        let branch = maybe "HEAD" T.strip mBranch

        -- Simplified worktree detection
        mGitDir <- gitCommand container workDir ["rev-parse", "--git-dir"]
        let inWorktree = maybe False ("worktrees" `T.isInfixOf`) mGitDir

        pure $
          Just
            WorktreeInfo
              { wiName = if inWorktree then "remote-worktree" else "main",
                wiPath = T.unpack (T.strip path),
                wiBranch = branch,
                wiRepoRoot = T.unpack (T.strip path), -- Simplified
                wiIsWorktree = inWorktree
              }
  GetDirtyFiles -> do
    result <-
      execCommand $
        ExecRequest
          { container = Just container,
            command = "git",
            args = ["status", "--porcelain"],
            workingDir = workDir,
            env = [],
            timeout = 30
          }
    pure $
      if result.exitCode == Just 0
        then map (drop 3 . T.unpack) $ T.lines (result.stdout)
        else []
  GetRecentCommits n -> do
    result <-
      execCommand $
        ExecRequest
          { container = Just container,
            command = "git",
            args = ["log", "--oneline", "-" <> T.pack (show n), "--format=%s"],
            workingDir = workDir,
            env = [],
            timeout = 30
          }
    pure $
      if result.exitCode == Just 0
        then T.lines (result.stdout)
        else []
  GetCurrentBranch -> do
    mBranch <- gitCommand container workDir ["branch", "--show-current"]
    pure $ maybe "HEAD" T.strip mBranch
  GetCommitsAhead ref -> do
    result <-
      execCommand $
        ExecRequest
          { container = Just container,
            command = "git",
            args = ["rev-list", "--count", ref <> "..HEAD"],
            workingDir = workDir,
            env = [],
            timeout = 30
          }
    pure $
      if result.exitCode == Just 0
        then case reads (T.unpack $ T.strip $ result.stdout) of
          [(n, "")] -> n
          _ -> 0
        else 0
  FetchRemote remote mRefspec -> do
    let args = case mRefspec of
          Just refspec -> ["fetch", remote, refspec]
          Nothing -> ["fetch", remote]
    _ <-
      execCommand $
        ExecRequest
          { container = Just container,
            command = "git",
            args = args,
            workingDir = workDir,
            env = [],
            timeout = 120 -- Fetches can take longer
          }
    pure ()

-- | Helper to run a git command via SshExec
gitCommand :: (Member SshExec effs) => Text -> FilePath -> [Text] -> Eff effs (Maybe Text)
gitCommand container wd args = do
  result <-
    execCommand $
      ExecRequest
        { container = Just container,
          command = "git",
          args = args,
          workingDir = wd,
          env = [],
          timeout = 30
        }
  pure $
    if result.exitCode == Just 0
      then Just (result.stdout)
      else Nothing
