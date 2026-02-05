{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | Git effect for querying repository state.
--
-- Effect type only - interpreters live in exomonad-git-interpreter.
-- Enables agents to query git worktree info, dirty files, and commit history.
--
-- = Example Usage
--
-- @
-- import ExoMonad.Effects.Git (Git, getWorktreeInfo, getDirtyFiles)
--
-- myHandler :: Member Git r => Sem r ()
-- myHandler = do
--   wt <- getWorktreeInfo
--   dirtyFiles <- getDirtyFiles
--   -- process git info...
-- @
module ExoMonad.Effects.Git
  ( -- * Effect
    Git (..),
    getWorktreeInfo,
    getDirtyFiles,
    getRecentCommits,
    getCurrentBranch,
    getCommitsAhead,
    fetchRemote,

    -- * Types
    WorktreeInfo (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Kind (Type)
import ExoMonad.Path (Abs, Dir, File, Path, Rel, toFilePathText)
import Polysemy (Member, Sem, makeSem)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Information about the current git worktree or repo.
data WorktreeInfo = WorktreeInfo
  { -- | Worktree name (e.g., "gh-123", "native-server").
    -- For main repo, this is "main".
    wiName :: Text,
    -- | Absolute path to the worktree root.
    wiPath :: Path Abs Dir,
    -- | Current branch name.
    wiBranch :: Text,
    -- | Path to the main git repository (may be same as wiPath).
    wiRepoRoot :: Path Abs Dir,
    -- | True if this is a worktree, False if main repo.
    wiIsWorktree :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON WorktreeInfo where
  toJSON wt =
    object
      [ "name" .= wt.wiName,
        "path" .= toFilePathText wt.wiPath,
        "branch" .= wt.wiBranch,
        "repo_root" .= toFilePathText wt.wiRepoRoot,
        "is_worktree" .= wt.wiIsWorktree
      ]

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Git effect for repository operations.
--
-- Mostly read-only queries, plus fetch for remote sync.
data Git m a where
  -- | Get worktree/repo information.
  -- Returns Nothing if not in a git repository.
  GetWorktreeInfo :: Git m (Maybe WorktreeInfo)
  -- | Get list of dirty (uncommitted) files.
  GetDirtyFiles :: Git m [Path Rel File]
  -- | Get recent commit subjects.
  GetRecentCommits :: Int -> Git m [Text]
  -- | Get current branch name.
  GetCurrentBranch :: Git m Text
  -- | Get number of commits ahead of a ref (e.g., "origin/main").
  GetCommitsAhead :: Text -> Git m Int
  -- | Fetch from a remote (e.g., "origin") to update refs.
  -- Optionally specify a refspec (e.g., "main" to fetch only main).
  FetchRemote :: Text -> Maybe Text -> Git m ()

makeSem ''Git
