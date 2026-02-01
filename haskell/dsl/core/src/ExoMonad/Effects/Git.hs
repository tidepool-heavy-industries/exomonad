-- | Git effect for querying repository state.
--
-- Effect type only - interpreters live in exomonad-git-interpreter.
-- Enables graphs to query git worktree info, dirty files, and commit history.
--
-- = Example Usage
--
-- @
-- import ExoMonad.Effects.Git (Git, getWorktreeInfo, getDirtyFiles)
--
-- myHandler :: Member Git effs => Eff effs ()
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

import Control.Monad.Freer (Eff, Member, send)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Information about the current git worktree or repo.
data WorktreeInfo = WorktreeInfo
  { -- | Worktree name (e.g., "gh-123", "native-server").
    -- For main repo, this is "main".
    wiName :: Text,
    -- | Absolute path to the worktree root.
    wiPath :: FilePath,
    -- | Current branch name.
    wiBranch :: Text,
    -- | Path to the main git repository (may be same as wiPath).
    wiRepoRoot :: FilePath,
    -- | True if this is a worktree, False if main repo.
    wiIsWorktree :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON WorktreeInfo where
  toJSON wt =
    object
      [ "name" .= wt.wiName,
        "path" .= wt.wiPath,
        "branch" .= wt.wiBranch,
        "repo_root" .= wt.wiRepoRoot,
        "is_worktree" .= wt.wiIsWorktree
      ]

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Git effect for repository operations.
--
-- Mostly read-only queries, plus fetch for remote sync.
data Git r where
  -- | Get worktree/repo information.
  -- Returns Nothing if not in a git repository.
  GetWorktreeInfo :: Git (Maybe WorktreeInfo)
  -- | Get list of dirty (uncommitted) files.
  GetDirtyFiles :: Git [FilePath]
  -- | Get recent commit subjects.
  GetRecentCommits :: Int -> Git [Text]
  -- | Get current branch name.
  GetCurrentBranch :: Git Text
  -- | Get number of commits ahead of a ref (e.g., "origin/main").
  GetCommitsAhead :: Text -> Git Int
  -- | Fetch from a remote (e.g., "origin") to update refs.
  -- Optionally specify a refspec (e.g., "main" to fetch only main).
  FetchRemote :: Text -> Maybe Text -> Git ()

-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Get worktree/repo information.
getWorktreeInfo :: (Member Git effs) => Eff effs (Maybe WorktreeInfo)
getWorktreeInfo = send GetWorktreeInfo

-- | Get list of dirty files.
getDirtyFiles :: (Member Git effs) => Eff effs [FilePath]
getDirtyFiles = send GetDirtyFiles

-- | Get recent commit subjects.
getRecentCommits :: (Member Git effs) => Int -> Eff effs [Text]
getRecentCommits = send . GetRecentCommits

-- | Get current branch name.
getCurrentBranch :: (Member Git effs) => Eff effs Text
getCurrentBranch = send GetCurrentBranch

-- | Get number of commits ahead of a ref (e.g., "origin/main").
getCommitsAhead :: (Member Git effs) => Text -> Eff effs Int
getCommitsAhead = send . GetCommitsAhead

-- | Fetch from a remote to update refs.
-- Example: @fetchRemote "origin" (Just "main")@ fetches only main from origin.
-- Example: @fetchRemote "origin" Nothing@ fetches all refs from origin.
fetchRemote :: (Member Git effs) => Text -> Maybe Text -> Eff effs ()
fetchRemote remote refspec = send $ FetchRemote remote refspec
