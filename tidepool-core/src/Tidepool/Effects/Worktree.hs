-- | Worktree effect for managing git worktrees.
--
-- Effect type only - executor lives in tidepool-native-gui/worktree-executor.
-- Enables graphs to create isolated working directories for parallel agents.
--
-- = Example Usage
--
-- @
-- import Tidepool.Effects.Worktree (Worktree, WorktreeSpec(..), createWorktree, deleteWorktree)
--
-- forkHandler :: Member Worktree effs => Eff effs ()
-- forkHandler = do
--   wtTests <- createWorktree (WorktreeSpec "types-first-tests" Nothing)
--   wtImpl  <- createWorktree (WorktreeSpec "types-first-impl" Nothing)
--   -- ... run parallel agents in each worktree ...
--   deleteWorktree wtTests
--   deleteWorktree wtImpl
-- @
--
-- = Design Notes
--
-- Worktrees provide isolation for parallel agent execution:
-- - Each worktree has its own working directory
-- - Changes in one worktree don't affect others
-- - Claude Code can be spawned with --cwd pointing to a worktree
-- - After work completes, files can be cherry-picked back to main
module Tidepool.Effects.Worktree
  ( -- * Effect
    Worktree(..)
  , createWorktree
  , deleteWorktree
  , mergeWorktree
  , cherryPickFiles
  , listWorktrees

    -- * Types
  , WorktreeSpec(..)
  , MergeResult(..)
  ) where

import Control.Monad.Freer (Eff, Member, send)
import Data.Text (Text)


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Specification for creating a worktree.
data WorktreeSpec = WorktreeSpec
  { wsBaseName :: Text
    -- ^ Base name for the worktree (e.g., "types-first-tests").
    -- A unique suffix may be added by the executor.
  , wsFromBranch :: Maybe Text
    -- ^ Branch to create worktree from. Nothing = current HEAD.
  }
  deriving (Show, Eq)

-- | Result of merging a worktree back to main.
data MergeResult
  = MergeSuccess
    -- ^ Merge completed successfully.
  | MergeConflict Text
    -- ^ Merge had conflicts. Text describes the conflicts.
  | MergeError Text
    -- ^ Merge failed for other reasons.
  deriving (Show, Eq)


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Worktree effect for managing git worktrees.
--
-- Provides isolation for parallel agent execution.
data Worktree r where
  -- | Create a new worktree with a branch.
  -- Returns the absolute path to the created worktree.
  CreateWorktree
    :: WorktreeSpec
    -> Worktree FilePath

  -- | Delete a worktree and its branch.
  -- Fails silently if worktree doesn't exist.
  DeleteWorktree
    :: FilePath        -- ^ Worktree path
    -> Worktree ()

  -- | Merge changes from a worktree back to main branch.
  -- The worktree should have committed changes to merge.
  MergeWorktree
    :: FilePath        -- ^ Worktree path to merge from
    -> Text            -- ^ Commit message for the merge
    -> Worktree MergeResult

  -- | Cherry-pick specific files from a worktree to a destination.
  -- Copies files without committing - useful for combining outputs.
  CherryPickFiles
    :: FilePath        -- ^ Source worktree
    -> [FilePath]      -- ^ Relative file paths to copy
    -> FilePath        -- ^ Destination directory
    -> Worktree ()

  -- | List all worktrees for the repository.
  -- Returns list of (path, branch) pairs.
  ListWorktrees
    :: Worktree [(FilePath, Text)]


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Create a new worktree.
--
-- @
-- path <- createWorktree (WorktreeSpec "my-feature" Nothing)
-- -- path is now something like "/repo/worktrees/my-feature-abc123"
-- @
createWorktree :: Member Worktree effs => WorktreeSpec -> Eff effs FilePath
createWorktree = send . CreateWorktree

-- | Delete a worktree and its associated branch.
--
-- Safe to call on non-existent worktrees (no-op).
deleteWorktree :: Member Worktree effs => FilePath -> Eff effs ()
deleteWorktree = send . DeleteWorktree

-- | Merge a worktree's changes back to main branch.
--
-- The worktree should have changes committed to its branch.
-- Returns 'MergeSuccess' if clean, 'MergeConflict' if conflicts occurred.
mergeWorktree :: Member Worktree effs => FilePath -> Text -> Eff effs MergeResult
mergeWorktree path msg = send (MergeWorktree path msg)

-- | Copy specific files from a worktree to a destination.
--
-- Useful for combining outputs from parallel agents without full merge.
--
-- @
-- cherryPickFiles "/worktrees/tests" ["test/StackSpec.hs"] "/worktrees/impl"
-- @
cherryPickFiles :: Member Worktree effs => FilePath -> [FilePath] -> FilePath -> Eff effs ()
cherryPickFiles src files dest = send (CherryPickFiles src files dest)

-- | List all worktrees in the repository.
listWorktrees :: Member Worktree effs => Eff effs [(FilePath, Text)]
listWorktrees = send ListWorktrees
