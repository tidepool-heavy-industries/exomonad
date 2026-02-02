{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Worktree effect for managing git worktrees.
--
-- Effect type only - interpreter lives in exomonad-native-gui/worktree-interpreter.
-- Enables agents to create isolated working directories for parallel subagents.
--
-- = Example Usage
--
-- @
-- import ExoMonad.Effects.Worktree
--
-- forkHandler :: Member Worktree r => Sem r (Either WorktreeError ())
-- forkHandler = do
--   result <- withWorktree (WorktreeSpec "tests" Nothing Nothing Nothing) $ \wtTests ->
--     withWorktree (WorktreeSpec "impl" Nothing Nothing Nothing) $ \wtImpl -> do
--       -- ... run parallel agents in each worktree ...
--       pure ()
--   pure result
-- @
--
-- = Design Notes
--
-- Worktrees provide isolation for parallel agent execution:
-- - Each worktree has its own working directory
-- - Changes in one worktree don't affect others
-- - Agents can be spawned with --cwd pointing to a worktree
-- - After work completes, files can be cherry-picked back to main
--
-- = Error Handling
--
-- All operations return @Either WorktreeError a@ for explicit error handling.
-- Use 'withWorktree' for bracket-style resource safety (automatic cleanup).
module ExoMonad.Effects.Worktree
  ( -- * Effect
    Worktree (..),
    createWorktree,
    deleteWorktree,
    mergeWorktree,
    cherryPickFiles,
    listWorktrees,

    -- * Construction
    defaultWorktreeSpec,

    -- * Bracket
    withWorktree,

    -- * Types
    WorktreePath (..),
    WorktreeSpec (..),
    MergeResult (..),
    WorktreeError (..),
  )
where

import Polysemy (Sem, Member, makeSem)
import Data.Kind (Type)
import ExoMonad.StructuredOutput (StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Type-safe wrapper for worktree paths.
--
-- Prevents accidental confusion between worktree paths and regular FilePath values.
-- Use 'unWorktreePath' to extract the underlying path when needed.
--
-- @
-- path <- createWorktree spec
-- case path of
--   Right (WorktreePath p) -> runAgent p
--   Left err -> handleError err
-- @
newtype WorktreePath = WorktreePath {unWorktreePath :: FilePath}
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString, ToJSON, FromJSON, StructuredOutput)

-- | Specification for creating a worktree.
data WorktreeSpec = WorktreeSpec
  { -- | Base name for the worktree (e.g., "types-first-tests").
    -- A unique suffix will be added by the interpreter if branchName is Nothing.
    baseName :: Text,
    -- | Branch to create worktree from. Nothing = current HEAD.
    fromBranch :: Maybe Text,
    -- | Explicit branch name to use. If provided, no suffix is added.
    branchName :: Maybe Text,
    -- | Explicit path for the worktree. If provided, wcWorktreeDir is ignored.
    path :: Maybe FilePath
  }
  deriving stock (Show, Eq)

-- | Default specification for a worktree.
defaultWorktreeSpec :: Text -> WorktreeSpec
defaultWorktreeSpec baseName =
  WorktreeSpec
    { baseName = baseName,
      fromBranch = Nothing,
      branchName = Nothing,
      path = Nothing
    }

-- | Result of merging a worktree back to main.
data MergeResult
  = -- | Merge completed successfully.
    MergeSuccess
  | -- | Merge had conflicts. Text describes the conflicts.
    MergeConflict Text
  deriving stock (Show, Eq, Generic)

instance ToJSON MergeResult

instance FromJSON MergeResult

-- ════════════════════════════════════════════════════════════════════════════
-- ERRORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Errors that can occur during worktree operations.
--
-- Use pattern matching to handle specific error cases:
--
-- @
-- case result of
--   Left (WorktreeGitError cmd code stderr) -> logError stderr
--   Left (WorktreeFileCopyError src dest reason) -> retry
--   Right value -> proceed value
-- @
data WorktreeError
  = WorktreeGitError
      { -- | Git command that failed (e.g., "worktree add")
        command :: Text,
        -- | Exit code from git
        exitCode :: Int,
        -- | Stderr output from git
        stderr :: Text
      }
  | -- \^ Git command failed

    -- | Failed to copy files during cherry-pick
    WorktreeFileCopyError
      { srcPath :: FilePath,
        destPath :: FilePath,
        reason :: Text
      }
  deriving stock (Eq, Show, Generic)

instance ToJSON WorktreeError

instance FromJSON WorktreeError

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Worktree effect for managing git worktrees.
--
-- All operations return @Either WorktreeError@ for explicit error handling.
-- Provides isolation for parallel agent execution.
data Worktree m a where
  -- | Create a new worktree with a branch.
  -- Returns the path to the created worktree, or an error.
  CreateWorktree ::
    WorktreeSpec ->
    Worktree m (Either WorktreeError WorktreePath)
  -- | Delete a worktree and its branch.
  -- Returns () on success, or an error if deletion failed.
  DeleteWorktree ::
    WorktreePath ->
    Worktree m (Either WorktreeError ())
  -- | Merge changes from a worktree back to main branch.
  -- Returns MergeSuccess or MergeConflict on success, or an error.
  MergeWorktree ::
    -- | Worktree path to merge from
    WorktreePath ->
    -- | Commit message for the merge
    Text ->
    Worktree m (Either WorktreeError MergeResult)
  -- | Cherry-pick specific files from a worktree to a destination.
  -- Copies files without committing - useful for combining outputs.
  CherryPickFiles ::
    -- | Source worktree
    WorktreePath ->
    -- | Relative file paths to copy
    [FilePath] ->
    -- | Destination directory
    FilePath ->
    Worktree m (Either WorktreeError ())
  -- | List all worktrees for the repository.
  -- Returns list of (path, branch) pairs.
  ListWorktrees ::
    Worktree m (Either WorktreeError [(WorktreePath, Text)])

makeSem ''Worktree

-- ════════════════════════════════════════════════════════════════════════════
-- BRACKET
-- ════════════════════════════════════════════════════════════════════════════

-- | Bracket-style worktree management with guaranteed cleanup.
--
-- Creates a worktree, runs an action, and deletes the worktree afterward
-- regardless of success or failure of the action.
--
-- @
-- result <- withWorktree (WorktreeSpec "tests" Nothing Nothing Nothing) $ \wtPath -> do
--   -- Work in the worktree (wtPath :: WorktreePath)
--   output <- runAgent (unWorktreePath wtPath)
--   pure output
-- -- Worktree is automatically deleted here
-- @
--
-- __Cleanup semantics:__ The worktree is ALWAYS deleted after the action
-- completes, whether it succeeds or fails. This is "temporary workspace"
-- semantics - use manual 'createWorktree'/'deleteWorktree' if you need
-- the worktree to persist beyond the action.
--
-- Note: If the action throws an exception at the IO level, cleanup may not
-- occur. For full IO-level safety, the interpreter also uses IO bracket.
withWorktree ::
  (Member Worktree r) =>
  WorktreeSpec ->
  (WorktreePath -> Sem r a) ->
  Sem r (Either WorktreeError a)
withWorktree spec action = do
  createResult <- createWorktree spec
  case createResult of
    Left err -> pure $ Left err
    Right wtPath -> do
      -- Run the action
      result <- action wtPath
      -- Always cleanup (best effort - ignore delete errors)
      _ <- deleteWorktree wtPath
      pure $ Right result

