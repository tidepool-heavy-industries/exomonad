-- | Worktree effect for managing git worktrees.
--
-- Effect type only - interpreter lives in tidepool-native-gui/worktree-interpreter.
-- Enables graphs to create isolated working directories for parallel agents.
--
-- = Example Usage
--
-- @
-- import Tidepool.Effects.Worktree
--
-- forkHandler :: Member Worktree effs => Eff effs (Either WorktreeError ())
-- forkHandler = do
--   result <- withWorktree (WorktreeSpec "tests" Nothing) $ \wtTests ->
--     withWorktree (WorktreeSpec "impl" Nothing) $ \wtImpl -> do
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
-- - Claude Code can be spawned with --cwd pointing to a worktree
-- - After work completes, files can be cherry-picked back to main
--
-- = Error Handling
--
-- All operations return @Either WorktreeError a@ for explicit error handling.
-- Use 'withWorktree' for bracket-style resource safety (automatic cleanup).
module Tidepool.Effects.Worktree
  ( -- * Effect
    Worktree(..)
  , createWorktree
  , deleteWorktree
  , mergeWorktree
  , cherryPickFiles
  , listWorktrees

    -- * Bracket
  , withWorktree

    -- * Types
  , WorktreePath(..)
  , WorktreeSpec(..)
  , MergeResult(..)
  , WorktreeError(..)
  ) where

import Control.Monad.Freer (Eff, Member, send)
import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.StructuredOutput (StructuredOutput(..))


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
--   Right (WorktreePath p) -> runClaudeCode p
--   Left err -> handleError err
-- @
newtype WorktreePath = WorktreePath { unWorktreePath :: FilePath }
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString, ToJSON, FromJSON)

-- | StructuredOutput for WorktreePath delegates to String.
instance StructuredOutput WorktreePath where
  structuredSchema = structuredSchema @String
  encodeStructured (WorktreePath p) = encodeStructured p
  parseStructured v = WorktreePath <$> parseStructured v

-- | Specification for creating a worktree.
data WorktreeSpec = WorktreeSpec
  { wsBaseName :: Text
    -- ^ Base name for the worktree (e.g., "types-first-tests").
    -- A unique suffix will be added by the interpreter.
  , wsFromBranch :: Maybe Text
    -- ^ Branch to create worktree from. Nothing = current HEAD.
  }
  deriving stock (Show, Eq)

-- | Result of merging a worktree back to main.
data MergeResult
  = MergeSuccess
    -- ^ Merge completed successfully.
  | MergeConflict Text
    -- ^ Merge had conflicts. Text describes the conflicts.
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
      { wgeCommand :: Text
        -- ^ Git command that failed (e.g., "worktree add")
      , wgeExitCode :: Int
        -- ^ Exit code from git
      , wgeStderr :: Text
        -- ^ Stderr output from git
      }
    -- ^ Git command failed
  | WorktreeFileCopyError
      { wfceSrcPath :: FilePath
      , wfceDestPath :: FilePath
      , wfceReason :: Text
      }
    -- ^ Failed to copy files during cherry-pick
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
data Worktree r where
  -- | Create a new worktree with a branch.
  -- Returns the path to the created worktree, or an error.
  CreateWorktree
    :: WorktreeSpec
    -> Worktree (Either WorktreeError WorktreePath)

  -- | Delete a worktree and its branch.
  -- Returns () on success, or an error if deletion failed.
  DeleteWorktree
    :: WorktreePath
    -> Worktree (Either WorktreeError ())

  -- | Merge changes from a worktree back to main branch.
  -- Returns MergeSuccess or MergeConflict on success, or an error.
  MergeWorktree
    :: WorktreePath        -- ^ Worktree path to merge from
    -> Text                -- ^ Commit message for the merge
    -> Worktree (Either WorktreeError MergeResult)

  -- | Cherry-pick specific files from a worktree to a destination.
  -- Copies files without committing - useful for combining outputs.
  CherryPickFiles
    :: WorktreePath        -- ^ Source worktree
    -> [FilePath]          -- ^ Relative file paths to copy
    -> FilePath            -- ^ Destination directory
    -> Worktree (Either WorktreeError ())

  -- | List all worktrees for the repository.
  -- Returns list of (path, branch) pairs.
  ListWorktrees
    :: Worktree (Either WorktreeError [(WorktreePath, Text)])


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Create a new worktree.
--
-- @
-- result <- createWorktree (WorktreeSpec "my-feature" Nothing)
-- case result of
--   Right path -> -- path is like "/repo/.worktrees/my-feature-abc123"
--   Left err -> handleError err
-- @
createWorktree
  :: Member Worktree effs
  => WorktreeSpec
  -> Eff effs (Either WorktreeError WorktreePath)
createWorktree = send . CreateWorktree

-- | Delete a worktree and its associated branch.
--
-- Returns 'Right ()' on success (including when the worktree doesn't exist),
-- or 'Left WorktreeGitError' if git fails unexpectedly.
deleteWorktree
  :: Member Worktree effs
  => WorktreePath
  -> Eff effs (Either WorktreeError ())
deleteWorktree = send . DeleteWorktree

-- | Merge a worktree's changes back to main branch.
--
-- The worktree should have changes committed to its branch.
-- Returns 'MergeSuccess' if clean, 'MergeConflict' if conflicts occurred,
-- or 'Left WorktreeGitError' if git fails.
mergeWorktree
  :: Member Worktree effs
  => WorktreePath
  -> Text
  -> Eff effs (Either WorktreeError MergeResult)
mergeWorktree path msg = send (MergeWorktree path msg)

-- | Copy specific files from a worktree to a destination.
--
-- Useful for combining outputs from parallel agents without full merge.
--
-- @
-- cherryPickFiles testsWt ["test/StackSpec.hs"] implWt
-- @
cherryPickFiles
  :: Member Worktree effs
  => WorktreePath
  -> [FilePath]
  -> FilePath
  -> Eff effs (Either WorktreeError ())
cherryPickFiles src files dest = send (CherryPickFiles src files dest)

-- | List all worktrees in the repository.
listWorktrees
  :: Member Worktree effs
  => Eff effs (Either WorktreeError [(WorktreePath, Text)])
listWorktrees = send ListWorktrees


-- ════════════════════════════════════════════════════════════════════════════
-- BRACKET
-- ════════════════════════════════════════════════════════════════════════════

-- | Bracket-style worktree management with guaranteed cleanup.
--
-- Creates a worktree, runs an action, and deletes the worktree afterward
-- regardless of success or failure of the action.
--
-- @
-- result <- withWorktree (WorktreeSpec "tests" Nothing) $ \wtPath -> do
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
withWorktree
  :: Member Worktree effs
  => WorktreeSpec
  -> (WorktreePath -> Eff effs a)
  -> Eff effs (Either WorktreeError a)
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
