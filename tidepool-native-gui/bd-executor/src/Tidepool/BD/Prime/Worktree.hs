-- | Git worktree detection for urchin prime.
--
-- Detects whether we're in a git worktree or the main repo,
-- extracts branch and path information for context generation.
module Tidepool.BD.Prime.Worktree
  ( -- * Types
    WorktreeInfo(..)

    -- * Detection
  , detectWorktree
  , detectFromCwd

    -- * Utilities
  , worktreeName
  , isWorktree
  ) where

import Control.Exception (try, SomeException)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName, takeDirectory)
import System.Process (readProcessWithExitCode)


-- | Information about the current git worktree or repo.
data WorktreeInfo = WorktreeInfo
  { wiName     :: Text
    -- ^ Worktree name (e.g., "bd", "native-server").
    -- For main repo, this is "main".
  , wiPath     :: FilePath
    -- ^ Absolute path to the worktree root.
  , wiBranch   :: Text
    -- ^ Current branch name.
  , wiRepoRoot :: FilePath
    -- ^ Path to the main git repository (may be same as wiPath).
  , wiIsWorktree :: Bool
    -- ^ True if this is a worktree, False if main repo.
  }
  deriving (Show, Eq)


-- | Detect worktree info from the current working directory.
--
-- Returns Nothing if not in a git repository.
detectFromCwd :: IO (Maybe WorktreeInfo)
detectFromCwd = detectWorktree


-- | Detect worktree information.
--
-- Uses git commands to determine:
-- - Whether we're in a worktree or main repo
-- - Current branch
-- - Repository root paths
detectWorktree :: IO (Maybe WorktreeInfo)
detectWorktree = do
  -- Get the toplevel of current repo (works for both worktree and main)
  mPath <- gitCommand ["rev-parse", "--show-toplevel"]
  case mPath of
    Nothing -> pure Nothing
    Just path -> do
      let cleanPath = T.unpack $ T.strip $ T.pack path

      -- Get current branch
      mBranch <- gitCommand ["branch", "--show-current"]
      let branch = maybe "HEAD" (T.strip . T.pack) mBranch

      -- Check if we're in a worktree by looking for .git file (not dir)
      -- Worktrees have a .git file pointing to main repo's .git/worktrees/X
      mGitDir <- gitCommand ["rev-parse", "--git-dir"]

      case mGitDir of
        Nothing -> pure Nothing
        Just gitDir -> do
          let cleanGitDir = T.unpack $ T.strip $ T.pack gitDir

          -- If git-dir contains "/worktrees/", we're in a worktree
          let inWorktree = "/worktrees/" `T.isInfixOf` T.pack cleanGitDir

          -- Get the main repo root
          mMainRoot <- if inWorktree
            then gitCommand ["rev-parse", "--path-format=absolute", "--git-common-dir"]
            else pure (Just cleanPath)

          let mainRoot = case mMainRoot of
                Just r -> takeDirectory $ T.unpack $ T.strip $ T.pack r  -- Remove .git suffix
                Nothing -> cleanPath

          let name = if inWorktree
                then worktreeName cleanPath
                else "main"

          pure $ Just WorktreeInfo
            { wiName = name
            , wiPath = cleanPath
            , wiBranch = branch
            , wiRepoRoot = mainRoot
            , wiIsWorktree = inWorktree
            }


-- | Extract worktree name from path.
--
-- Assumes worktrees are in a "worktrees" directory:
-- @/home/user/project/worktrees/bd@ -> @"bd"@
--
-- Falls back to the last path component if not in worktrees dir.
worktreeName :: FilePath -> Text
worktreeName path =
  let parts = T.splitOn "/" (T.pack path)
      -- Find "worktrees" and take the next component
      findAfterWorktrees [] = Nothing
      findAfterWorktrees [_] = Nothing
      findAfterWorktrees (x:y:rest)
        | x == "worktrees" = Just y
        | otherwise = findAfterWorktrees (y:rest)
  in case findAfterWorktrees parts of
       Just name -> name
       Nothing -> T.pack $ takeFileName path


-- | Check if the current directory is in a git worktree.
isWorktree :: IO Bool
isWorktree = do
  mInfo <- detectWorktree
  pure $ maybe False wiIsWorktree mInfo


-- | Run a git command and return stdout on success.
gitCommand :: [String] -> IO (Maybe String)
gitCommand args = do
  result <- try $ readProcessWithExitCode "git" args ""
  case result of
    Left (_ :: SomeException) -> pure Nothing
    Right (ExitSuccess, stdout, _) -> pure $ Just stdout
    Right (ExitFailure _, _, _) -> pure Nothing
