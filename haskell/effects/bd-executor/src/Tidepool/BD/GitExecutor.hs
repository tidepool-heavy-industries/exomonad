-- | Git effect executor - CLI client.
--
-- Implements Git effect by calling git CLI commands.
-- All queries are read-only.
--
-- = Usage
--
-- @
-- import Tidepool.BD.GitExecutor (runGitIO)
-- import Tidepool.Effects.Git (Git, getWorktreeInfo, getDirtyFiles)
--
-- main = runM $ runGitIO $ do
--   mWt <- getWorktreeInfo
--   dirtyFiles <- getDirtyFiles
--   ...
-- @
module Tidepool.BD.GitExecutor
  ( -- * Executor
    runGit
  , runGitIO

    -- * Utilities (for testing)
  , worktreeName
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName, takeDirectory)
import System.Process (readProcessWithExitCode)

import Tidepool.Effects.Git (Git(..), WorktreeInfo(..))


-- ════════════════════════════════════════════════════════════════════════════
-- EXECUTOR
-- ════════════════════════════════════════════════════════════════════════════

-- | Run Git effects with a pure handler.
runGit
  :: Eff effs (Maybe WorktreeInfo)  -- ^ Handler for GetWorktreeInfo
  -> Eff effs [FilePath]            -- ^ Handler for GetDirtyFiles
  -> (Int -> Eff effs [Text])       -- ^ Handler for GetRecentCommits
  -> Eff effs Text                  -- ^ Handler for GetCurrentBranch
  -> Eff (Git ': effs) a
  -> Eff effs a
runGit hWorktree hDirty hCommits hBranch = interpret $ \case
  GetWorktreeInfo      -> hWorktree
  GetDirtyFiles        -> hDirty
  GetRecentCommits n   -> hCommits n
  GetCurrentBranch     -> hBranch


-- | Run Git effects using the git CLI.
runGitIO :: LastMember IO effs => Eff (Git ': effs) a -> Eff effs a
runGitIO = interpret $ \case
  GetWorktreeInfo    -> sendM detectWorktree
  GetDirtyFiles      -> sendM getGitDirtyFiles
  GetRecentCommits n -> sendM $ getGitRecentCommits n
  GetCurrentBranch   -> sendM getGitCurrentBranch


-- ════════════════════════════════════════════════════════════════════════════
-- GIT CLI FUNCTIONS
-- ════════════════════════════════════════════════════════════════════════════

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


-- | Get list of dirty (uncommitted) files.
getGitDirtyFiles :: IO [FilePath]
getGitDirtyFiles = do
  result <- try $ readProcessWithExitCode "git" ["status", "--porcelain"] ""
  case result of
    Left (_ :: SomeException) -> pure []
    Right (ExitSuccess, stdout, _) ->
      pure $ map (drop 3) $ lines stdout  -- Drop status prefix "XY "
    Right _ -> pure []


-- | Get recent commit subjects.
getGitRecentCommits :: Int -> IO [Text]
getGitRecentCommits n = do
  result <- try $ readProcessWithExitCode "git"
    ["log", "--oneline", "-" ++ show n, "--format=%s"] ""
  case result of
    Left (_ :: SomeException) -> pure []
    Right (ExitSuccess, stdout, _) ->
      pure $ map T.pack $ take n $ lines stdout
    Right _ -> pure []


-- | Get current branch name.
getGitCurrentBranch :: IO Text
getGitCurrentBranch = do
  result <- try $ readProcessWithExitCode "git" ["branch", "--show-current"] ""
  case result of
    Left (_ :: SomeException) -> pure "HEAD"
    Right (ExitSuccess, stdout, _) -> pure $ T.strip $ T.pack stdout
    Right _ -> pure "HEAD"


-- | Run a git command and return stdout on success.
gitCommand :: [String] -> IO (Maybe String)
gitCommand args = do
  result <- try $ readProcessWithExitCode "git" args ""
  case result of
    Left (_ :: SomeException) -> pure Nothing
    Right (ExitSuccess, stdout, _) -> pure $ Just stdout
    Right (ExitFailure _, _, _) -> pure Nothing
