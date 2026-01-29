-- | Git effect interpreter - CLI client.
--
-- Implements Git effect by calling git CLI commands.
-- All queries are read-only.
module ExoMonad.Git.Interpreter
  ( -- * Interpreter
    runGit
  , runGitIO

    -- * Utilities (for testing)
  , worktreeName
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName, takeDirectory)
import System.Process (readProcessWithExitCode)

import ExoMonad.Effects.Git (Git(..), WorktreeInfo(..))


-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run Git effects with a pure handler.
runGit
  :: Eff effs (Maybe WorktreeInfo)  -- ^ Handler for GetWorktreeInfo
  -> Eff effs [FilePath]            -- ^ Handler for GetDirtyFiles
  -> (Int -> Eff effs [Text])       -- ^ Handler for GetRecentCommits
  -> Eff effs Text                  -- ^ Handler for GetCurrentBranch
  -> (Text -> Eff effs Int)         -- ^ Handler for GetCommitsAhead
  -> Eff (Git ': effs) a
  -> Eff effs a
runGit hWorktree hDirty hCommits hBranch hAhead = interpret $ \case
  GetWorktreeInfo      -> hWorktree
  GetDirtyFiles        -> hDirty
  GetRecentCommits n   -> hCommits n
  GetCurrentBranch     -> hBranch
  GetCommitsAhead ref  -> hAhead ref


-- | Run Git effects using the git CLI.
runGitIO :: LastMember IO effs => Eff (Git ': effs) a -> Eff effs a
runGitIO = interpret $ \case
  GetWorktreeInfo    -> sendM detectWorktree
  GetDirtyFiles      -> sendM getGitDirtyFiles
  GetRecentCommits n -> sendM $ getGitRecentCommits n
  GetCurrentBranch   -> sendM getGitCurrentBranch
  GetCommitsAhead ref -> sendM $ getGitCommitsAhead ref


-- ════════════════════════════════════════════════════════════════════════════
-- GIT CLI FUNCTIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Detect worktree information.
detectWorktree :: IO (Maybe WorktreeInfo)
detectWorktree = do
  -- Get the toplevel of current repo (works for both worktree and main)
  mPath <- gitCommand ["rev-parse", "--show-toplevel"]
  case mPath of
    Nothing -> pure Nothing
    Just path -> do
      let cleanPath = T.unpack $ T.strip $ T.pack path

      -- Get current branch
      -- Use rev-parse --abbrev-ref HEAD for robustness (handles detached HEAD as "HEAD")
      mBranch <- gitCommand ["rev-parse", "--abbrev-ref", "HEAD"]
      let branch = case mBranch of
            Nothing -> "HEAD"
            Just b -> T.strip $ T.pack b

      -- Check if we're in a worktree by looking for .git file (not dir)
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
worktreeName :: FilePath -> Text
worktreeName path =
  let parts = T.splitOn "/" (T.pack path)
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


-- | Get number of commits ahead of a ref.
getGitCommitsAhead :: Text -> IO Int
getGitCommitsAhead ref = do
  let refStr = T.unpack ref <> "..HEAD"
  result <- try $ readProcessWithExitCode "git" ["rev-list", "--count", refStr] ""
  case result of
    Left (_ :: SomeException) -> pure 0
    Right (ExitSuccess, stdout, _) ->
      case reads (T.unpack $ T.strip $ T.pack stdout) of
        [(n, "")] -> pure n
        _ -> pure 0
    Right _ -> pure 0


-- | Run a git command and return stdout on success.
gitCommand :: [String] -> IO (Maybe String)
gitCommand args = do
  result <- try $ readProcessWithExitCode "git" args ""
  case result of
    Left (_ :: SomeException) -> pure Nothing
    Right (ExitSuccess, stdout, _) -> pure $ Just stdout
    Right (ExitFailure _, _, _) -> pure Nothing
