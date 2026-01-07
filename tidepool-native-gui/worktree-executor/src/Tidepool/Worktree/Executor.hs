-- | Worktree effect executor - git worktree management.
--
-- Implements Worktree effect by calling git commands.
-- Enables parallel agent execution in isolated working directories.
--
-- = Usage
--
-- @
-- import Tidepool.Worktree.Executor (runWorktreeIO)
-- import Tidepool.Effects.Worktree
--
-- main = runM $ runWorktreeIO "/path/to/repo" $ do
--   path <- createWorktree (WorktreeSpec "my-feature" Nothing)
--   -- path is now something like "/path/to/repo/.worktrees/my-feature-a1b2c3"
--   -- ... spawn Claude Code with --cwd path ...
--   deleteWorktree path
-- @
--
-- = Requirements
--
-- Requires git to be installed and the working directory to be inside a git repo.
module Tidepool.Worktree.Executor
  ( -- * Executor
    runWorktreeIO

    -- * Configuration
  , WorktreeConfig(..)
  , defaultWorktreeConfig
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.Directory (createDirectoryIfMissing, copyFile, doesDirectoryExist, removeDirectoryRecursive)
import System.Process (readProcessWithExitCode)
import System.Random (randomRIO)
import Numeric (showHex)

import Tidepool.Effects.Worktree
  ( Worktree(..)
  , WorktreeSpec(..)
  , MergeResult(..)
  )


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for Worktree executor.
data WorktreeConfig = WorktreeConfig
  { wcRepoRoot :: FilePath
    -- ^ Root of the git repository.
  , wcWorktreeDir :: FilePath
    -- ^ Directory where worktrees are created (relative to repo root).
    -- Defaults to ".worktrees".
  , wcQuiet :: Bool
    -- ^ Suppress verbose output.
  }
  deriving (Show, Eq)

-- | Default configuration.
defaultWorktreeConfig :: FilePath -> WorktreeConfig
defaultWorktreeConfig repoRoot = WorktreeConfig
  { wcRepoRoot = repoRoot
  , wcWorktreeDir = ".worktrees"
  , wcQuiet = True
  }


-- ════════════════════════════════════════════════════════════════════════════
-- EXECUTOR
-- ════════════════════════════════════════════════════════════════════════════

-- | Run Worktree effects using git commands.
--
-- This interpreter shells out to git for each operation.
runWorktreeIO :: LastMember IO effs => WorktreeConfig -> Eff (Worktree ': effs) a -> Eff effs a
runWorktreeIO config = interpret $ \case
  CreateWorktree spec -> sendM $ createWorktreeIO config spec
  DeleteWorktree path -> sendM $ deleteWorktreeIO config path
  MergeWorktree path msg -> sendM $ mergeWorktreeIO config path msg
  CherryPickFiles src files dest -> sendM $ cherryPickFilesIO src files dest
  ListWorktrees -> sendM $ listWorktreesIO config


-- ════════════════════════════════════════════════════════════════════════════
-- IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Create a new worktree with a unique branch.
createWorktreeIO :: WorktreeConfig -> WorktreeSpec -> IO FilePath
createWorktreeIO config spec = do
  -- Generate unique suffix
  suffix <- randomHex 6

  let branchName = T.unpack spec.wsBaseName <> "-" <> suffix
      wtDir = config.wcRepoRoot </> config.wcWorktreeDir
      wtPath = wtDir </> branchName
      baseBranch = maybe "HEAD" T.unpack spec.wsFromBranch

  -- Ensure worktree directory exists
  createDirectoryIfMissing True wtDir

  -- Create the worktree with a new branch
  (exitCode, _stdout, stderr) <- readProcessWithExitCode
    "git"
    [ "-C", config.wcRepoRoot
    , "worktree", "add"
    , "-b", branchName  -- Create new branch
    , wtPath
    , baseBranch        -- Start from this ref
    ]
    ""

  case exitCode of
    ExitSuccess -> pure wtPath
    ExitFailure code -> error $
      "git worktree add failed (exit " <> show code <> "): " <> stderr

-- | Delete a worktree and its branch.
deleteWorktreeIO :: WorktreeConfig -> FilePath -> IO ()
deleteWorktreeIO config wtPath = do
  -- Check if worktree exists
  exists <- doesDirectoryExist wtPath
  if not exists
    then pure ()  -- Silently succeed if already gone
    else do
      -- Get the branch name from the worktree
      branchResult <- try @SomeException $ readProcessWithExitCode
        "git"
        [ "-C", wtPath
        , "rev-parse", "--abbrev-ref", "HEAD"
        ]
        ""

      let branchName = case branchResult of
            Right (ExitSuccess, out, _) -> Just $ T.strip $ T.pack out
            _ -> Nothing

      -- Remove the worktree
      (exitCode, _, stderr) <- readProcessWithExitCode
        "git"
        [ "-C", config.wcRepoRoot
        , "worktree", "remove"
        , "--force"  -- Force in case of uncommitted changes
        , wtPath
        ]
        ""

      case exitCode of
        ExitSuccess -> pure ()
        ExitFailure _ -> do
          -- Fallback: remove directory manually if git command fails
          result <- try $ removeDirectoryRecursive wtPath
          case result of
            Left (_ :: SomeException) -> error $ "Failed to remove worktree: " <> stderr
            Right () -> pure ()

      -- Delete the branch (best effort)
      case branchName of
        Nothing -> pure ()
        Just branch -> do
          _ <- readProcessWithExitCode
            "git"
            [ "-C", config.wcRepoRoot
            , "branch", "-D"
            , T.unpack branch
            ]
            ""
          pure ()

-- | Merge a worktree's branch back to main.
mergeWorktreeIO :: WorktreeConfig -> FilePath -> Text -> IO MergeResult
mergeWorktreeIO config wtPath commitMsg = do
  -- Get the branch name from the worktree
  (branchExit, branchOut, _) <- readProcessWithExitCode
    "git"
    [ "-C", wtPath
    , "rev-parse", "--abbrev-ref", "HEAD"
    ]
    ""

  case branchExit of
    ExitFailure code ->
      pure $ MergeError $ "Failed to get branch name (exit " <> T.pack (show code) <> ")"
    ExitSuccess -> do
      let branch = T.strip $ T.pack branchOut

      -- Checkout main in repo root
      (checkoutExit, _, checkoutErr) <- readProcessWithExitCode
        "git"
        ["-C", config.wcRepoRoot, "checkout", "main"]
        ""

      case checkoutExit of
        ExitFailure _ ->
          pure $ MergeError $ "Failed to checkout main: " <> T.pack checkoutErr
        ExitSuccess -> do
          -- Merge the branch
          (mergeExit, _, mergeErr) <- readProcessWithExitCode
            "git"
            [ "-C", config.wcRepoRoot
            , "merge"
            , "--no-ff"
            , "-m", T.unpack commitMsg
            , T.unpack branch
            ]
            ""

          case mergeExit of
            ExitSuccess -> pure MergeSuccess
            ExitFailure _ ->
              if "CONFLICT" `T.isInfixOf` T.pack mergeErr
                then pure $ MergeConflict $ T.pack mergeErr
                else pure $ MergeError $ T.pack mergeErr

-- | Copy specific files from source to destination.
--
-- Handles nested paths by creating parent directories as needed.
-- Uses portable Haskell file operations (not shell commands).
cherryPickFilesIO :: FilePath -> [FilePath] -> FilePath -> IO ()
cherryPickFilesIO srcWorktree files destDir = do
  createDirectoryIfMissing True destDir
  mapM_ copyFile' files
  where
    copyFile' relPath = do
      let srcPath = srcWorktree </> relPath
          dstPath = destDir </> relPath
          dstParentDir = takeDirectory dstPath
      -- Create parent directories for nested paths
      createDirectoryIfMissing True dstParentDir
      copyFile srcPath dstPath

-- | List all worktrees in the repository.
listWorktreesIO :: WorktreeConfig -> IO [(FilePath, Text)]
listWorktreesIO config = do
  (exitCode, stdout, _) <- readProcessWithExitCode
    "git"
    ["-C", config.wcRepoRoot, "worktree", "list", "--porcelain"]
    ""

  case exitCode of
    ExitFailure _ -> pure []
    ExitSuccess -> pure $ parseWorktreeList stdout


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Generate a random hex string of the given length (in hex characters).
randomHex :: Int -> IO String
randomHex n = do
  -- Each byte produces 2 hex chars, so we need ceiling(n/2) bytes
  let byteCount = (n + 1) `div` 2
  bytes <- sequence $ replicate byteCount (randomRIO (0, 255 :: Int))
  pure $ take n $ concatMap toHex2 bytes
  where
    -- Zero-pad to exactly 2 hex digits
    toHex2 b = case showHex b "" of
      [c] -> ['0', c]
      s   -> s

-- | Parse git worktree list --porcelain output.
--
-- Format:
-- @
-- worktree /path/to/worktree
-- HEAD abc123
-- branch refs/heads/branch-name
-- (blank line)
-- @
parseWorktreeList :: String -> [(FilePath, Text)]
parseWorktreeList output = go [] (lines output)
  where
    go acc [] = reverse acc
    go acc ls =
      case parseEntry ls of
        Nothing -> go acc (drop 1 ls)
        Just (entry, rest) -> go (entry : acc) rest

    parseEntry :: [String] -> Maybe ((FilePath, Text), [String])
    parseEntry ls = do
      let (entryLines, rest) = break null ls
          fields = map parseLine entryLines
      path <- lookup "worktree" fields
      -- Branch may be missing for detached HEAD worktrees
      let branch = lookup "branch" fields
          branchName = case branch of
            Just b -> T.pack $ stripRefsHeads b
            Nothing -> "(detached)"
      pure ((path, branchName), drop 1 rest)

    -- Strip "refs/heads/" prefix if present
    stripRefsHeads :: String -> String
    stripRefsHeads s
      | "refs/heads/" `isPrefixOf` s = drop 11 s
      | otherwise = s

    parseLine :: String -> (String, String)
    parseLine line =
      case words line of
        (key:vals) -> (key, unwords vals)
        [] -> ("", "")
