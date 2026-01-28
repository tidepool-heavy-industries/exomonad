-- | Worktree effect interpreter - git worktree management.
--
-- Implements Worktree effect by calling git commands.
-- Enables parallel agent execution in isolated working directories.
--
-- = Usage
--
-- @
-- import ExoMonad.Worktree.Interpreter (runWorktreeIO)
-- import ExoMonad.Effects.Worktree
--
-- main = runM $ runWorktreeIO config $ do
--   result <- createWorktree (WorktreeSpec "my-feature" Nothing)
--   case result of
--     Right path -> ... -- use path
--     Left err -> ... -- handle error
-- @
--
-- = Requirements
--
-- Requires git to be installed and the working directory to be inside a git repo.
module ExoMonad.Worktree.Interpreter
  ( -- * Interpreter
    runWorktreeIO

    -- * Configuration
  , WorktreeConfig(..)
  , defaultWorktreeConfig

    -- * Testing Utilities
  , parseWorktreeList
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

import ExoMonad.Effects.Worktree
  ( Worktree(..)
  , WorktreePath(..)
  , WorktreeSpec(..)
  , WorktreeError(..)
  , MergeResult(..)
  )


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for Worktree interpreter.
data WorktreeConfig = WorktreeConfig
  { repoRoot :: FilePath
    -- ^ Root of the git repository.
  , worktreeDir :: FilePath
    -- ^ Directory where worktrees are created (relative to repo root).
    -- Defaults to ".worktrees".
  , targetBranch :: String
    -- ^ Branch to merge worktrees into. Defaults to "main".
    -- Set to "master" for repos using traditional naming.
  , quiet :: Bool
    -- ^ Suppress verbose output.
  }
  deriving (Show, Eq)

-- | Default configuration.
defaultWorktreeConfig :: FilePath -> WorktreeConfig
defaultWorktreeConfig repoRoot = WorktreeConfig
  { repoRoot = repoRoot
  , worktreeDir = ".worktrees"
  , targetBranch = "main"
  , quiet = True
  }


-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run Worktree effects using git commands.
--
-- This interpreter shells out to git for each operation.
-- All operations return Either, so errors are recoverable.
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
createWorktreeIO :: WorktreeConfig -> WorktreeSpec -> IO (Either WorktreeError WorktreePath)
createWorktreeIO config spec = do
  result <- try @SomeException $ do
    -- Generate branch name
    branchName <- case spec.branchName of
      Just b -> pure $ T.unpack b
      Nothing -> do
        suffix <- randomHex 6
        pure $ T.unpack spec.baseName <> "-" <> suffix

    -- Determine worktree path
    let wtPath = case spec.path of
          Just p -> p
          Nothing ->
            let wtDir = config.repoRoot </> config.worktreeDir
            in wtDir </> branchName
        baseBranch = maybe "HEAD" T.unpack spec.fromBranch
        wtDirParent = takeDirectory wtPath

    -- Ensure parent directory for worktree exists
    createDirectoryIfMissing True wtDirParent

    -- Create the worktree with a new branch
    (exitCode, _stdout, stderr) <- readProcessWithExitCode
      "git"
      [ "-C", config.repoRoot
      , "worktree", "add"
      , "-b", branchName  -- Create new branch
      , wtPath
      , baseBranch        -- Start from this ref
      ]
      ""

    case exitCode of
      ExitSuccess -> pure $ Right $ WorktreePath wtPath
      ExitFailure code -> pure $ Left WorktreeGitError
        { command = "worktree add"
        , exitCode = code
        , stderr = T.pack stderr
        }

  case result of
    Left (e :: SomeException) -> pure $ Left WorktreeGitError
      { command = "worktree add"
      , exitCode = -1
      , stderr = T.pack (show e)
      }
    Right inner -> pure inner

-- | Delete a worktree and its branch.
deleteWorktreeIO :: WorktreeConfig -> WorktreePath -> IO (Either WorktreeError ())
deleteWorktreeIO config (WorktreePath wtPath) = do
  result <- try @SomeException $ do
    -- Check if worktree exists
    exists <- doesDirectoryExist wtPath
    if not exists
      then pure $ Right ()  -- Silently succeed if already gone
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
          [ "-C", config.repoRoot
          , "worktree", "remove"
          , "--force"  -- Force in case of uncommitted changes
          , wtPath
          ]
          ""

        case exitCode of
          ExitSuccess -> do
            -- Delete the branch (best effort, ignore errors)
            deleteBranch config branchName
            pure $ Right ()
          ExitFailure _ -> do
            -- Fallback: remove directory manually if git command fails
            rmResult <- try @SomeException $ removeDirectoryRecursive wtPath
            case rmResult of
              Left _ -> pure $ Left WorktreeGitError
                { command = "worktree remove"
                , exitCode = 1
                , stderr = T.pack stderr
                }
              Right () -> do
                -- Directory removed, delete branch (best effort)
                deleteBranch config branchName
                pure $ Right ()

  case result of
    Left (e :: SomeException) -> pure $ Left WorktreeGitError
      { command = "worktree remove"
      , exitCode = -1
      , stderr = T.pack (show e)
      }
    Right inner -> pure inner

-- | Helper to delete a branch (best effort, ignores errors).
deleteBranch :: WorktreeConfig -> Maybe Text -> IO ()
deleteBranch _ Nothing = pure ()
deleteBranch config (Just branch) = do
  _ <- readProcessWithExitCode
    "git"
    [ "-C", config.repoRoot
    , "branch", "-D"
    , T.unpack branch
    ]
    ""
  pure ()

-- | Merge a worktree's branch back to main.
mergeWorktreeIO :: WorktreeConfig -> WorktreePath -> Text -> IO (Either WorktreeError MergeResult)
mergeWorktreeIO config (WorktreePath wtPath) commitMsg = do
  result <- try @SomeException $ do
    -- Get the branch name from the worktree
    (branchExit, branchOut, branchErr) <- readProcessWithExitCode
      "git"
      [ "-C", wtPath
      , "rev-parse", "--abbrev-ref", "HEAD"
      ]
      ""

    case branchExit of
      ExitFailure code -> pure $ Left WorktreeGitError
        { command = "rev-parse --abbrev-ref HEAD"
        , exitCode = code
        , stderr = T.pack branchErr
        }
      ExitSuccess -> do
        let branch = T.strip $ T.pack branchOut
            targetBranch = config.targetBranch

        -- Checkout target branch in repo root
        (checkoutExit, _, checkoutErr) <- readProcessWithExitCode
          "git"
          ["-C", config.repoRoot, "checkout", targetBranch]
          ""

        case checkoutExit of
          ExitFailure code -> pure $ Left WorktreeGitError
            { command = T.pack $ "checkout " <> targetBranch
            , exitCode = code
            , stderr = T.pack checkoutErr
            }
          ExitSuccess -> do
            -- Merge the branch
            (mergeExit, _, mergeErr) <- readProcessWithExitCode
              "git"
              [ "-C", config.repoRoot
              , "merge"
              , "--no-ff"
              , "-m", T.unpack commitMsg
              , T.unpack branch
              ]
              ""

            case mergeExit of
              ExitSuccess -> pure $ Right MergeSuccess
              ExitFailure code ->
                if "CONFLICT" `T.isInfixOf` T.pack mergeErr
                  then pure $ Right $ MergeConflict $ T.pack mergeErr
                  else pure $ Left WorktreeGitError
                    { command = "merge"
                    , exitCode = code
                    , stderr = T.pack mergeErr
                    }

  case result of
    Left (e :: SomeException) -> pure $ Left WorktreeGitError
      { command = "merge"
      , exitCode = -1
      , stderr = T.pack (show e)
      }
    Right inner -> pure inner

-- | Copy specific files from source to destination.
--
-- Handles nested paths by creating parent directories as needed.
-- Uses portable Haskell file operations (not shell commands).
cherryPickFilesIO :: WorktreePath -> [FilePath] -> FilePath -> IO (Either WorktreeError ())
cherryPickFilesIO (WorktreePath srcWorktree) files destDir = do
  result <- try @SomeException $ do
    createDirectoryIfMissing True destDir
    mapM_ copyFile' files
    pure ()

  case result of
    Left (e :: SomeException) ->
      -- Find which file failed (best effort - report first file if unknown)
      let failedFile = case files of
            (f:_) -> f
            [] -> ""
      in pure $ Left WorktreeFileCopyError
        { srcPath = srcWorktree </> failedFile
        , destPath = destDir </> failedFile
        , reason = T.pack (show e)
        }
    Right () -> pure $ Right ()
  where
    copyFile' relPath = do
      let srcPath = srcWorktree </> relPath
          dstPath = destDir </> relPath
          dstParentDir = takeDirectory dstPath
      -- Create parent directories for nested paths
      createDirectoryIfMissing True dstParentDir
      copyFile srcPath dstPath

-- | List all worktrees in the repository.
listWorktreesIO :: WorktreeConfig -> IO (Either WorktreeError [(WorktreePath, Text)])
listWorktreesIO config = do
  result <- try @SomeException $ readProcessWithExitCode
    "git"
    ["-C", config.repoRoot, "worktree", "list", "--porcelain"]
    ""

  case result of
    Left (e :: SomeException) -> pure $ Left WorktreeGitError
      { command = "worktree list"
      , exitCode = -1
      , stderr = T.pack (show e)
      }
    Right (exitCode, stdout, stderr) ->
      case exitCode of
        ExitFailure code -> pure $ Left WorktreeGitError
          { command = "worktree list"
          , exitCode = code
          , stderr = T.pack stderr
          }
        ExitSuccess -> pure $ Right $ parseWorktreeList stdout


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
--
-- Exported for testing.
parseWorktreeList :: String -> [(WorktreePath, Text)]
parseWorktreeList output = go [] (lines output)
  where
    go acc [] = reverse acc
    go acc ls =
      case parseEntry ls of
        Nothing -> go acc (drop 1 ls)
        Just (entry, rest) -> go (entry : acc) rest

    parseEntry :: [String] -> Maybe ((WorktreePath, Text), [String])
    parseEntry ls = do
      let (entryLines, rest) = break null ls
          fields = map parseLine entryLines
      path <- lookup "worktree" fields
      -- Branch may be missing for detached HEAD worktrees
      let branch = lookup "branch" fields
          branchName = case branch of
            Just b -> T.pack $ stripRefsHeads b
            Nothing -> "(detached)"
      pure ((WorktreePath path, branchName), drop 1 rest)

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
