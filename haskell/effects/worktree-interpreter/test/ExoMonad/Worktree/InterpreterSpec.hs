-- | Integration tests for worktree interpreter.
--
-- These tests require git to be installed and create temporary repositories.
module ExoMonad.Worktree.InterpreterSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Freer (Eff, runM, sendM)
import Data.Either (isLeft, isRight)
import Data.OpenUnion (LastMember)
import Data.Text qualified as T
import ExoMonad.Effects.Worktree
  ( MergeResult (..),
    WorktreeError (..),
    WorktreePath (..),
    WorktreeSpec (..),
    cherryPickFiles,
    createWorktree,
    deleteWorktree,
    listWorktrees,
    mergeWorktree,
    withWorktree,
  )
import ExoMonad.Worktree.Interpreter (defaultWorktreeConfig, runWorktreeIO)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    removeDirectoryRecursive,
  )
import System.FilePath ((</>))
import System.Process (getCurrentPid, readProcessWithExitCode)
import Test.Hspec

-- | Create a temporary git repository for testing.
--
-- The repo is initialized with an empty commit so branches can be created.
withTempGitRepo :: (FilePath -> IO a) -> IO a
withTempGitRepo action = bracket setup teardown action
  where
    setup = do
      tmpDir <- getTemporaryDirectory
      pid <- getCurrentPid
      let testDir = tmpDir </> "worktree-test-" <> show pid
      createDirectoryIfMissing True testDir

      -- Initialize git repo with main as default branch
      void $ readProcessWithExitCode "git" ["-C", testDir, "init", "-b", "main"] ""

      -- Configure git user (required for commits)
      void $
        readProcessWithExitCode
          "git"
          ["-C", testDir, "config", "user.email", "test@test.com"]
          ""
      void $
        readProcessWithExitCode
          "git"
          ["-C", testDir, "config", "user.name", "Test"]
          ""

      -- Create initial commit (required for worktrees)
      void $
        readProcessWithExitCode
          "git"
          ["-C", testDir, "commit", "--allow-empty", "-m", "Initial commit"]
          ""

      pure testDir

    teardown dir = do
      exists <- doesDirectoryExist dir
      if exists
        then removeDirectoryRecursive dir
        else pure ()

spec :: Spec
spec = describe "Worktree Interpreter" $ do
  describe "createWorktree" $ do
    it "creates a worktree directory that exists" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      result <-
        runM $
          runWorktreeIO config $
            createWorktree (WorktreeSpec "test-wt" Nothing Nothing Nothing)

      result `shouldSatisfy` isRight
      case result of
        Right (WorktreePath path) -> do
          exists <- doesDirectoryExist path
          exists `shouldBe` True
        Left _ -> expectationFailure "Expected Right"

    it "creates worktrees with unique paths" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      (result1, result2) <- runM $ runWorktreeIO config $ do
        r1 <- createWorktree (WorktreeSpec "same-name" Nothing Nothing Nothing)
        r2 <- createWorktree (WorktreeSpec "same-name" Nothing Nothing Nothing)
        pure (r1, r2)

      result1 `shouldSatisfy` isRight
      result2 `shouldSatisfy` isRight

      case (result1, result2) of
        (Right path1, Right path2) ->
          path1 `shouldNotBe` path2 -- Different random suffixes
        _ -> expectationFailure "Expected both to be Right"

    it "returns error for invalid base branch" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      result <-
        runM $
          runWorktreeIO config $
            createWorktree (WorktreeSpec "test" (Just "nonexistent-branch") Nothing Nothing)

      result `shouldSatisfy` isLeft
      case result of
        Left (WorktreeGitError {command}) ->
          command `shouldBe` "worktree add"
        _ -> expectationFailure "Expected WorktreeGitError"

  describe "deleteWorktree" $ do
    it "removes an existing worktree" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      result <- runM $ runWorktreeIO config $ do
        createResult <- createWorktree (WorktreeSpec "to-delete" Nothing Nothing Nothing)
        case createResult of
          Left err -> pure $ Left err
          Right path -> do
            deleteResult <- deleteWorktree path
            pure $ fmap (const path) deleteResult

      result `shouldSatisfy` isRight
      case result of
        Right (WorktreePath path) -> do
          exists <- doesDirectoryExist path
          exists `shouldBe` False
        Left _ -> expectationFailure "Expected Right"

    it "succeeds silently for non-existent worktree" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      result <-
        runM $
          runWorktreeIO config $
            deleteWorktree (WorktreePath "/nonexistent/path")

      result `shouldBe` Right ()

  describe "listWorktrees" $ do
    it "lists created worktrees" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      result <- runM $ runWorktreeIO config $ do
        _ <- createWorktree (WorktreeSpec "list-test" Nothing Nothing Nothing)
        listWorktrees

      result `shouldSatisfy` isRight
      case result of
        Right wts -> do
          -- Should have at least 2: main repo + our worktree
          length wts `shouldSatisfy` (>= 2)
          -- One should contain "list-test"
          let containsListTest (WorktreePath p, _) = "list-test" `T.isInfixOf` T.pack p
          any containsListTest wts `shouldBe` True
        Left _ -> expectationFailure "Expected Right"

  describe "withWorktree (bracket)" $ do
    it "cleans up worktree after action completes" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      (result, capturedPath) <- runM $ runWorktreeIO config $ do
        res <- withWorktree (WorktreeSpec "bracket-test" Nothing Nothing Nothing) $ \path -> do
          pure path
        case res of
          Right path -> pure (res, Just path)
          Left _ -> pure (res, Nothing)

      result `shouldSatisfy` isRight
      case capturedPath of
        Just (WorktreePath path) -> do
          exists <- doesDirectoryExist path
          exists `shouldBe` False -- Should be cleaned up
        Nothing -> expectationFailure "Expected to capture path"

    it "cleans up even if action returns error value" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      (_, capturedPath) <- runM $ runWorktreeIO config $ do
        res <- withWorktree (WorktreeSpec "error-test" Nothing Nothing Nothing) $ \path -> do
          -- Return an error-like value (but don't throw)
          pure (path, "simulated error" :: String)
        case res of
          Right (path, _) -> pure (res, Just path)
          Left _ -> pure (res, Nothing)

      case capturedPath of
        Just (WorktreePath path) -> do
          exists <- doesDirectoryExist path
          exists `shouldBe` False -- Should still be cleaned up
        Nothing -> expectationFailure "Expected to capture path"

    it "returns creation error without running action" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      actionRan <- runM $ runWorktreeIO config $ do
        res <- withWorktree (WorktreeSpec "fail" (Just "bad-branch") Nothing Nothing) $ \_ -> do
          pure True -- This should never run
        case res of
          Right ran -> pure ran
          Left _ -> pure False

      actionRan `shouldBe` False

  describe "mergeWorktree" $ do
    it "merges worktree changes back to main" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      result <- runM $ runWorktreeIO config $ do
        -- Create a worktree
        createResult <- createWorktree (WorktreeSpec "merge-test" Nothing Nothing Nothing)
        case createResult of
          Left err -> pure $ Left err
          Right wtPath@(WorktreePath wtDir) -> do
            -- Create a file and commit in worktree (via IO)
            liftIO $ do
              writeFile (wtDir </> "test.txt") "test content"
              void $ readProcessWithExitCode "git" ["-C", wtDir, "add", "test.txt"] ""
              void $ readProcessWithExitCode "git" ["-C", wtDir, "commit", "-m", "Add test file"] ""
            -- Merge back to main
            mergeWorktree wtPath "Merge test changes"

      result `shouldSatisfy` isRight
      case result of
        Right MergeSuccess -> pure ()
        Right (MergeConflict _) -> expectationFailure "Expected MergeSuccess, got MergeConflict"
        Left err -> expectationFailure $ "Expected Right, got: " <> show err

    it "returns error for non-existent worktree" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      result <-
        runM $
          runWorktreeIO config $
            mergeWorktree (WorktreePath "/nonexistent/worktree") "Test merge"

      result `shouldSatisfy` isLeft

  describe "cherryPickFiles" $ do
    it "copies files from worktree to destination" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
          destDir = repoDir </> "cherry-pick-dest"
      createDirectoryIfMissing True destDir

      result <- runM $ runWorktreeIO config $ do
        -- Create a worktree
        createResult <- createWorktree (WorktreeSpec "cherry-test" Nothing Nothing Nothing)
        case createResult of
          Left err -> pure $ Left err
          Right wtPath@(WorktreePath wtDir) -> do
            -- Create test files in worktree
            liftIO $ do
              createDirectoryIfMissing True (wtDir </> "subdir")
              writeFile (wtDir </> "file1.txt") "content1"
              writeFile (wtDir </> "subdir" </> "file2.txt") "content2"
            -- Cherry-pick files to destination
            cherryPickFiles wtPath ["file1.txt", "subdir/file2.txt"] destDir

      result `shouldSatisfy` isRight
      -- Verify files were copied
      file1Exists <- doesFileExist (destDir </> "file1.txt")
      file2Exists <- doesFileExist (destDir </> "subdir" </> "file2.txt")
      file1Exists `shouldBe` True
      file2Exists `shouldBe` True

    it "returns error for non-existent source files" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
          destDir = repoDir </> "cherry-pick-dest2"
      createDirectoryIfMissing True destDir

      result <- runM $ runWorktreeIO config $ do
        createResult <- createWorktree (WorktreeSpec "cherry-err" Nothing Nothing Nothing)
        case createResult of
          Left err -> pure $ Left err
          Right wtPath -> do
            -- Try to cherry-pick non-existent file
            cherryPickFiles wtPath ["nonexistent.txt"] destDir

      result `shouldSatisfy` isLeft
      case result of
        Left (WorktreeFileCopyError {}) -> pure ()
        Left err -> expectationFailure $ "Expected WorktreeFileCopyError, got: " <> show err
        Right () -> expectationFailure "Expected Left, got Right"

    it "succeeds with empty file list" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
          destDir = repoDir </> "cherry-pick-empty"
      createDirectoryIfMissing True destDir

      result <- runM $ runWorktreeIO config $ do
        createResult <- createWorktree (WorktreeSpec "cherry-empty" Nothing Nothing Nothing)
        case createResult of
          Left err -> pure $ Left err
          Right wtPath -> cherryPickFiles wtPath [] destDir

      result `shouldBe` Right ()

-- | Helper to lift IO into Eff
liftIO :: (LastMember IO effs) => IO a -> Eff effs a
liftIO = sendM
