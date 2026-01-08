-- | Integration tests for worktree executor.
--
-- These tests require git to be installed and create temporary repositories.
module Tidepool.Worktree.ExecutorSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Freer (runM)
import Data.Either (isRight, isLeft)
import Data.Text qualified as T
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , removeDirectoryRecursive
  , getTemporaryDirectory
  )
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode, getCurrentPid)
import Test.Hspec

import Tidepool.Effects.Worktree
  ( WorktreePath(..)
  , WorktreeSpec(..)
  , WorktreeError(..)
  , createWorktree
  , deleteWorktree
  , listWorktrees
  , withWorktree
  )
import Tidepool.Worktree.Executor (runWorktreeIO, defaultWorktreeConfig)


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

      -- Initialize git repo
      void $ readProcessWithExitCode "git" ["-C", testDir, "init"] ""

      -- Configure git user (required for commits)
      void $ readProcessWithExitCode "git"
        ["-C", testDir, "config", "user.email", "test@test.com"] ""
      void $ readProcessWithExitCode "git"
        ["-C", testDir, "config", "user.name", "Test"] ""

      -- Create initial commit (required for worktrees)
      void $ readProcessWithExitCode "git"
        ["-C", testDir, "commit", "--allow-empty", "-m", "Initial commit"] ""

      pure testDir

    teardown dir = do
      exists <- doesDirectoryExist dir
      if exists
        then removeDirectoryRecursive dir
        else pure ()


spec :: Spec
spec = describe "Worktree Executor" $ do

  describe "createWorktree" $ do

    it "creates a worktree directory that exists" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      result <- runM $ runWorktreeIO config $
        createWorktree (WorktreeSpec "test-wt" Nothing)

      result `shouldSatisfy` isRight
      case result of
        Right (WorktreePath path) -> do
          exists <- doesDirectoryExist path
          exists `shouldBe` True
        Left _ -> expectationFailure "Expected Right"

    it "creates worktrees with unique paths" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      (result1, result2) <- runM $ runWorktreeIO config $ do
        r1 <- createWorktree (WorktreeSpec "same-name" Nothing)
        r2 <- createWorktree (WorktreeSpec "same-name" Nothing)
        pure (r1, r2)

      result1 `shouldSatisfy` isRight
      result2 `shouldSatisfy` isRight

      case (result1, result2) of
        (Right path1, Right path2) ->
          path1 `shouldNotBe` path2  -- Different random suffixes
        _ -> expectationFailure "Expected both to be Right"

    it "returns error for invalid base branch" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      result <- runM $ runWorktreeIO config $
        createWorktree (WorktreeSpec "test" (Just "nonexistent-branch"))

      result `shouldSatisfy` isLeft
      case result of
        Left (WorktreeGitError {wgeCommand}) ->
          wgeCommand `shouldBe` "worktree add"
        _ -> expectationFailure "Expected WorktreeGitError"


  describe "deleteWorktree" $ do

    it "removes an existing worktree" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      result <- runM $ runWorktreeIO config $ do
        createResult <- createWorktree (WorktreeSpec "to-delete" Nothing)
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
      result <- runM $ runWorktreeIO config $
        deleteWorktree (WorktreePath "/nonexistent/path")

      result `shouldBe` Right ()


  describe "listWorktrees" $ do

    it "lists created worktrees" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      result <- runM $ runWorktreeIO config $ do
        _ <- createWorktree (WorktreeSpec "list-test" Nothing)
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
        res <- withWorktree (WorktreeSpec "bracket-test" Nothing) $ \path -> do
          pure path
        case res of
          Right path -> pure (res, Just path)
          Left _ -> pure (res, Nothing)

      result `shouldSatisfy` isRight
      case capturedPath of
        Just (WorktreePath path) -> do
          exists <- doesDirectoryExist path
          exists `shouldBe` False  -- Should be cleaned up
        Nothing -> expectationFailure "Expected to capture path"

    it "cleans up even if action returns error value" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      (_, capturedPath) <- runM $ runWorktreeIO config $ do
        res <- withWorktree (WorktreeSpec "error-test" Nothing) $ \path -> do
          -- Return an error-like value (but don't throw)
          pure (path, "simulated error" :: String)
        case res of
          Right (path, _) -> pure (res, Just path)
          Left _ -> pure (res, Nothing)

      case capturedPath of
        Just (WorktreePath path) -> do
          exists <- doesDirectoryExist path
          exists `shouldBe` False  -- Should still be cleaned up
        Nothing -> expectationFailure "Expected to capture path"

    it "returns creation error without running action" $ withTempGitRepo $ \repoDir -> do
      let config = defaultWorktreeConfig repoDir
      actionRan <- runM $ runWorktreeIO config $ do
        res <- withWorktree (WorktreeSpec "fail" (Just "bad-branch")) $ \_ -> do
          pure True  -- This should never run
        case res of
          Right ran -> pure ran
          Left _ -> pure False

      actionRan `shouldBe` False
