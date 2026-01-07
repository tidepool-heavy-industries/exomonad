module Main where

import System.FilePath ((</>))
import Test.Hspec

import Tidepool.Worktree.Executor (WorktreeConfig(..), defaultWorktreeConfig)


-- | Worktree executor tests.
--
-- Note: The core IO functions (createWorktree, deleteWorktree, merge, etc.)
-- require a real git repository for integration testing. Those tests would
-- need to set up a temporary git repo, which is beyond the scope of unit tests.
--
-- These tests focus on the pure configuration logic.
main :: IO ()
main = hspec $ do
  describe "WorktreeConfig" $ do
    describe "defaultWorktreeConfig" $ do
      it "sets repo root to provided path" $ do
        let config = defaultWorktreeConfig "/my/repo"
        wcRepoRoot config `shouldBe` "/my/repo"

      it "defaults worktree directory to .worktrees" $ do
        let config = defaultWorktreeConfig "/my/repo"
        wcWorktreeDir config `shouldBe` ".worktrees"

      it "defaults quiet to True" $ do
        let config = defaultWorktreeConfig "/my/repo"
        wcQuiet config `shouldBe` True

    describe "WorktreeConfig fields" $ do
      it "can be customized via record update" $ do
        let config = (defaultWorktreeConfig "/repo")
              { wcWorktreeDir = "custom-worktrees"
              , wcQuiet = False
              }
        wcRepoRoot config `shouldBe` "/repo"
        wcWorktreeDir config `shouldBe` "custom-worktrees"
        wcQuiet config `shouldBe` False

      it "worktree path is constructed correctly" $ do
        let config = defaultWorktreeConfig "/my/repo"
            expectedPath = wcRepoRoot config </> wcWorktreeDir config </> "branch-name"
        expectedPath `shouldBe` "/my/repo/.worktrees/branch-name"
