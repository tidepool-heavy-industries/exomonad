-- | Pure tests for worktree list parser.
--
-- These tests don't require git - they test the pure parsing logic.
module ExoMonad.Worktree.ParseSpec (spec) where

import Data.Text (Text)
import ExoMonad.Effects.Worktree (WorktreePath (..))
import ExoMonad.Worktree.Interpreter (parseWorktreeList)
import Test.Hspec

spec :: Spec
spec = describe "parseWorktreeList" $ do
  it "parses a single worktree entry" $ do
    let output =
          unlines
            [ "worktree /repo/main",
              "HEAD abc123def456",
              "branch refs/heads/main",
              ""
            ]
    parseWorktreeList output
      `shouldBe` [ (WorktreePath "/repo/main", "main")
                 ]

  it "parses multiple worktree entries" $ do
    let output =
          unlines
            [ "worktree /repo/main",
              "HEAD abc123",
              "branch refs/heads/main",
              "",
              "worktree /repo/.worktrees/feature-x",
              "HEAD def456",
              "branch refs/heads/feature-x",
              "",
              "worktree /repo/.worktrees/bugfix-y",
              "HEAD 789abc",
              "branch refs/heads/bugfix-y",
              ""
            ]
    parseWorktreeList output
      `shouldBe` [ (WorktreePath "/repo/main", "main"),
                   (WorktreePath "/repo/.worktrees/feature-x", "feature-x"),
                   (WorktreePath "/repo/.worktrees/bugfix-y", "bugfix-y")
                 ]

  it "handles detached HEAD (no branch line)" $ do
    let output =
          unlines
            [ "worktree /repo/detached",
              "HEAD abc123",
              ""
            ]
    parseWorktreeList output
      `shouldBe` [ (WorktreePath "/repo/detached", "(detached)")
                 ]

  it "handles mixed attached and detached worktrees" $ do
    let output =
          unlines
            [ "worktree /repo/main",
              "HEAD abc123",
              "branch refs/heads/main",
              "",
              "worktree /repo/detached-wt",
              "HEAD def456",
              ""
            ]
    parseWorktreeList output
      `shouldBe` [ (WorktreePath "/repo/main", "main"),
                   (WorktreePath "/repo/detached-wt", "(detached)")
                 ]

  it "returns empty list for empty input" $ do
    parseWorktreeList "" `shouldBe` []

  it "returns empty list for whitespace-only input" $ do
    parseWorktreeList "   \n  \n" `shouldBe` []

  it "strips refs/heads/ prefix from branch names" $ do
    let output =
          unlines
            [ "worktree /repo/test",
              "HEAD 123",
              "branch refs/heads/feature/nested/branch-name",
              ""
            ]
    parseWorktreeList output
      `shouldBe` [ (WorktreePath "/repo/test", "feature/nested/branch-name")
                 ]

  it "handles paths with spaces" $ do
    let output =
          unlines
            [ "worktree /repo/path with spaces",
              "HEAD abc",
              "branch refs/heads/main",
              ""
            ]
    parseWorktreeList output
      `shouldBe` [ (WorktreePath "/repo/path with spaces", "main")
                 ]

  it "handles bare worktree marker" $ do
    -- Bare repos have a "bare" line instead of "branch"
    let output =
          unlines
            [ "worktree /repo/bare.git",
              "bare",
              ""
            ]
    parseWorktreeList output
      `shouldBe` [ (WorktreePath "/repo/bare.git", "(detached)")
                 ]

  it "handles prunable worktree marker" $ do
    -- Prunable worktrees have additional markers
    let output =
          unlines
            [ "worktree /repo/prunable",
              "HEAD abc123",
              "branch refs/heads/old-branch",
              "prunable gitdir file points to non-existent location",
              ""
            ]
    -- Should still parse the worktree correctly
    parseWorktreeList output
      `shouldBe` [ (WorktreePath "/repo/prunable", "old-branch")
                 ]
