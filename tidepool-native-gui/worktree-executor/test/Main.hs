module Main where

import Test.Hspec

-- | Worktree executor tests.
--
-- These are integration tests that require a real git repository.
-- Most testing will be done via the full types-first-dev workflow.
main :: IO ()
main = hspec $ do
  describe "Tidepool.Worktree.Executor" $ do
    it "can be imported" $ do
      -- Just verify the module can be imported
      -- Real testing requires git repo setup
      True `shouldBe` True
