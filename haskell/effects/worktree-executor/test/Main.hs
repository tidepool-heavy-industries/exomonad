module Main where

import Test.Hspec

import qualified Tidepool.Worktree.ParseSpec as ParseSpec
import qualified Tidepool.Worktree.ExecutorSpec as ExecutorSpec


-- | Run all worktree executor tests.
--
-- Includes:
-- - Pure tests (ParseSpec): No external dependencies
-- - Integration tests (ExecutorSpec): Requires git installed
main :: IO ()
main = hspec $ do
  describe "Pure Tests" ParseSpec.spec
  describe "Integration Tests" ExecutorSpec.spec
