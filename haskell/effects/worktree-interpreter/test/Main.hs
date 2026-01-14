module Main where

import Test.Hspec

import qualified Tidepool.Worktree.ParseSpec as ParseSpec
import qualified Tidepool.Worktree.InterpreterSpec as InterpreterSpec


-- | Run all worktree interpreter tests.
--
-- Includes:
-- - Pure tests (ParseSpec): No external dependencies
-- - Integration tests (InterpreterSpec): Requires git installed
main :: IO ()
main = hspec $ do
  describe "Pure Tests" ParseSpec.spec
  describe "Integration Tests" InterpreterSpec.spec
