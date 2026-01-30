module Main where

import ExoMonad.Worktree.InterpreterSpec qualified as InterpreterSpec
import ExoMonad.Worktree.ParseSpec qualified as ParseSpec
import Test.Hspec

-- | Run all worktree interpreter tests.
--
-- Includes:
-- - Pure tests (ParseSpec): No external dependencies
-- - Integration tests (InterpreterSpec): Requires git installed
main :: IO ()
main = hspec $ do
  describe "Pure Tests" ParseSpec.spec
  describe "Integration Tests" InterpreterSpec.spec
