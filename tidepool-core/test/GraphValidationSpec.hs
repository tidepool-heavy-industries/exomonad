{-# LANGUAGE OverloadedStrings #-}

-- | Golden tests for Graph DSL compile-time validation.
--
-- These tests verify that invalid graph constructions produce the expected
-- compiler error messages. Each test compiles an ill-typed module and checks
-- that specific error strings appear in GHC's output.
--
-- Tests cover both the list-based syntax (Graph '[...]) and the record-based
-- Servant-style syntax (data MyGraph mode = ...).
module GraphValidationSpec (spec) where

import Test.Hspec
import System.Process
import System.Exit (ExitCode(..))
import Data.List (isInfixOf)

-- | Run GHC on a test file and return (exit code, stderr)
compileTestFile :: FilePath -> IO (ExitCode, String)
compileTestFile path = do
  -- Use cabal exec to get correct package database
  (exitCode, _stdout, stderr) <- readProcessWithExitCode "cabal"
    [ "exec", "ghc", "--"
    , "-fno-code"          -- don't generate code, just typecheck
    , "-package", "tidepool-core"
    , path
    ] ""
  pure (exitCode, stderr)

-- | Assert that compilation fails and stderr contains expected strings
shouldFailWith :: FilePath -> [String] -> Expectation
shouldFailWith path expectedErrors = do
  (exitCode, stderr) <- compileTestFile path
  exitCode `shouldBe` ExitFailure 1
  forM_ expectedErrors $ \expected ->
    stderr `shouldSatisfy` (expected `isInfixOf`)
  where
    forM_ xs f = mapM_ f xs

spec :: Spec
spec = describe "Graph validation error messages" $ do

  -- ══════════════════════════════════════════════════════════════════════════
  -- Record-based (Servant-style) structural validation
  -- ══════════════════════════════════════════════════════════════════════════

  describe "Record-based structural validation" $ do

    it "unreachable field produces clear error" $ do
      "test/golden/UnreachableFieldRecord.hs" `shouldFailWith`
        [ "Graph validation failed: unreachable node"
        , "Field 'orphan' cannot be reached from Entry"
        ]

    it "Logic field without exit path produces clear error" $ do
      "test/golden/NoExitPathFieldRecord.hs" `shouldFailWith`
        [ "Graph validation failed: Logic node cannot reach Exit"
        , "Field 'loop' has no path to Exit"
        ]
