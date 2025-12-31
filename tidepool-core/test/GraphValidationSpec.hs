{-# LANGUAGE OverloadedStrings #-}

-- | Golden tests for Graph DSL compile-time validation.
--
-- These tests verify that:
-- 1. Valid graphs compile successfully
-- 2. Invalid graph constructions produce the expected compiler error messages
--
-- Each negative test compiles an ill-typed module and checks that specific
-- error strings appear in GHC's output. Positive tests simply verify compilation
-- succeeds.
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

-- | Assert that compilation succeeds
shouldCompile :: FilePath -> Expectation
shouldCompile path = do
  (exitCode, stderr) <- compileTestFile path
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> expectationFailure $
      "Expected compilation to succeed, but got error:\n" ++ stderr

spec :: Spec
spec = do
  -- ════════════════════════════════════════════════════════════════════════════
  -- NEGATIVE TESTS: List-based DSL
  -- ════════════════════════════════════════════════════════════════════════════
  describe "Graph validation error messages (list DSL)" $ do

    it "missing Entry produces clear error" $ do
      "test/golden/MissingEntry.hs" `shouldFailWith`
        [ "Graph validation failed: missing Entry declaration"
        , "Add: Entry :~> YourInputType"
        ]

    it "missing Exit produces clear error" $ do
      "test/golden/MissingExit.hs" `shouldFailWith`
        [ "Graph validation failed: missing Exit declaration"
        , "Add: Exit :<~ YourOutputType"
        ]

    it "unsatisfied Needs produces clear error with node name and type" $ do
      "test/golden/UnsatisfiedNeed.hs" `shouldFailWith`
        [ "Graph validation failed: unsatisfied dependency"
        , "Node 'process' needs type:"
        , "Intent"
        , "Available types"
        ]

    it "multiple unsatisfied Needs produces error" $ do
      "test/golden/MultipleUnsatisfiedNeeds.hs" `shouldFailWith`
        [ "Graph validation failed: unsatisfied dependency"
        , "Intent"
        ]

    it "invalid Goto target produces clear error with node names" $ do
      "test/golden/InvalidGoto.hs" `shouldFailWith`
        [ "Graph validation failed: invalid Goto target"
        , "Node 'router' has:"
        , "Goto \"nonexistent\""
        , "But no node named \"nonexistent\" exists"
        ]

    it "duplicate Schema types produce error" $ do
      "test/golden/DuplicateSchema.hs" `shouldFailWith`
        [ "Graph validation failed: duplicate Schema type"
        , "Response"
        , "ambiguous"
        ]

    it "invalid tool (no ToolDef) produces clear error" $ do
      "test/golden/InvalidTool.hs" `shouldFailWith`
        [ "No instance for"
        , "ToolDef"
        ]

    it "oneOf schema in structured output produces compile error" $ do
      "test/golden/InvalidOneOf.hs" `shouldFailWith`
        [ "Schema error for structured output type"
        , "MyChoice"
        , "oneOf"
        ]

  -- ════════════════════════════════════════════════════════════════════════════
  -- POSITIVE TESTS: List-based DSL
  -- ════════════════════════════════════════════════════════════════════════════
  describe "Valid graphs compile (list DSL)" $ do

    it "linear graph compiles" $ do
      shouldCompile "test/golden/valid/LinearGraph.hs"

    it "branching graph with Goto compiles" $ do
      shouldCompile "test/golden/valid/BranchingGraph.hs"

    it "fan-in graph (multiple inputs) compiles" $ do
      shouldCompile "test/golden/valid/FanInGraph.hs"

    it "Goto Exit graph compiles" $ do
      shouldCompile "test/golden/valid/GotoExitGraph.hs"
