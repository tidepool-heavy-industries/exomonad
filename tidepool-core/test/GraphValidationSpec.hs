{-# LANGUAGE OverloadedStrings #-}

-- | Golden tests for Graph DSL compile-time validation.
--
-- These tests verify that:
-- 1. Valid graph constructions compile successfully
-- 2. Invalid graph constructions produce the expected compiler error messages
--
-- Each test compiles a module and checks the result:
-- * Valid modules should compile without errors
-- * Invalid modules should fail with specific error strings
--
-- Tests cover the record-based Servant-style syntax (data MyGraph mode = ...).
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

-- | Assert that compilation succeeds
shouldPass :: FilePath -> Expectation
shouldPass path = do
  (exitCode, stderr) <- compileTestFile path
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> expectationFailure $
      "Expected " ++ path ++ " to compile successfully, but got:\n" ++ stderr

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
spec = do
  -- ════════════════════════════════════════════════════════════════════════════
  -- POSITIVE TESTS: Valid record-based graphs
  -- ════════════════════════════════════════════════════════════════════════════
  describe "Valid graph definitions compile successfully" $ do

    it "simple linear graph compiles" $ do
      shouldPass "test/golden/valid/SimpleLinearRecord.hs"

    it "branching graph with Logic node compiles" $ do
      shouldPass "test/golden/valid/BranchingLogicRecord.hs"

    it "fan-in graph with multiple producers compiles" $ do
      shouldPass "test/golden/valid/FanInGraphRecord.hs"

    it "mixed LLM/Logic graph compiles" $ do
      shouldPass "test/golden/valid/MixedLLMLogicRecord.hs"

    it "self-loop graph with Goto Self compiles" $ do
      shouldPass "test/golden/valid/SelfLoopRecord.hs"

  -- ════════════════════════════════════════════════════════════════════════════
  -- NEGATIVE TESTS: Record-based DSL
  -- ════════════════════════════════════════════════════════════════════════════
  describe "Graph validation error messages (record DSL)" $ do

    it "unreachable field produces clear error" $ do
      "test/golden/UnreachableFieldRecord.hs" `shouldFailWith`
        [ "Graph validation failed: unreachable node"
        , "Field 'orphan' cannot be reached from Entry"
        ]

    it "Logic field without exit path produces error" $ do
      "test/golden/NoExitPathFieldRecord.hs" `shouldFailWith`
        [ "Graph validation failed: Logic node cannot reach Exit"
        , "Field 'loop' has no path to Exit"
        ]

    it "missing Entry field produces clear error" $ do
      "test/golden/MissingEntryRecord.hs" `shouldFailWith`
        [ "Missing Entry field"
        , "WHAT HAPPENED:"
        , "Your graph record has no Entry field."
        ]

    it "missing Exit field produces clear error" $ do
      "test/golden/MissingExitRecord.hs" `shouldFailWith`
        [ "Missing Exit field"
        , "WHAT HAPPENED:"
        , "Your graph record has no Exit field."
        ]

    it "invalid Goto target produces clear error" $ do
      "test/golden/InvalidGotoTargetRecord.hs" `shouldFailWith`
        [ "Goto target \"nonexistent\" not found"
        , "WHAT HAPPENED:"
        , "no field named \"nonexistent\" exists"
        ]

    it "Goto type mismatch produces clear error with expected types" $ do
      "test/golden/GotoTypeMismatchRecord.hs" `shouldFailWith`
        [ "Graph validation failed: Goto payload type mismatch"
        , "Node 'router' sends:"
        , "Goto \"handler\""
        , "But target 'handler' needs:"
        ]
