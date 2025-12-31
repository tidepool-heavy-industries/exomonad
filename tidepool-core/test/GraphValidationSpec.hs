{-# LANGUAGE OverloadedStrings #-}

-- | Golden tests for Graph DSL compile-time validation.
--
-- These tests verify that invalid graph constructions produce the expected
-- compiler error messages. Each test compiles an ill-typed module and checks
-- that specific error strings appear in GHC's output.
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
    , "-package", "tidepool-sketch"
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
      , "But no node provides it via Schema"
      ]

  it "invalid Goto target produces clear error with node names" $ do
    "test/golden/InvalidGoto.hs" `shouldFailWith`
      [ "Graph validation failed: invalid Goto target"
      , "Node 'router' has:"
      , "Goto \"nonexistent\""
      , "But no node named \"nonexistent\" exists"
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
