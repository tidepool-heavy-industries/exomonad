{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | E2E tests for CLI graph integration.
--
-- Tests the full flow: CLI args → parse → graph execution → formatted output.
--
-- This validates that:
--
-- 1. deriveCLIParser generates correct parser from Haddock-documented types
-- 2. Graph definition with mode-parameterized records
-- 3. Logic handler returning GotoChoice
-- 4. DispatchGoto executing the graph
-- 5. Output formatting (JSON and text)
-- 6. Full CLI → Graph → Output pipeline
module CLIGraphSpec (spec) where

import Control.Monad.Freer (run)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Options.Applicative
import Test.Hspec

import CLIGraphTypes (CounterInput(..), CounterOutput(..))
import ExoMonad.Graph.CLI
  ( deriveCLIParser
  , formatOutput
  , OutputFormat(..)
  , runGraphCLIPure
  )
import ExoMonad.Graph.Interpret (runGraph)
import ExoMonad.Graph.Generic (GraphMode(..), type (:-), AsHandler)
import ExoMonad.Graph.Goto (gotoExit, Goto)
import ExoMonad.Graph.Types (Input, UsesEffects, type (:@))
import qualified ExoMonad.Graph.Generic as G
import qualified ExoMonad.Graph.Types as Types (Exit)

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Counter graph: Entry(CounterInput) → compute(logic) → Exit(CounterOutput)
--
-- This is a minimal logic-only graph that:
-- 1. Takes counter parameters as input
-- 2. Computes: finalValue = startValue + (increment * times)
-- 3. Returns CounterOutput with the result
data CounterGraph mode = CounterGraph
  { cgEntry   :: mode :- G.EntryNode CounterInput
  , cgCompute :: mode :- G.LogicNode :@ Input CounterInput
                    :@ UsesEffects '[Goto Types.Exit CounterOutput]
  , cgExit    :: mode :- G.ExitNode CounterOutput
  }
  deriving Generic

-- | Handlers for the counter graph (pure logic, no effects).
counterHandlers :: CounterGraph (AsHandler '[])
counterHandlers = CounterGraph
  { cgEntry   = ()
  , cgCompute = \input -> pure $ gotoExit CounterOutput
      { finalValue = input.startValue + (input.increment * input.times)
      , operationsPerformed = input.times
      }
  , cgExit    = ()
  }

-- | Execute the counter graph.
--
-- Uses runGraph which automatically finds the entry handler via FindEntryHandler
-- and dispatches through the graph until Exit is reached.
runCounterGraph :: CounterInput -> CounterOutput
runCounterGraph input = run $ runGraph counterHandlers input

-- | Compile-time test that runGraphCLIPure type constraints resolve correctly.
--
-- This function is never called (prefixed with _), but if it compiles,
-- it proves that the type machinery works for our CounterGraph.
_counterCLI :: IO ()
_counterCLI = runGraphCLIPure
  "Counter graph CLI"
  counterParser
  counterHandlers

-- ════════════════════════════════════════════════════════════════════════════
-- CLI PARSER (TH-derived)
-- ════════════════════════════════════════════════════════════════════════════

-- | Parser derived from CounterInput's Haddock documentation.
counterParser :: Parser CounterInput
counterParser = $(deriveCLIParser ''CounterInput)

-- | Helper to parse arguments, returning either error or result.
parseArgs :: Parser a -> [String] -> Either String a
parseArgs p args =
  case execParserPure defaultPrefs (info p mempty) args of
    Success a -> Right a
    Failure f -> Left $ show f
    CompletionInvoked _ -> Left "completion invoked"

-- ════════════════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  describe "E2E: Logic-only graph with CLI" $ do

    -- ─────────────────────────────────────────────────────────────────────────
    -- CLI Parsing Tests
    -- ─────────────────────────────────────────────────────────────────────────

    describe "CLI parsing" $ do
      it "parses all required flags" $ do
        let result = parseArgs counterParser
              ["--start-value", "10", "--increment", "5", "--times", "3"]
        result `shouldBe` Right CounterInput
          { startValue = 10
          , increment = 5
          , times = 3
          }

      it "parses zero values" $ do
        let result = parseArgs counterParser
              ["--start-value", "0", "--increment", "0", "--times", "0"]
        result `shouldBe` Right CounterInput
          { startValue = 0
          , increment = 0
          , times = 0
          }

      it "parses negative values" $ do
        let result = parseArgs counterParser
              ["--start-value", "-10", "--increment", "-5", "--times", "3"]
        result `shouldBe` Right CounterInput
          { startValue = -10
          , increment = -5
          , times = 3
          }

      it "fails on missing required flag" $ do
        let result = parseArgs counterParser ["--start-value", "10"]
            isLeft (Left _) = True
            isLeft _ = False
        result `shouldSatisfy` isLeft

    -- ─────────────────────────────────────────────────────────────────────────
    -- Graph Execution Tests
    -- ─────────────────────────────────────────────────────────────────────────

    describe "Graph execution" $ do
      it "computes correct result" $ do
        let input = CounterInput { startValue = 10, increment = 5, times = 3 }
            result = runCounterGraph input
        result.finalValue `shouldBe` 25  -- 10 + (5 * 3)
        result.operationsPerformed `shouldBe` 3

      it "handles zero iterations" $ do
        let input = CounterInput { startValue = 100, increment = 50, times = 0 }
            result = runCounterGraph input
        result.finalValue `shouldBe` 100  -- 100 + (50 * 0)
        result.operationsPerformed `shouldBe` 0

      it "handles negative increment" $ do
        let input = CounterInput { startValue = 10, increment = -3, times = 4 }
            result = runCounterGraph input
        result.finalValue `shouldBe` (-2)  -- 10 + (-3 * 4)
        result.operationsPerformed `shouldBe` 4

    -- ─────────────────────────────────────────────────────────────────────────
    -- Full Pipeline Tests (CLI → Graph → Output)
    -- ─────────────────────────────────────────────────────────────────────────

    describe "Full pipeline" $ do
      it "parses CLI args and executes graph" $ do
        let args = ["--start-value", "10", "--increment", "5", "--times", "3"]
        case parseArgs counterParser args of
          Right input -> do
            let result = runCounterGraph input
            result.finalValue `shouldBe` 25
            result.operationsPerformed `shouldBe` 3
          Left err -> expectationFailure err

      it "formats output as JSON" $ do
        let output = CounterOutput 25 3
            jsonOutput = formatOutput FormatJSON output
        -- Verify JSON contains expected values
        jsonOutput `shouldSatisfy` T.isInfixOf "25"
        jsonOutput `shouldSatisfy` T.isInfixOf "finalValue"
        jsonOutput `shouldSatisfy` T.isInfixOf "3"
        jsonOutput `shouldSatisfy` T.isInfixOf "operationsPerformed"

      it "formats output as text (via Show)" $ do
        let output = CounterOutput 25 3
        formatOutput FormatText output `shouldBe` T.pack (show output)

      it "full E2E: CLI args → parse → execute → JSON format" $ do
        let args = ["--start-value", "100", "--increment", "10", "--times", "5"]
        case parseArgs counterParser args of
          Right input -> do
            let result = runCounterGraph input
                jsonOutput = formatOutput FormatJSON result
            result.finalValue `shouldBe` 150  -- 100 + (10 * 5)
            jsonOutput `shouldSatisfy` T.isInfixOf "150"
          Left err -> expectationFailure err
