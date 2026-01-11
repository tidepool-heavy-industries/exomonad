{-# LANGUAGE OverloadedStrings #-}

-- | V3 TDD Runner
--
-- Orchestrates the complete V3 TDD protocol:
-- 1. Loads YAML specification
-- 2. Initializes effect state (memory, worktree)
-- 3. Spawns root graph with deferred child spawning
-- 4. Returns merged work result
module Main (main) where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad (unless)
import Data.Text (Text, pack)
import Data.Yaml (decodeFileThrow)
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory, doesFileExist)
import System.IO (hFlush, stdout)
import System.Exit (exitFailure)
import Text.Printf (printf)

import Control.Monad.Freer (Eff)

import Tidepool.Actor.Subgraph (withRecursiveGraph)
import Tidepool.Graph.Execute (runGraph)

import TypesFirstDev.Handlers.Scaffold (ScaffoldInput(..))
import TypesFirstDev.Handlers.ImplReviewMerge (ImplInput(..))
import TypesFirstDev.Types.Core (Spec)
import TypesFirstDev.Types.Payloads (MergeComplete(..))
import TypesFirstDev.Types.Shared (ImpactLevel(..))
import TypesFirstDev.Types.Memory (TDDMem(..), ImplMem(..), emptyTDDMem, emptyImplMem)
import TypesFirstDev.V3.Interpreters (runV3Effects, WorktreeConfig(..), V3Effects)
import TypesFirstDev.Graph (TDDGraph(..))
import Tidepool.Graph.Generic (AsHandler)
import qualified TypesFirstDev.V3.Interpreters as V3 (V3Result(..))

-- | Application-level result type (more detailed than V3Result)
data RunnerResult = RunnerSuccess MergeComplete | RunnerFailure Text

-- | Print banner
printBanner :: String
printBanner = unlines
  [ "════════════════════════════════════════════════════════════════"
  , "  V3 TDD Orchestrator"
  , "════════════════════════════════════════════════════════════════"
  ]

-- | Print result
printResult :: RunnerResult -> IO ()
printResult result = do
  putStrLn "════════════════════════════════════════════════════════════════"
  putStrLn "  Result"
  putStrLn "════════════════════════════════════════════════════════════════"
  case result of
    RunnerSuccess (MergeComplete commit author impact changes) -> do
      putStrLn "✓ TDD Cycle Complete"
      printf "  Commit: %s\n" commit
      printf "  Author: %s\n" author
      printf "  Impact: %s\n" (show impact)
      printf "  Changes: %d entries\n" (length changes)
    RunnerFailure reason -> do
      putStrLn "✗ TDD Failed"
      printf "  Reason: %s\n" (show reason)

-- | Run a full V3 TDD graph from a Spec.
--
-- This is the main orchestration function. It:
-- 1. Creates a recursive subgraph with deferred binding
-- 2. Initializes memory (TVars)
-- 3. Wires the graph runner (CRITICAL: before running)
-- 4. Executes root graph
-- 5. Returns final result
runV3Graph :: Spec -> WorktreeConfig -> IO RunnerResult
runV3Graph spec wtConfig =
  -- Step 1: Create deferred subgraph state
  withRecursiveGraph @Spec @V3.V3Result $ \subgraphState wire -> do
    -- Step 2: Initialize shared memory
    tddMem <- newTVarIO $ emptyTDDMem "root-conversation"
    implMem <- newTVarIO $ emptyImplMem "root-conversation"

    -- Step 3: Create effect interpreter (captures subgraphState, memory, config)
    let interpret :: forall a. Eff V3Effects a -> IO a
        interpret = runV3Effects subgraphState tddMem implMem wtConfig

    -- Step 4: WIRE THE RECURSION (MUST happen before running!)
    -- This is the critical deferred binding that enables child graphs to spawn
    --
    -- TODO: Complete handler record assembly (Phase 6 expanded)
    -- The wire function should run child graphs via runGraph with v3Handlers,
    -- but v3Handlers needs all handler implementations first.
    wire $ \childSpec -> do
      -- TODO: Execute child graph with v3Handlers
      -- let childInput = ScaffoldInput
      --       { siSpec = childSpec
      --       , siParentContext = Nothing
      --       }
      -- interpret (runGraph v3Handlers childInput)
      pure V3.V3Success

    -- Step 5: Run root graph
    -- TODO: Execute root graph with v3Handlers (requires handler assembly)
    -- let rootInput = ScaffoldInput spec Nothing
    -- _ <- interpret (runGraph v3Handlers rootInput)
    pure $ RunnerSuccess (MergeComplete "" "" Trivial [])

-- | Main entry point
main :: IO ()
main = do
  -- Print banner
  putStr printBanner
  hFlush stdout

  -- Parse CLI arguments
  args <- getArgs
  cwd <- getCurrentDirectory

  case args of
    [specFile] -> do
      -- Check if file exists
      exists <- doesFileExist specFile
      unless exists $ do
        putStrLn $ "Error: Spec file not found: " <> specFile
        exitFailure

      putStrLn $ "Loading spec: " <> specFile

      -- Load YAML spec
      spec <- decodeFileThrow specFile

      -- Configure execution
      let wtConfig = WorktreeConfig
            { wtcBaseDir = cwd
            , wtcParentBranch = "main"
            }

      putStrLn ""
      putStrLn "Starting V3 TDD cycle..."
      putStrLn ""
      hFlush stdout

      -- Run the graph
      result <- runV3Graph spec wtConfig

      -- Print results
      putStrLn ""
      printResult result

    _ -> do
      putStrLn "Usage: types-first-dev-runner <spec.yaml>"
      putStrLn ""
      putStrLn "Arguments:"
      putStrLn "  spec.yaml - YAML file with TDD specification"
      exitFailure
