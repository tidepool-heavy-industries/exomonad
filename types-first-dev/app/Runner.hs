{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Work Graph Runner
--
-- Executes the single-node recursive Work graph.
--
-- Flow: Work → (Spawn/Continue/AwaitNext) → Work → ... → Complete
--
-- Produces RunTree output for post-run analysis.
module Main (main) where

import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime)
import Data.Yaml (decodeFileThrow)
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory, doesFileExist, createDirectoryIfMissing)
import System.IO (hFlush, stdout)
import System.Exit (exitFailure)
import Text.Printf (printf)

import Control.Concurrent.STM (atomically, modifyTVar')
import qualified Data.Map.Strict as Map
import Tidepool.Actor.Subgraph (withRecursiveGraph, ChildId(..))
import Tidepool.Graph.Interpret (callHandler)

import TypesFirstDev.Types.Core (Spec(..))
import TypesFirstDev.Types.Work (WorkInput(..), WorkResult(..), PlanRevisionDetails(..))
import TypesFirstDev.WorkInterpreters (runWorkEffects, WorkConfig(..))
import TypesFirstDev.WorkInterpreter (runWork)
import TypesFirstDev.WorkHandlersAssembled (workHandlers)
import TypesFirstDev.WorkGraph (WorkGraph(..))
import TypesFirstDev.RunTree (RunTree(..))
import TypesFirstDev.Effect.RunTreeLog (newNodeBuilder, freezeNode, NodeBuilder(..))
import TypesFirstDev.RunTree.Render (renderRunTree, writeRunTree)

-- | Application-level result type
data RunnerResult = RunnerSuccess WorkResult | RunnerFailure Text

-- | Print banner
printBanner :: String
printBanner = unlines
  [ "════════════════════════════════════════════════════════════════"
  , "  Work Graph Orchestrator (single recursive node)"
  , "════════════════════════════════════════════════════════════════"
  ]

-- | Print result
printResult :: RunnerResult -> IO ()
printResult result = do
  putStrLn "════════════════════════════════════════════════════════════════"
  putStrLn "  Result"
  putStrLn "════════════════════════════════════════════════════════════════"
  case result of
    RunnerSuccess (WorkResult { wrCommitHash = commitHash, wrPlanRevision = revision }) ->
      case revision of
        Nothing -> do
          putStrLn "✓ Work Complete"
          printf "  Final commit: %s\n" (T.unpack commitHash)
        Just details -> do
          putStrLn "⚠️  Plan Revision Needed"
          printf "  Issue: %s\n" (T.unpack details.prdIssue)
          printf "  Discovery: %s\n" (T.unpack details.prdDiscovery)
          printf "  Proposed: %s\n" (T.unpack details.prdProposedChange)
    RunnerFailure reason -> do
      putStrLn "✗ Work Failed"
      printf "  Reason: %s\n" (T.unpack reason)

-- | Run the Work graph with effect interpreter chain.
--
-- Returns both the WorkResult and the RunTree for post-run analysis.
runWorkGraph :: Spec -> WorkConfig -> IO (WorkResult, RunTree)
runWorkGraph spec config = do
  startTime <- getCurrentTime

  -- Create root node builder for execution logging
  rootBuilder <- newNodeBuilder spec.sDescription 0

  result <- withRecursiveGraph @WorkInput @WorkResult (Just config.wcMaxConcurrency) $ \subgraphState wire -> do
    -- Wire the recursive graph runner - children spawn with WorkInput
    -- Each child gets its own NodeBuilder, linked to parent's nbChildren
    wire $ \childId childInput -> do
      childBuilder <- newNodeBuilder childInput.wiSpec.sDescription childInput.wiDepth
      -- Register child builder in parent's nbChildren map for tree construction
      atomically $ modifyTVar' rootBuilder.nbChildren (Map.insert childId childBuilder)
      runWorkEffects subgraphState childBuilder childInput config $
        runWork
          (callHandler (wgWork workHandlers))
          childInput

    -- Build root input (no parent session for root)
    let rootInput = WorkInput
          { wiSpec = spec
          , wiDepth = 0
          , wiMaxDepth = config.wcMaxDepth
          , wiParentInfo = Nothing  -- Root has no parent
          }

    -- Run root graph with root builder
    runWorkEffects subgraphState rootBuilder rootInput config $
      runWork
        (callHandler (wgWork workHandlers))
        rootInput

  -- Freeze the execution tree
  endTime <- getCurrentTime
  rootNode <- freezeNode rootBuilder

  let tree = RunTree
        { rtSpec = spec
        , rtStartTime = startTime
        , rtEndTime = Just endTime
        , rtRoot = rootNode
        }

  pure (result, tree)

-- | Main entry point
main :: IO ()
main = do
  putStr printBanner
  hFlush stdout

  args <- getArgs
  cwd <- getCurrentDirectory

  case args of
    [specFile, targetDir] -> runWithTarget specFile targetDir
    [specFile] -> runWithTarget specFile cwd
    _ -> do
      putStrLn "Usage: types-first-dev-runner <spec.yaml> [target-dir]"
      putStrLn ""
      putStrLn "Arguments:"
      putStrLn "  spec.yaml   - YAML file with TDD specification"
      putStrLn "  target-dir  - Directory to work in (default: cwd)"
      exitFailure
  where
    runWithTarget specFile targetDir = do
      exists <- doesFileExist specFile
      unless exists $ do
        putStrLn $ "Error: Spec file not found: " <> specFile
        exitFailure

      putStrLn $ "Loading spec: " <> specFile

      -- Load and validate YAML spec
      spec <- decodeFileThrow specFile :: IO Spec

      putStrLn $ "  ID: " <> T.unpack spec.sId
      putStrLn $ "  Description: " <> take 60 (T.unpack spec.sDescription) <> "..."
      putStrLn $ "  Target: " <> targetDir
      putStrLn ""
      putStrLn "Executing Work graph..."
      putStrLn ""

      -- Set up config - use target dir, not cwd
      let config = WorkConfig
            { wcBaseDir = targetDir
            , wcParentBranch = "main"
            , wcMaxDepth = 3  -- Root(0) → Package(1) → Module(2) → Leaf(3)
            , wcMaxConcurrency = 4  -- Max 4 children running per parent
            }

      -- Run the graph
      (result, tree) <- runWorkGraph spec config
      printResult (RunnerSuccess result)

      -- Output the execution tree
      putStrLn ""
      putStrLn "════════════════════════════════════════════════════════════════"
      putStrLn "  Execution Tree"
      putStrLn "════════════════════════════════════════════════════════════════"
      TIO.putStrLn (renderRunTree tree)

      -- Write tree to output directory
      let outputDir = targetDir <> "/.mantle/runs"
      createDirectoryIfMissing True outputDir
      writeRunTree outputDir tree
      putStrLn $ "Run tree written to: " <> outputDir
