{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Work Graph Runner
--
-- Executes the single-node recursive Work graph.
--
-- Flow: Work → (Spawn/Continue/AwaitNext) → Work → ... → Complete
module Main (main) where

import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileThrow)
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory, doesFileExist)
import System.IO (hFlush, stdout)
import System.Exit (exitFailure)
import Text.Printf (printf)

import Tidepool.Actor.Subgraph (withRecursiveGraph)
import Tidepool.Graph.Execute (callHandler)

import TypesFirstDev.Types.Core (Spec(..))
import TypesFirstDev.Types.Work (WorkInput(..), WorkResult(..))
import TypesFirstDev.WorkInterpreters (runWorkEffects, WorkConfig(..))
import TypesFirstDev.WorkExecutor (runWork)
import TypesFirstDev.WorkHandlersAssembled (workHandlers)
import TypesFirstDev.WorkGraph (WorkGraph(..))

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
    RunnerSuccess (WorkResult commitHash) -> do
      putStrLn "✓ Work Complete"
      printf "  Final commit: %s\n" (T.unpack commitHash)
    RunnerFailure reason -> do
      putStrLn "✗ Work Failed"
      printf "  Reason: %s\n" (T.unpack reason)

-- | Run the Work graph with effect interpreter chain.
runWorkGraph :: Spec -> WorkConfig -> IO WorkResult
runWorkGraph spec config = do
  -- Set up Subgraph with recursive wiring
  withRecursiveGraph @WorkInput @WorkResult $ \subgraphState wire -> do
    -- Wire the recursive graph runner - children spawn with WorkInput
    wire $ \childInput -> do
      runWorkEffects subgraphState childInput config $
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

    -- Run root graph
    runWorkEffects subgraphState rootInput config $
      runWork
        (callHandler (wgWork workHandlers))
        rootInput

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
            , wcMaxDepth = 3
            }

      -- Run the graph
      result <- runWorkGraph spec config
      printResult (RunnerSuccess result)
