{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | V3 TDD Runner
--
-- Executes the V3 TDD graph using direct handler chaining.
-- This bypasses the generic dispatch mechanism that requires HasField
-- (which GHC can't derive for records with type-family-computed fields).
--
-- Flow: Scaffold → TDDWriteTests → ImplBarrier → Impl → ... → MergeComplete
module Main (main) where

import Control.Monad (unless)
import Control.Concurrent.STM (newTVarIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileThrow)
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory, doesFileExist)
import System.IO (hFlush, stdout)
import System.Exit (exitFailure)
import Text.Printf (printf)

import Tidepool.Actor.Subgraph (withRecursiveGraph)

import TypesFirstDev.Types.Core (Spec(..))
import TypesFirstDev.Types.Nodes (ScaffoldInput(..))
import TypesFirstDev.Types.Payloads (MergeComplete(..), ChildFailure(..))
import TypesFirstDev.Types.Shared (NodeInfo(..))
import TypesFirstDev.Types.Memory (emptyTDDMem, emptyImplMem)
import TypesFirstDev.V3.Interpreters (runV3Effects, WorktreeConfig(..), ExecutionContext(..))
import TypesFirstDev.Executor (runV3)

-- | Application-level result type
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
    RunnerSuccess (MergeSuccess commit author impact changes) -> do
      putStrLn "✓ TDD Cycle Complete"
      printf "  Commit: %s\n" commit
      printf "  Author: %s\n" author
      printf "  Impact: %s\n" (show impact)
      printf "  Changes: %d entries\n" (length changes)
    RunnerSuccess (MergeFailed failure) -> do
      putStrLn "✗ Child Failed"
      printf "  Reason: %s\n" (T.unpack failure.cfReason)
      printf "  Branch: %s\n" (T.unpack failure.cfBranch)
      printf "  Attempts: %d\n" (failure.cfAttempts :: Int)
      case failure.cfPartialCommit of
        Just partialCommit -> printf "  Partial commit: %s\n" (T.unpack partialCommit)
        Nothing -> pure ()
    RunnerFailure reason -> do
      putStrLn "✗ TDD Failed"
      printf "  Reason: %s\n" (show reason)

-- | Run the V3 graph with full effect interpreter chain.
runV3Graph :: Spec -> WorktreeConfig -> IO MergeComplete
runV3Graph spec wtConfig = do
  -- Initialize memory TVars
  tddMem <- newTVarIO (emptyTDDMem "root")
  implMem <- newTVarIO (emptyImplMem "root")

  -- Set up Subgraph with recursive wiring
  -- Subgraph type is ScaffoldInput (not Spec) because children need full context
  -- (depth, parent session, parent context) for proper recursive execution.
  withRecursiveGraph @ScaffoldInput @MergeComplete $ \subgraphState wire -> do
    -- Wire the recursive graph runner - children are spawned with full ScaffoldInput
    wire $ \childInput -> do
      let childSpec = childInput.siSpec
      let childExecCtx = ExecutionContext
            { ecSpec = childSpec
            , ecScaffold = Nothing  -- Graph starts at Scaffold; output populated after node runs
            , ecNodeInfo = Just NodeInfo
                { niId = childSpec.sId
                , niBranch = "tdd/" <> childSpec.sId
                }
            }
      runV3Effects subgraphState tddMem implMem childInput childExecCtx wtConfig (runV3 childInput)

    -- Build root input
    let rootInput = ScaffoldInput
          { siSpec = spec
          , siParentContext = Nothing
          , siCurrentDepth = 0
          , siMaxDepth = 3
          , siParentSessionId = Nothing
          , siClarificationNeeded = Nothing  -- Fresh start, no prior failure
          }

    -- Build root execution context
    let rootExecCtx = ExecutionContext
          { ecSpec = spec
          , ecScaffold = Nothing  -- Graph starts at Scaffold; output populated after node runs
          , ecNodeInfo = Just NodeInfo
              { niId = spec.sId
              , niBranch = "tdd/" <> spec.sId
              }
          }

    runV3Effects subgraphState tddMem implMem rootInput rootExecCtx wtConfig (runV3 rootInput)

-- | Main entry point
main :: IO ()
main = do
  putStr printBanner
  hFlush stdout

  args <- getArgs
  cwd <- getCurrentDirectory

  case args of
    [specFile] -> do
      exists <- doesFileExist specFile
      unless exists $ do
        putStrLn $ "Error: Spec file not found: " <> specFile
        exitFailure

      putStrLn $ "Loading spec: " <> specFile

      -- Load and validate YAML spec
      spec <- decodeFileThrow specFile :: IO Spec

      putStrLn $ "  Description: " <> show (take 50 $ show spec) <> "..."
      putStrLn ""
      putStrLn "Executing V3 TDD graph..."
      putStrLn ""

      -- Set up worktree config
      let wtConfig = WorktreeConfig
            { wtcBaseDir = cwd
            , wtcParentBranch = "main"
            }

      -- Run the graph
      mergeResult <- runV3Graph spec wtConfig
      printResult (RunnerSuccess mergeResult)

    _ -> do
      putStrLn "Usage: types-first-dev-runner <spec.yaml>"
      putStrLn ""
      putStrLn "Arguments:"
      putStrLn "  spec.yaml - YAML file with TDD specification"
      exitFailure
