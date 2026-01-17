module Main where

import Control.Concurrent (threadDelay)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
import System.IO (hPutStrLn, stderr)
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Control.Server
import Tidepool.Control.Export (exportTrainingExamples, exportGroupedTrainingExamples, exportWithExpansion, discoverSymbols)
import Tidepool.LSP.Interpreter (withLSPSession)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["export-training"] -> runExportMode False []
    ["export-training", "--grouped"] -> runExportMode True []
    ["export-training", "--expand", countStr] -> runExpandMode (read countStr)
    ("export-training" : "--grouped" : seeds) -> runExportMode True (map T.pack seeds)
    ("export-training" : seeds) -> runExportMode False (map T.pack seeds)
    ["--help"] -> printUsage
    ["-h"] -> printUsage
    _ -> runServerMode

runServerMode :: IO ()
runServerMode = do
  -- Read project directory from environment or use current directory
  projectDirEnv <- lookupEnv "TIDEPOOL_PROJECT_DIR"
  projectDir <- case projectDirEnv of
    Just dir -> pure dir
    Nothing -> getCurrentDirectory

  let config = ServerConfig { projectDir = projectDir }

  runServer config

runExportMode :: Bool -> [Text] -> IO ()
runExportMode useGrouped seeds = do
  projectDir <- getCurrentDirectory
  withLSPSession projectDir $ \session -> do
    -- HLS needs time to index the workspace before workspaceSymbol works
    hPutStrLn stderr "Waiting for HLS to index workspace (10 seconds)..."
    threadDelay (10 * 1000000)  -- 10 seconds in microseconds

    actualSeeds <- if null seeds
      then do
        hPutStrLn stderr "Discovering symbols..."
        discoverSymbols session
      else pure seeds

    hPutStrLn stderr $ "Generating examples for " <> show (length actualSeeds) <> " symbols..."
    if useGrouped
      then do
        hPutStrLn stderr "Using grouped format (v2) with LSP orchestration..."
        exportGroupedTrainingExamples session actualSeeds
      else exportTrainingExamples session actualSeeds

runExpandMode :: Int -> IO ()
runExpandMode targetCount = do
  projectDir <- getCurrentDirectory
  withLSPSession projectDir $ \session -> do
    -- HLS needs time to index the workspace before workspaceSymbol works
    hPutStrLn stderr "Waiting for HLS to index workspace (10 seconds)..."
    threadDelay (10 * 1000000)  -- 10 seconds in microseconds

    exportWithExpansion session targetCount

printUsage :: IO ()
printUsage = do
  putStrLn "tidepool-control-server - Claude Code++ control server"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  tidepool-control-server                    Start control server"
  putStrLn "  tidepool-control-server export-training    Auto-discover and generate training data"
  putStrLn "  tidepool-control-server export-training --grouped"
  putStrLn "                                             Use v2 format with grouped candidates"
  putStrLn "  tidepool-control-server export-training --expand <count>"
  putStrLn "                                             Generate training data with BFS expansion"
  putStrLn "  tidepool-control-server export-training <symbols...>"
  putStrLn "                                             Generate training data for specific symbols"
  putStrLn "  tidepool-control-server --help             Show this help"
  putStrLn ""
  putStrLn "Formats:"
  putStrLn "  v1 (default): Flat candidate list"
  putStrLn "  v2 (--grouped): Candidates grouped by edge type (Fields, Types, References)"
  putStrLn "                  Uses LSP findReferences with hub symbol detection"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  tidepool-control-server"
  putStrLn "  tidepool-control-server export-training > training.jsonl"
  putStrLn "  tidepool-control-server export-training --grouped > training-v2.jsonl"
  putStrLn "  tidepool-control-server export-training --expand 1000 > training.jsonl"
  putStrLn "  tidepool-control-server export-training ScoreConfig EdgeContext > training.jsonl"
