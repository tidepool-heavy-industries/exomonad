module Main where

import Control.Concurrent (threadDelay)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
import System.IO (hPutStrLn, stderr)
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Control.Server
import Tidepool.Control.Export (exportTrainingExamples, discoverSymbols)
import Tidepool.LSP.Interpreter (withLSPSession)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["export-training"] -> runExportMode []
    ("export-training" : seeds) -> runExportMode (map T.pack seeds)
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

runExportMode :: [Text] -> IO ()
runExportMode seeds = do
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
    exportTrainingExamples session actualSeeds

printUsage :: IO ()
printUsage = do
  putStrLn "tidepool-control-server - Claude Code++ control server"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  tidepool-control-server                    Start control server"
  putStrLn "  tidepool-control-server export-training    Auto-discover and generate training data"
  putStrLn "  tidepool-control-server export-training <symbols...>  Generate training data for specific symbols"
  putStrLn "  tidepool-control-server --help             Show this help"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  tidepool-control-server"
  putStrLn "  tidepool-control-server export-training > training.jsonl"
  putStrLn "  tidepool-control-server export-training ScoreConfig EdgeContext > training.jsonl"
