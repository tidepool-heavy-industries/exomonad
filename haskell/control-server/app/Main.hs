module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
import System.IO (hPutStrLn, stderr)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as T
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL

import Tidepool.Control.Server (runServer)
import Tidepool.Control.Types (ServerConfig(..))
import Tidepool.Control.Export (exportTrainingExamples, exportGroupedTrainingExamples, exportWithExpansion, discoverSymbols, exportCodeSamples)
import Tidepool.LSP.Interpreter (withLSPSession)
import Tidepool.Training.Format (formatTrainingFromSkeleton)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["export-training"] -> runExportMode False []
    ["export-training", "--grouped"] -> runExportMode True []
    ["export-training", "--expand", countStr] -> runExpandMode (read countStr)
    ("export-training" : "--grouped" : seeds) -> runExportMode True (map T.pack seeds)
    ("export-training" : seeds) -> runExportMode False (map T.pack seeds)
    ["export-code-samples", "--count", countStr] -> runCodeSamplesMode (read countStr)
    ["format-training", skeletonFile] -> runFormatTrainingMode skeletonFile
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

runCodeSamplesMode :: Int -> IO ()
runCodeSamplesMode count = do
  projectDir <- getCurrentDirectory
  withLSPSession projectDir $ \session -> do
    -- HLS needs time to index the workspace
    hPutStrLn stderr "Waiting for HLS to index workspace (10 seconds)..."
    threadDelay (10 * 1000000)  -- 10 seconds in microseconds

    hPutStrLn stderr $ "Generating " <> show count <> " code samples (v3 format)..."
    exportCodeSamples session projectDir count

runFormatTrainingMode :: FilePath -> IO ()
runFormatTrainingMode skeletonFile = do
  hPutStrLn stderr $ "Reading skeleton file: " <> skeletonFile
  content <- TIO.readFile skeletonFile
  let skeletonLines = T.lines content

  hPutStrLn stderr $ "Processing " <> show (length skeletonLines) <> " skeleton entries..."

  -- Process each line
  forM_ skeletonLines $ \line -> do
    case eitherDecode (BL.fromStrict $ T.encodeUtf8 line) of
      Left err -> hPutStrLn stderr $ "ERROR parsing line: " <> err
      Right skeleton -> case formatTrainingFromSkeleton skeleton of
        Left err -> hPutStrLn stderr $ "ERROR formatting: " <> T.unpack err
        Right formatted -> do
          BL.putStr (encode formatted)
          putStrLn ""  -- Add newline for JSONL format

  hPutStrLn stderr "Done"

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
  putStrLn "  tidepool-control-server export-code-samples --count <N>"
  putStrLn "                                             Generate v3 code-based skeleton JSONL"
  putStrLn "  tidepool-control-server format-training <skeleton-file>"
  putStrLn "                                             Format annotated skeletons to training JSONL"
  putStrLn "  tidepool-control-server --help             Show this help"
  putStrLn ""
  putStrLn "Formats:"
  putStrLn "  v1 (default): Flat candidate list"
  putStrLn "  v2 (--grouped): Candidates grouped by edge type (Fields, Types, References)"
  putStrLn "                  Uses LSP findReferences with hub symbol detection"
  putStrLn "  v3 (code-based): Raw code bodies + semantic criteria (export-code-samples)"
  putStrLn "                   Eliminates keyword matching bias from v1/v2"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  tidepool-control-server"
  putStrLn "  tidepool-control-server export-training > training.jsonl"
  putStrLn "  tidepool-control-server export-training --grouped > training-v2.jsonl"
  putStrLn "  tidepool-control-server export-training --expand 1000 > training.jsonl"
  putStrLn "  tidepool-control-server export-training ScoreConfig EdgeContext > training.jsonl"
  putStrLn "  tidepool-control-server export-code-samples --count 1000 > skeleton.jsonl"
  putStrLn "  tidepool-control-server format-training skeleton-annotated.jsonl > training-v3.jsonl"
