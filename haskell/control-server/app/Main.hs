{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as T
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL

import Tidepool.Control.Server (runServer)
import Tidepool.Control.Types (ServerConfig(..))
import Tidepool.Control.Hook.Policy (loadHookPolicy)
import Tidepool.Control.Logging (Logger, withDualLogger, logInfo, logError)
import Tidepool.Training.Format (formatTrainingFromSkeleton)

main :: IO ()
main = do
  args <- getArgs

  -- Extract project directory early for dual logger initialization
  projectDirEnv <- lookupEnv "TIDEPOOL_PROJECT_DIR"
  projectDir <- case projectDirEnv of
    Just dir -> pure dir
    Nothing -> getCurrentDirectory

  withDualLogger projectDir $ \logger -> do
    logInfo logger $ "Session log: " <> T.pack projectDir <> "/.tidepool/logs/control-server-*.log"

    case args of
      ["format-training", skeletonFile] -> runFormatTrainingMode logger skeletonFile
      ["--help"] -> printUsage
      ["-h"] -> printUsage
      ["--no-tui"] -> runServerMode logger projectDir True
      _ -> runServerMode logger projectDir False

runServerMode :: Logger -> FilePath -> Bool -> IO ()
runServerMode logger projectDir noTui = do
  roleEnv <- lookupEnv "TIDEPOOL_ROLE"
  policy <- loadHookPolicy projectDir
  let config = ServerConfig 
        { projectDir = projectDir
        , role = fmap T.pack roleEnv
        , noTui = noTui
        , observabilityConfig = Nothing
        , hookPolicy = policy
        }

  -- Start main control server
  runServer logger config

runFormatTrainingMode :: Logger -> FilePath -> IO ()
runFormatTrainingMode logger skeletonFile = do
  logInfo logger $ "Reading skeleton file: " <> T.pack skeletonFile
  content <- TIO.readFile skeletonFile
  let skeletonLines = T.lines content

  logInfo logger $ "Processing " <> T.pack (show (length skeletonLines)) <> " skeleton entries..."

  -- Process each line
  forM_ skeletonLines $ \line -> do
    case eitherDecode (BL.fromStrict $ T.encodeUtf8 line) of
      Left err -> logError logger $ "ERROR parsing line: " <> T.pack err
      Right skeleton -> case formatTrainingFromSkeleton skeleton of
        Left err -> logError logger $ "ERROR formatting: " <> err
        Right formatted -> do
          BL.putStr (encode formatted)
          putStrLn ""  -- Add newline for JSONL format

  logInfo logger "Done"

printUsage :: IO ()
printUsage = do
  putStrLn "tidepool-control-server - Claude Code++ control server"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  tidepool-control-server                    Start control server (HTTP over Unix socket)"
  putStrLn "  tidepool-control-server --no-tui           Start control server without TUI sidebar listener"
  putStrLn "  tidepool-control-server format-training <skeleton-file>"
  putStrLn "                                             Format annotated skeletons to training JSONL"
  putStrLn "  tidepool-control-server --help             Show this help"
  putStrLn ""