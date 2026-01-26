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
import Data.Maybe (fromMaybe)

import ExoMonad.Control.Server (runServer)
import ExoMonad.Control.Types (ServerConfig(..))
import ExoMonad.Control.Hook.Policy (loadHookPolicy)
import ExoMonad.Control.Logging (Logger, withDualLogger, logInfo, logError)
import ExoMonad.Control.Observability (withTracerProvider, getTracer, TracingConfig(..))
import ExoMonad.Control.RoleConfig (Role(..))
import ExoMonad.Training.Format (formatTrainingFromSkeleton)

main :: IO ()
main = do
  args <- getArgs

  -- Extract project directory early for dual logger initialization
  projectDirEnv <- lookupEnv "EXOMONAD_PROJECT_DIR"
  projectDir <- case projectDirEnv of
    Just dir -> pure dir
    Nothing -> getCurrentDirectory

  withDualLogger projectDir $ \logger -> do
    logInfo logger $ "Session log: " <> T.pack projectDir <> "/.exomonad/logs/control-server-*.log"

    case args of
      ["format-training", skeletonFile] -> runFormatTrainingMode logger skeletonFile
      ["--help"] -> printUsage
      ["-h"] -> printUsage
      ["--no-tui"] -> runServerMode logger projectDir True
      _ -> runServerMode logger projectDir False

runServerMode :: Logger -> FilePath -> Bool -> IO ()
runServerMode logger projectDir noTui = do
  roleEnv <- lookupEnv "EXOMONAD_ROLE"
  policy <- loadHookPolicy projectDir

  -- Observability config
  otelEndpoint <- fromMaybe "localhost" <$> lookupEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
  email <- fromMaybe "admin@exomonad.local" <$> lookupEnv "OPENOBSERVE_EMAIL"
  password <- fromMaybe "exomonad-dev" <$> lookupEnv "OPENOBSERVE_PASSWORD"
  
  let tracingCfg = TracingConfig
        { tcEndpoint = otelEndpoint
        , tcAuthEmail = T.pack email
        , tcAuthPassword = T.pack password
        , tcServiceName = "exomonad-control-server"
        }

  let config = ServerConfig 
        { projectDir = projectDir
        , role = fmap T.pack roleEnv
        , defaultRole = Dev
        , noTui = noTui
        , observabilityConfig = Nothing
        , hookPolicy = policy
        }

  -- Initialize tracing and run server
  withTracerProvider tracingCfg $ \provider -> do
    let tracer = getTracer provider "control-server"
    runServer logger config tracer

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
  putStrLn "exomonad-control-server - Claude Code++ control server"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  exomonad-control-server                    Start control server (HTTP over Unix socket)"
  putStrLn "  exomonad-control-server --no-tui           Start control server without TUI sidebar listener"
  putStrLn "  exomonad-control-server format-training <skeleton-file>"
  putStrLn "                                             Format annotated skeletons to training JSONL"
  putStrLn "  exomonad-control-server --help             Show this help"
  putStrLn ""