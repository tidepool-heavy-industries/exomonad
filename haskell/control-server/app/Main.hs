{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Control.Monad.Managed
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as TIO
import ExoMonad.Control.Hook.CircuitBreaker (loadCircuitBreakerConfig)
import ExoMonad.Control.Hook.Policy (loadHookPolicy)
import ExoMonad.Control.Logging (Logger, logError, logInfo, withDualLogger)
import ExoMonad.Control.Observability (TracingConfig (..), getTracer, withTracerProvider)
import ExoMonad.Control.OpenObserve (loadOpenObserveConfig)
import ExoMonad.Control.RoleConfig (Role (..))
import ExoMonad.Control.Server (runServer)
import ExoMonad.Control.Types (ServerConfig (..))
import ExoMonad.Control.Version (versionString)
import ExoMonad.Control.Workflow.Store (initWorkflowStore)
import ExoMonad.Training.Format (formatTrainingFromSkeleton)
import Options.Applicative
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)

data Command
  = Run {noTui :: Bool}
  | FormatTraining {skeletonFile :: FilePath}

data ControlServerOpts = ControlServerOpts
  { cmd :: Command
  }

parser :: Parser ControlServerOpts
parser =
  ControlServerOpts
    <$> ( subparser
            ( Options.Applicative.command "run" (info (runParser <**> helper) (progDesc "Start control server (default)"))
                <> Options.Applicative.command "format-training" (info (formatParser <**> helper) (progDesc "Format annotated skeletons to training JSONL"))
            )
            <|> runParser -- Default to run if no subcommand matches
        )

runParser :: Parser Command
runParser =
  Run
    <$> switch (long "no-tui" <> help "Start control server without TUI sidebar listener")

formatParser :: Parser Command
formatParser =
  FormatTraining
    <$> strArgument (metavar "SKELETON_FILE" <> help "Path to skeleton file")

optsInfo :: ParserInfo ControlServerOpts
optsInfo =
  info
    (helper <*> versionOption <*> parser)
    ( fullDesc
        <> progDesc "ExoMonad Control Server - Unix socket control server for Claude Code++ integration"
        <> header "exomonad-control-server - The brain of the operation"
    )

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    (T.unpack versionString)
    ( long "version"
        <> short 'V'
        <> help "Show version"
    )

main :: IO ()
main = do
  opts <- execParser optsInfo

  -- Extract project directory early for dual logger initialization
  projectDirEnv <- lookupEnv "EXOMONAD_PROJECT_DIR"
  projectDir <- case projectDirEnv of
    Just dir -> pure dir
    Nothing -> getCurrentDirectory

  runManaged $ do
    logger <- managed (withDualLogger projectDir)
    liftIO $ do
      logInfo logger $ "Session log: " <> T.pack projectDir <> "/.exomonad/logs/control-server-*.log"

      case opts.cmd of
        Run noTui -> runServerMode logger projectDir noTui
        FormatTraining file -> runFormatTrainingMode logger file

runServerMode :: Logger -> FilePath -> Bool -> IO ()
runServerMode logger projectDir noTui = do
  roleEnv <- lookupEnv "EXOMONAD_ROLE"
  policy <- loadHookPolicy projectDir

  -- Load and validate circuit breaker config
  cbConfigResult <- loadCircuitBreakerConfig
  cbConfig <- case cbConfigResult of
    Left err -> error $ "Invalid circuit breaker config: " <> T.unpack err
    Right c -> pure c

  logInfo logger $ "Circuit breaker config loaded: " <> T.pack (show cbConfig)
  ooConfig <- loadOpenObserveConfig

  -- Observability config
  otelEndpoint <- fromMaybe "localhost" <$> lookupEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
  email <- fromMaybe "admin@exomonad.local" <$> lookupEnv "OPENOBSERVE_EMAIL"
  password <- fromMaybe "exomonad-dev" <$> lookupEnv "OPENOBSERVE_PASSWORD"

  let tracingCfg =
        TracingConfig
          { tcEndpoint = otelEndpoint,
            tcAuthEmail = T.pack email,
            tcAuthPassword = T.pack password,
            tcServiceName = "exomonad-control-server"
          }

  workflowStore <- initWorkflowStore

  let config =
        ServerConfig
          { projectDir = projectDir,
            role = fmap T.pack roleEnv,
            defaultRole = Dev,
            noTui = noTui,
            observabilityConfig = Nothing,
            openObserveConfig = ooConfig,
            hookPolicy = policy,
            circuitBreakerConfig = cbConfig,
            workflowStore = workflowStore,
            llmConfig = Nothing,
            githubConfig = Nothing
          }

  -- Initialize tracing and run server
  runManaged $ do
    provider <- managed (withTracerProvider tracingCfg)
    liftIO $ do
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
          putStrLn "" -- Add newline for JSONL format
  logInfo logger "Done"

printUsage :: IO ()
printUsage = do
  TIO.putStrLn versionString
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  exomonad-control-server                    Start control server (HTTP over Unix socket)"
  putStrLn "  exomonad-control-server --no-tui           Start control server without TUI sidebar listener"
  putStrLn "  exomonad-control-server format-training <skeleton-file>"
  putStrLn "                                             Format annotated skeletons to training JSONL"
  putStrLn "  exomonad-control-server --help             Show this help"
  putStrLn "  exomonad-control-server --version          Show version info"
  putStrLn ""
