-- | Effect runner - composes all interpreters.
--
-- Wires up UI, Habitica, Observability, and LLM interpreters to run
-- complete agent graphs.
--
-- = Usage
--
-- @
-- import ExoMonad.Server.EffectRunner
--
-- config <- loadInterpreterConfig
-- env <- mkInterpreterEnv config
-- result <- runEffects env ctx callback agentProgram
-- @
module ExoMonad.Server.EffectRunner
  ( -- * Configuration
    InterpreterConfig(..)
  , defaultInterpreterConfig
  , loadInterpreterConfig

    -- * Environment
  , InterpreterEnv(..)
  , mkInterpreterEnv

    -- * Running Effects
  , runEffects
  ) where

import Control.Exception (bracket)
import Control.Monad.Freer (Eff, runM)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.Read (readMaybe)

-- Interpreter imports
import ExoMonad.UI.Interpreter
  ( UIContext
  , UICallback
  , newUIContext
  , runUI
  )
import ExoMonad.Habitica.Interpreter
  ( HabiticaConfig(..)
  , HabiticaEnv
  , mkHabiticaEnv
  , runHabitica
  , defaultHabiticaConfig
  )
import ExoMonad.Observability.Interpreter
  ( LokiConfig(..)
  , OTLPConfig(..)
  , runObservabilityWithContext
  , newTraceContext
  , flushTraces
  , defaultLokiConfig
  , defaultOTLPConfig
  )
import ExoMonad.LLM.Interpreter
  ( LLMEnv
  , LLMConfig(..)
  , mkLLMEnv
  , runLLMComplete
  )
import ExoMonad.LLM.Types
  ( AnthropicSecrets(..)
  , OpenAISecrets(..)
  , ApiKey(..)
  , BaseUrl(..)
  )
import ExoMonad.Log.Interpreter
  ( runLogFastLogger
  , LogConfig(..)
  , LogOutput(..)
  , defaultLogConfig
  )
import ExoMonad.Effect.Log (Log, LogLevel(..), LogContext, emptyLogContext)
import Control.Monad.Freer.Reader (Reader, runReader)
import System.Log.FastLogger (LoggerSet, newStderrLoggerSet, newFileLoggerSet, flushLogStr, rmLoggerSet)

-- Effect imports
import ExoMonad.Effects.UI (UI)
import ExoMonad.Effects.Habitica (Habitica)
import ExoMonad.Effects.Observability (Observability)
import ExoMonad.Effects.LLMProvider (LLMComplete)


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for all interpreters.
data InterpreterConfig = InterpreterConfig
  { ecLLMConfig :: LLMConfig
    -- ^ LLM provider configuration
  , ecHabiticaConfig :: HabiticaConfig
    -- ^ Habitica API configuration
  , ecLokiConfig :: LokiConfig
    -- ^ Loki/Grafana observability configuration (logs)
  , ecOTLPConfig :: Maybe OTLPConfig
    -- ^ OTLP/Grafana Tempo configuration (traces)
  , ecServiceName :: Text
    -- ^ Service name for trace attribution
  , ecLogConfig :: LogConfig
    -- ^ Log (session-scoped file logging) configuration
  }
  deriving stock (Show)

-- | Default configuration with empty credentials.
--
-- Use 'loadInterpreterConfig' to load from environment variables.
defaultInterpreterConfig :: InterpreterConfig
defaultInterpreterConfig = InterpreterConfig
  { ecLLMConfig = LLMConfig
      { lcAnthropicSecrets = Nothing
      , lcOpenAISecrets = Nothing
      }
  , ecHabiticaConfig = defaultHabiticaConfig
  , ecLokiConfig = defaultLokiConfig
  , ecOTLPConfig = Nothing  -- Disabled by default
  , ecServiceName = "exomonad-native"
  , ecLogConfig = defaultLogConfig
  }

-- | Load configuration from environment variables.
--
-- Environment variables:
--
-- * @ANTHROPIC_API_KEY@ - Anthropic API key
-- * @OPENAI_API_KEY@ - OpenAI API key
-- * @HABITICA_USER_ID@ - Habitica user ID
-- * @HABITICA_API_TOKEN@ - Habitica API token
-- * @LOKI_URL@ - Loki push endpoint (default: http://localhost:3100)
-- * @LOKI_USER@ - Grafana Cloud user ID (optional)
-- * @LOKI_TOKEN@ - Grafana Cloud API token (optional)
-- * @OTLP_ENDPOINT@ - OTLP trace endpoint (optional, e.g., https://otlp-gateway.../otlp/v1/traces)
-- * @OTLP_USER@ - OTLP basic auth user (optional)
-- * @OTLP_TOKEN@ - OTLP basic auth token (optional)
-- * @SERVICE_NAME@ - Service name for traces (default: exomonad-native)
-- * @LOG_DIR@ - Log output directory (default: disabled, logs to stderr)
-- * @LOG_LEVEL@ - Minimum log level: trace|debug|info|warn|error (default: info)
-- * @LOG_LATEST@ - Create latest.log symlink: true|false (default: true)
loadInterpreterConfig :: IO InterpreterConfig
loadInterpreterConfig = do
  -- LLM secrets
  anthropicKey <- lookupEnv "ANTHROPIC_API_KEY"
  openaiKey <- lookupEnv "OPENAI_API_KEY"

  -- Habitica config
  habiticaUser <- lookupEnv "HABITICA_USER_ID"
  habiticaToken <- lookupEnv "HABITICA_API_TOKEN"

  -- Loki config
  lokiUrl <- lookupEnv "LOKI_URL"
  lokiUser <- lookupEnv "LOKI_USER"
  lokiToken <- lookupEnv "LOKI_TOKEN"

  -- OTLP config
  otlpEndpoint <- lookupEnv "OTLP_ENDPOINT"
  otlpUser <- lookupEnv "OTLP_USER"
  otlpToken <- lookupEnv "OTLP_TOKEN"
  serviceName <- lookupEnv "SERVICE_NAME"

  -- Log config
  logDir <- lookupEnv "LOG_DIR"
  logLevel <- lookupEnv "LOG_LEVEL"
  logLatest <- lookupEnv "LOG_LATEST"

  let minLevel = case logLevel of
        Just "trace" -> Trace
        Just "debug" -> Debug
        Just "warn"  -> Warn
        Just "error" -> Error
        _            -> Info

      logOutput = case logDir of
        Just dir -> LogFile dir
        Nothing  -> LogStderr  -- Default to stderr if no dir specified

      createLatest = case logLatest of
        Just "false" -> False
        _            -> True

  pure InterpreterConfig
    { ecLLMConfig = LLMConfig
        { lcAnthropicSecrets = case anthropicKey of
            Just k -> Just AnthropicSecrets
              { asApiKey = ApiKey (T.pack k)
              , asBaseUrl = BaseUrl "https://api.anthropic.com"
              }
            Nothing -> Nothing
        , lcOpenAISecrets = case openaiKey of
            Just k -> Just OpenAISecrets
              { osApiKey = ApiKey (T.pack k)
              , osBaseUrl = BaseUrl "https://api.openai.com"
              , osOrgId = Nothing
              }
            Nothing -> Nothing
        }
    , ecHabiticaConfig = HabiticaConfig
        { hcBaseUrl = "https://habitica.com/api/v3"
        , hcUserId = maybe "" T.pack habiticaUser
        , hcApiToken = maybe "" T.pack habiticaToken
        }
    , ecLokiConfig = LokiConfig
        { lcBaseUrl = maybe "http://localhost:3100" T.pack lokiUrl
        , lcUser = T.pack <$> lokiUser
        , lcToken = T.pack <$> lokiToken
        , lcJobLabel = "exomonad-native"
        }
    , ecOTLPConfig = case otlpEndpoint of
        Just endpoint -> Just OTLPConfig
          { otlpEndpoint = T.pack endpoint
          , otlpUser = T.pack <$> otlpUser
          , otlpToken = T.pack <$> otlpToken
          }
        Nothing -> Nothing
    , ecServiceName = maybe "exomonad-native" T.pack serviceName
    , ecLogConfig = LogConfig
        { lcMinLevel = minLevel
        , lcOutput = logOutput
        , lcHumanReadable = True
        , lcSessionId = Nothing  -- Auto-generated
        , lcSessionName = Nothing
        , lcSymlinkLatest = createLatest
        }
    }


-- ════════════════════════════════════════════════════════════════════════════
-- ENVIRONMENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Runtime environment with initialized resources.
data InterpreterEnv = InterpreterEnv
  { eeConfig :: InterpreterConfig
    -- ^ Original configuration
  , eeLLMEnv :: LLMEnv
    -- ^ Initialized LLM client (with HTTP manager)
  , eeHabiticaEnv :: HabiticaEnv
    -- ^ Initialized Habitica client (with HTTP manager)
  }

-- | Create a new interpreter environment.
--
-- Initializes HTTP managers and other resources needed by interpreters.
mkInterpreterEnv :: InterpreterConfig -> IO InterpreterEnv
mkInterpreterEnv config = do
  llmEnv <- mkLLMEnv (ecLLMConfig config)
  habiticaEnv <- mkHabiticaEnv (ecHabiticaConfig config)
  pure InterpreterEnv
    { eeConfig = config
    , eeLLMEnv = llmEnv
    , eeHabiticaEnv = habiticaEnv
    }


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT RUNNERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a program with all effects composed.
--
-- This composes the full effect stack:
--
-- @
-- Eff '[UI, Habitica, LLMComplete, Log, Reader LogContext, Observability, IO] a
--   → runObservabilityWithContext (interpret Observability)
--   → runReader emptyLogContext (interpret Reader LogContext)
--   → runLogFastLogger (interpret Log, requires Reader LogContext in tail)
--   → runLLMComplete (interpret LLMComplete)
--   → runHabitica (interpret Habitica)
--   → runUI (interpret UI)
--   → IO a
-- @
--
-- Note: Effect stack ordering matters. Effects are peeled from the outside in:
-- 1. UI (first to peel) - handles user interaction
-- 2. Habitica - makes Habitica API calls
-- 3. LLMComplete - makes LLM API calls
-- 4. Log - session-scoped logging (fast-logger backend)
-- 5. Reader LogContext - provides correlation context for Log
-- 6. Observability (last to peel) - records events and spans
--
-- The Log interpreter has constraint @Member (Reader LogContext) es@ where @es@
-- is the tail after Log is removed. So Log must come BEFORE Reader LogContext.
--
-- Traces are automatically flushed to OTLP (Grafana Tempo) after execution
-- if @OTLP_ENDPOINT@ is configured.
--
-- Example:
--
-- @
-- myAgent :: Eff '[UI, Habitica, LLMComplete, Log, Reader LogContext, Observability, IO] String
-- myAgent = do
--   publishEvent $ GraphTransition "entry" "greeting" "start"
--   showText "Welcome!"
--   name <- requestTextInput "What's your name?"
--   pure $ "Hello, " <> name
--
-- result <- runEffects env ctx callback myAgent
-- @
runEffects
  :: InterpreterEnv
  -> UIContext
  -> UICallback
  -> Eff '[UI, Habitica, LLMComplete, Log, Reader LogContext, Observability, IO] a
  -> IO a
runEffects env ctx callback action = do
  -- Create a fresh trace context for this request
  traceCtx <- newTraceContext

  let logConfig = ecLogConfig $ eeConfig env

  -- Create and manage logger with bracket for exception safety
  result <- bracket
    (mkLoggerSet logConfig)
    (\ls -> flushLogStr ls >> rmLoggerSet ls)
    (\loggerSet -> runM
        . runObservabilityWithContext traceCtx (ecLokiConfig $ eeConfig env)
        . runReader emptyLogContext
        . runLogFastLogger logConfig loggerSet
        . runLLMComplete (eeLLMEnv env)
        . runHabitica (eeHabiticaEnv env)
        . runUI ctx callback
        $ action
    )

  -- Flush traces to OTLP if configured
  case ecOTLPConfig (eeConfig env) of
    Just otlpConfig ->
      flushTraces otlpConfig (ecServiceName $ eeConfig env) traceCtx
    Nothing ->
      pure ()

  pure result

-- | Create logger set based on config output setting.
--
-- Note: LogBoth creates a file logger (not stderr). For true dual logging
-- (stderr + file), use 'withLogInterpreter' from "ExoMonad.Log.Interpreter"
-- which handles both destinations with proper resource management.
mkLoggerSet :: LogConfig -> IO LoggerSet
mkLoggerSet config = case config.lcOutput of
  LogStderr -> newStderrLoggerSet 4096
  LogFile dir -> do
    createDirectoryIfMissing True dir
    newFileLoggerSet 4096 (dir </> "session.log")
  LogBoth dir -> do
    -- LogBoth falls back to file logging in this simplified API.
    -- For true dual logging, use withLogInterpreter from log-interpreter.
    createDirectoryIfMissing True dir
    newFileLoggerSet 4096 (dir </> "session.log")
