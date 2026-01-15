-- | Effect runner - composes all interpreters.
--
-- Wires up UI, Habitica, Observability, and LLM interpreters to run
-- complete agent graphs.
--
-- = Usage
--
-- @
-- import Tidepool.Server.EffectRunner
--
-- config <- loadInterpreterConfig
-- env <- mkInterpreterEnv config
-- result <- runEffects env ctx callback agentProgram
-- @
module Tidepool.Server.EffectRunner
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

import Control.Monad.Freer (Eff, runM)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- Interpreter imports
import Tidepool.UI.Interpreter
  ( UIContext
  , UICallback
  , newUIContext
  , runUI
  )
import Tidepool.Habitica.Interpreter
  ( HabiticaConfig(..)
  , HabiticaEnv
  , mkHabiticaEnv
  , runHabitica
  , defaultHabiticaConfig
  )
import Tidepool.Observability.Interpreter
  ( LokiConfig(..)
  , OTLPConfig(..)
  , runObservabilityWithContext
  , newTraceContext
  , flushTraces
  , defaultLokiConfig
  , defaultOTLPConfig
  )
import Tidepool.LLM.Interpreter
  ( LLMEnv
  , LLMConfig(..)
  , mkLLMEnv
  , runLLMComplete
  )
import Tidepool.LLM.Types
  ( AnthropicSecrets(..)
  , OpenAISecrets(..)
  , ApiKey(..)
  , BaseUrl(..)
  )
import Tidepool.DevLog.Interpreter
  ( runDevLog
  , DevLogConfig(..)
  , DevLogOutput(..)
  , defaultDevLogConfig
  )
import Tidepool.Effect.DevLog (DevLog, Verbosity(..))

-- Effect imports
import Tidepool.Effects.UI (UI)
import Tidepool.Effects.Habitica (Habitica)
import Tidepool.Effects.Observability (Observability)
import Tidepool.Effects.LLMProvider (LLMComplete)


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
  , ecDevLogConfig :: DevLogConfig
    -- ^ DevLog (session-scoped file logging) configuration
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
  , ecServiceName = "tidepool-native"
  , ecDevLogConfig = defaultDevLogConfig
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
-- * @SERVICE_NAME@ - Service name for traces (default: tidepool-native)
-- * @DEVLOG_DIR@ - DevLog output directory (default: disabled)
-- * @DEVLOG_VERBOSITY@ - Verbosity level: quiet|normal|verbose|trace (default: normal)
-- * @DEVLOG_LATEST@ - Create latest.log symlink: true|false (default: true)
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

  -- DevLog config
  devLogDir <- lookupEnv "DEVLOG_DIR"
  devLogVerbosity <- lookupEnv "DEVLOG_VERBOSITY"
  devLogLatest <- lookupEnv "DEVLOG_LATEST"

  let verbosity = case devLogVerbosity of
        Just "quiet"   -> VQuiet
        Just "verbose" -> VVerbose
        Just "trace"   -> VTrace
        _              -> VNormal

      devLogOutput = case devLogDir of
        Just dir -> OutputFile dir
        Nothing  -> OutputStderr  -- Default to stderr if no dir specified

      createLatest = case devLogLatest of
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
        , lcJobLabel = "tidepool-native"
        }
    , ecOTLPConfig = case otlpEndpoint of
        Just endpoint -> Just OTLPConfig
          { otlpEndpoint = T.pack endpoint
          , otlpUser = T.pack <$> otlpUser
          , otlpToken = T.pack <$> otlpToken
          }
        Nothing -> Nothing
    , ecServiceName = maybe "tidepool-native" T.pack serviceName
    , ecDevLogConfig = DevLogConfig
        { dcVerbosity = verbosity
        , dcOutput = devLogOutput
        , dcSymlinkLatest = createLatest
        , dcSessionId = Nothing  -- Auto-generated
        , dcSessionName = Nothing
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
-- Eff '[UI, Habitica, LLMComplete, DevLog, Observability, IO] a
--   → runObservabilityWithContext (interpret Observability)
--   → runDevLog (interpret DevLog)
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
-- 4. DevLog - session-scoped dev logging
-- 5. Observability (last to peel) - records events and spans
--
-- Traces are automatically flushed to OTLP (Grafana Tempo) after execution
-- if @OTLP_ENDPOINT@ is configured.
--
-- Example:
--
-- @
-- myAgent :: Eff '[UI, Habitica, LLMComplete, DevLog, Observability, IO] String
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
  -> Eff '[UI, Habitica, LLMComplete, DevLog, Observability, IO] a
  -> IO a
runEffects env ctx callback action = do
  -- Create a fresh trace context for this request
  traceCtx <- newTraceContext

  -- Run the effect stack
  result <- runM
    . runObservabilityWithContext traceCtx (ecLokiConfig $ eeConfig env)
    . runDevLog (ecDevLogConfig $ eeConfig env)
    . runLLMComplete (eeLLMEnv env)
    . runHabitica (eeHabiticaEnv env)
    . runUI ctx callback
    $ action

  -- Flush traces to OTLP if configured
  case ecOTLPConfig (eeConfig env) of
    Just otlpConfig ->
      flushTraces otlpConfig (ecServiceName $ eeConfig env) traceCtx
    Nothing ->
      pure ()

  pure result
