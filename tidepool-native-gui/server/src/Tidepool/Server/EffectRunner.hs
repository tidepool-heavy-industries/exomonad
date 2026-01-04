-- | Effect runner - composes all executors.
--
-- Wires up UI, Habitica, Observability, and LLM executors to run
-- complete agent graphs.
--
-- = Usage
--
-- @
-- import Tidepool.Server.EffectRunner
--
-- config <- loadExecutorConfig
-- env <- mkExecutorEnv config
-- result <- runEffects env ctx callback agentProgram
-- @
module Tidepool.Server.EffectRunner
  ( -- * Configuration
    ExecutorConfig(..)
  , defaultExecutorConfig
  , loadExecutorConfig

    -- * Environment
  , ExecutorEnv(..)
  , mkExecutorEnv

    -- * Running Effects
  , runEffects
  , runUIOnly
  ) where

import Control.Monad.Freer (Eff, runM)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)

-- Executor imports
import Tidepool.UI.Executor
  ( UIContext
  , UICallback
  , newUIContext
  , runUI
  )
import Tidepool.Habitica.Executor
  ( HabiticaConfig(..)
  , HabiticaEnv
  , mkHabiticaEnv
  , runHabitica
  , defaultHabiticaConfig
  )
import Tidepool.Observability.Executor
  ( LokiConfig(..)
  , runObservability
  , defaultLokiConfig
  )
import Tidepool.Observability.Types (SpanContext, newSpanContext)
import Tidepool.LLM.Executor
  ( LLMSecrets(..)
  , runLLMComplete
  )

-- Effect imports
import Tidepool.Effects.UI (UI)
import Tidepool.Effects.Habitica (Habitica)
import Tidepool.Effects.Observability (Observability)
import Tidepool.Effects.LLMProvider (LLMComplete)


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for all executors.
data ExecutorConfig = ExecutorConfig
  { ecLLMSecrets :: LLMSecrets
    -- ^ API keys for LLM providers
  , ecHabiticaConfig :: HabiticaConfig
    -- ^ Habitica API configuration
  , ecLokiConfig :: LokiConfig
    -- ^ Loki/Grafana observability configuration
  }
  deriving stock (Show, Eq)

-- | Default configuration with empty credentials.
--
-- Use 'loadExecutorConfig' to load from environment variables.
defaultExecutorConfig :: ExecutorConfig
defaultExecutorConfig = ExecutorConfig
  { ecLLMSecrets = LLMSecrets
      { lsAnthropicKey = Nothing
      , lsOpenAIKey = Nothing
      }
  , ecHabiticaConfig = defaultHabiticaConfig
  , ecLokiConfig = defaultLokiConfig
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
loadExecutorConfig :: IO ExecutorConfig
loadExecutorConfig = do
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

  pure ExecutorConfig
    { ecLLMSecrets = LLMSecrets
        { lsAnthropicKey = T.pack <$> anthropicKey
        , lsOpenAIKey = T.pack <$> openaiKey
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
    }


-- ════════════════════════════════════════════════════════════════════════════
-- ENVIRONMENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Runtime environment with initialized resources.
data ExecutorEnv = ExecutorEnv
  { eeConfig :: ExecutorConfig
    -- ^ Original configuration
  , eeHabiticaEnv :: HabiticaEnv
    -- ^ Initialized Habitica client (with HTTP manager)
  , eeSpanContext :: SpanContext
    -- ^ Observability span context
  }

-- | Create a new executor environment.
--
-- Initializes HTTP managers and other resources needed by executors.
mkExecutorEnv :: ExecutorConfig -> IO ExecutorEnv
mkExecutorEnv config = do
  habiticaEnv <- mkHabiticaEnv (ecHabiticaConfig config)
  spanCtx <- newSpanContext
  pure ExecutorEnv
    { eeConfig = config
    , eeHabiticaEnv = habiticaEnv
    , eeSpanContext = spanCtx
    }


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT RUNNERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a UI-only program.
--
-- This is useful for simple agents that only use UI effects without
-- LLM, Habitica, or observability.
runUIOnly :: UIContext -> UICallback -> Eff '[UI] a -> IO a
runUIOnly = runUI

-- | Run a program with all effects composed.
--
-- This composes the full effect stack:
--
-- @
-- Eff '[UI, Habitica, LLMComplete, Observability] a
--   → runObservability (interpret Observability)
--   → runLLMComplete (interpret LLMComplete)
--   → runHabitica (interpret Habitica)
--   → runUI (interpret UI)
--   → IO a
-- @
--
-- Note: Effect stack ordering matters. Effects are peeled from the outside in:
-- 1. Observability (outermost) - records spans/events
-- 2. LLMComplete - makes LLM API calls
-- 3. Habitica - makes Habitica API calls
-- 4. UI (innermost) - handles user interaction
--
-- Example:
--
-- @
-- myAgent :: Eff '[UI, Habitica, LLMComplete, Observability] String
-- myAgent = do
--   publishEvent $ GraphTransition "entry" "greeting" "start"
--   showText "Welcome!"
--   name <- requestTextInput "What's your name?"
--   pure $ "Hello, " <> name
--
-- result <- runEffects env ctx callback myAgent
-- @
runEffects
  :: ExecutorEnv
  -> UIContext
  -> UICallback
  -> Eff '[UI, Habitica, LLMComplete, Observability] a
  -> IO a
runEffects env ctx callback program = do
  -- We need to interpret effects from inside out (right to left in type)
  -- But the current executor types have constraints that make this tricky.
  --
  -- For now, we provide a simplified runner that handles the most common case:
  -- UI effects only. Full composition requires aligning effect interpreters.
  --
  -- TODO: Implement full effect composition with proper interpreter stacking
  error "Full effect composition not yet implemented - use runUIOnly for UI-only agents"


-- ════════════════════════════════════════════════════════════════════════════
-- NOTES ON EFFECT COMPOSITION
-- ════════════════════════════════════════════════════════════════════════════

-- The executors have different type signatures:
--
-- runUI :: UIContext -> UICallback -> Eff '[UI] a -> IO a
--   - Interprets UI into IO directly
--   - No effect stack polymorphism
--
-- runHabitica :: LastMember IO effs => HabiticaEnv -> Eff (Habitica ': effs) a -> Eff effs a
--   - Polymorphic over remaining effects
--   - Requires IO at the bottom
--
-- runLLMComplete :: LastMember IO effs => LLMSecrets -> Eff (LLMComplete ': effs) a -> Eff effs a
--   - Same pattern as Habitica
--
-- runObservability :: LokiConfig -> Eff '[Observability] a -> IO a
--   - Like UI, interprets directly to IO
--
-- To compose these properly, we need either:
-- 1. Make all interpreters polymorphic (runHabitica pattern)
-- 2. Use a different composition strategy (e.g., interpret each in sequence)
-- 3. Create a unified effect stack with custom handling
--
-- For now, agents should use individual executors or the simplified runUIOnly.
