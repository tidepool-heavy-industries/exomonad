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
import Tidepool.LLM.Executor
  ( LLMEnv
  , LLMConfig(..)
  , mkLLMEnv
  , runLLMComplete
  )
import Tidepool.LLM.Types
  ( AnthropicSecrets(..)
  , OpenAISecrets(..)
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
  { ecLLMConfig :: LLMConfig
    -- ^ LLM provider configuration
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
  { ecLLMConfig = LLMConfig
      { lcAnthropicSecrets = Nothing
      , lcOpenAISecrets = Nothing
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
    { ecLLMConfig = LLMConfig
        { lcAnthropicSecrets = case anthropicKey of
            Just k -> Just AnthropicSecrets
              { asApiKey = T.pack k
              , asBaseUrl = "https://api.anthropic.com"
              }
            Nothing -> Nothing
        , lcOpenAISecrets = case openaiKey of
            Just k -> Just OpenAISecrets
              { osApiKey = T.pack k
              , osBaseUrl = "https://api.openai.com"
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
    }


-- ════════════════════════════════════════════════════════════════════════════
-- ENVIRONMENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Runtime environment with initialized resources.
data ExecutorEnv = ExecutorEnv
  { eeConfig :: ExecutorConfig
    -- ^ Original configuration
  , eeLLMEnv :: LLMEnv
    -- ^ Initialized LLM client (with HTTP manager)
  , eeHabiticaEnv :: HabiticaEnv
    -- ^ Initialized Habitica client (with HTTP manager)
  }

-- | Create a new executor environment.
--
-- Initializes HTTP managers and other resources needed by executors.
mkExecutorEnv :: ExecutorConfig -> IO ExecutorEnv
mkExecutorEnv config = do
  llmEnv <- mkLLMEnv (ecLLMConfig config)
  habiticaEnv <- mkHabiticaEnv (ecHabiticaConfig config)
  pure ExecutorEnv
    { eeConfig = config
    , eeLLMEnv = llmEnv
    , eeHabiticaEnv = habiticaEnv
    }


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT RUNNERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a UI-only program.
--
-- This is useful for simple agents that only use UI effects without
-- LLM, Habitica, or observability.
--
-- Note: The effect stack must end with IO for the interpreters to work.
-- Agents use polymorphic types like @Member UI effs => Eff effs a@
-- which get instantiated to this concrete stack when run.
runUIOnly :: UIContext -> UICallback -> Eff '[UI, IO] a -> IO a
runUIOnly ctx callback = runM . runUI ctx callback

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
-- 1. UI (first to peel) - handles user interaction
-- 2. Habitica - makes Habitica API calls
-- 3. LLMComplete - makes LLM API calls
-- 4. Observability (last to peel) - records events
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
  -> Eff '[UI, Habitica, LLMComplete, Observability, IO] a
  -> IO a
runEffects env ctx callback =
  runM
    . runObservability (ecLokiConfig $ eeConfig env)
    . runLLMComplete (eeLLMEnv env)
    . runHabitica (eeHabiticaEnv env)
    . runUI ctx callback
