-- | Shared types for control server.
module ExoMonad.Control.Types
  ( ServerConfig(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

import ExoMonad.Observability.Types (ObservabilityConfig)
import ExoMonad.Control.OpenObserve (OpenObserveConfig)
import ExoMonad.Control.Hook.Policy (HookPolicy)
import ExoMonad.Control.RoleConfig (Role(..))
import ExoMonad.Control.Hook.CircuitBreaker (CircuitBreakerConfig(..))
import ExoMonad.Control.Workflow.Store (WorkflowStore)
import ExoMonad.LLM.Interpreter.Types (LLMConfig(..))
import ExoMonad.GitHub.Interpreter (GitHubConfig)

-- | Server configuration.
data ServerConfig = ServerConfig
  { projectDir :: FilePath
    -- ^ Project root directory (where .exomonad/ lives)
  , role       :: Maybe Text
    -- ^ Current agent role (e.g., "pm", "tl")
  , defaultRole :: Role
    -- ^ Default role for non-role-prefixed endpoints
  , noTui      :: Bool
    -- ^ Disable TUI sidebar listener (default: False)
  , observabilityConfig :: Maybe ObservabilityConfig
    -- ^ Optional observability configuration (Loki/OTLP)
  , openObserveConfig :: Maybe OpenObserveConfig
    -- ^ Optional OpenObserve configuration for transcript shipping
  , hookPolicy :: HookPolicy
    -- ^ Hook evaluation policy
  , circuitBreakerConfig :: CircuitBreakerConfig
    -- ^ Circuit breaker limits
  , workflowStore :: WorkflowStore
    -- ^ Persistent workflow state
  , llmConfig :: Maybe LLMConfig
    -- ^ Optional LLM configuration (HTTP or Socket)
  , githubConfig :: Maybe GitHubConfig
    -- ^ Optional GitHub configuration (CLI or Socket)
  } deriving (Generic)
