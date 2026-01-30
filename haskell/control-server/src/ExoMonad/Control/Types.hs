-- | Shared types for control server.
module ExoMonad.Control.Types
  ( ServerConfig (..),
  )
where

import Data.Text (Text)
import ExoMonad.Control.Hook.CircuitBreaker (CircuitBreakerConfig (..))
import ExoMonad.Control.Hook.Policy (HookPolicy)
import ExoMonad.Control.OpenObserve (OpenObserveConfig)
import ExoMonad.Control.RoleConfig (Role (..))
import ExoMonad.Control.Workflow.Store (WorkflowStore)
import ExoMonad.GitHub.Interpreter (GitHubConfig)
import ExoMonad.LLM.Interpreter.Types (LLMConfig (..))
import ExoMonad.Observability.Types (ObservabilityConfig)
import GHC.Generics (Generic)

-- | Server configuration.
data ServerConfig = ServerConfig
  { -- | Project root directory (where .exomonad/ lives)
    projectDir :: FilePath,
    -- | Current agent role (e.g., "pm", "tl")
    role :: Maybe Text,
    -- | Default role for non-role-prefixed endpoints
    defaultRole :: Role,
    -- | Disable TUI sidebar listener (default: False)
    noTui :: Bool,
    -- | Optional observability configuration (Loki/OTLP)
    observabilityConfig :: Maybe ObservabilityConfig,
    -- | Optional OpenObserve configuration for transcript shipping
    openObserveConfig :: Maybe OpenObserveConfig,
    -- | Hook evaluation policy
    hookPolicy :: HookPolicy,
    -- | Circuit breaker limits
    circuitBreakerConfig :: CircuitBreakerConfig,
    -- | Persistent workflow state
    workflowStore :: WorkflowStore,
    -- | Optional LLM configuration (HTTP or Socket)
    llmConfig :: Maybe LLMConfig,
    -- | Optional GitHub configuration (CLI or Socket)
    githubConfig :: Maybe GitHubConfig
  }
  deriving (Generic)
