-- | Shared types for control server.
module ExoMonad.Control.Types
  ( ServerConfig(..)
  , defaultConfig
  ) where

import Data.Text (Text)

import ExoMonad.Observability.Types (ObservabilityConfig)
import ExoMonad.Control.Hook.Policy (HookPolicy, defaultPolicy)
import ExoMonad.Control.RoleConfig (Role(..))
import ExoMonad.Control.Hook.CircuitBreaker (CircuitBreakerConfig(..))


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
  , hookPolicy :: HookPolicy
    -- ^ Hook evaluation policy
  , circuitBreakerConfig :: CircuitBreakerConfig
    -- ^ Circuit breaker limits
  }
  deriving stock (Show, Eq)


-- | Default configuration: current directory
defaultConfig :: ServerConfig
defaultConfig = ServerConfig
  { projectDir = "."
  , role       = Nothing
  , defaultRole = Dev
  , noTui      = False
  , observabilityConfig = Nothing
  , hookPolicy = defaultPolicy
  , circuitBreakerConfig = CircuitBreakerConfig 15 5 300
  }
