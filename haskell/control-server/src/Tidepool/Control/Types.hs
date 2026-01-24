-- | Shared types for control server.
module Tidepool.Control.Types
  ( ServerConfig(..)
  , defaultConfig
  ) where

import Data.Text (Text)

import Tidepool.Observability.Types (ObservabilityConfig)
import Tidepool.Control.Hook.Policy (HookPolicy, defaultPolicy)
import Tidepool.Control.RoleConfig (Role(..))



-- | Server configuration.

data ServerConfig = ServerConfig

  { projectDir :: FilePath

    -- ^ Project root directory (where .tidepool/ lives)

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

  }
