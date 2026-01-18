-- | Shared types for control server.
module Tidepool.Control.Types
  ( ServerConfig(..)
  , TeachingSettings(..)
  , defaultConfig
  ) where

import Data.Text (Text)
import Tidepool.Teaching.Types (AnthropicApiKey)

-- | Server configuration.
data ServerConfig = ServerConfig
  { projectDir :: FilePath
    -- ^ Project root directory (where .tidepool/ lives)
  , teachingSettings :: Maybe TeachingSettings
    -- ^ Optional teaching mode configuration
  }
  deriving stock (Show, Eq)

-- | Teaching mode settings (CLI args only).
data TeachingSettings = TeachingSettings
  { tsOutputDir :: FilePath
    -- ^ Output directory for training data
  , tsAnthropicKey :: AnthropicApiKey
    -- ^ Anthropic API key
  }
  deriving stock (Show, Eq)

-- | Default configuration: current directory, no teaching
defaultConfig :: ServerConfig
defaultConfig = ServerConfig
  { projectDir = "."
  , teachingSettings = Nothing
  }
