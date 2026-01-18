-- | Shared types for control server.
module Tidepool.Control.Types
  ( ServerConfig(..)
  , TeachingConfig(..)
  , defaultConfig
  ) where

import Data.Text (Text)

-- | Server configuration.
data ServerConfig = ServerConfig
  { projectDir :: FilePath
    -- ^ Project root directory (where .tidepool/ lives)
  , teachingConfig :: Maybe TeachingConfig
    -- ^ Optional teaching mode configuration
  }
  deriving stock (Show, Eq)

-- | Teaching mode configuration.
data TeachingConfig = TeachingConfig
  { teachOutputDir :: FilePath
    -- ^ Output directory for training data
  , teachAnthropicKey :: Text
    -- ^ Anthropic API key
  }
  deriving stock (Show, Eq)

-- | Default configuration: current directory, no teaching
defaultConfig :: ServerConfig
defaultConfig = ServerConfig
  { projectDir = "."
  , teachingConfig = Nothing
  }
