-- | Shared types for control server.
module Tidepool.Control.Types
  ( ServerConfig(..)
  , defaultConfig
  ) where

-- | Server configuration.
data ServerConfig = ServerConfig
  { projectDir :: FilePath
    -- ^ Project root directory (where .tidepool/ lives)
  }
  deriving stock (Show, Eq)

-- | Default configuration: current directory
defaultConfig :: ServerConfig
defaultConfig = ServerConfig
  { projectDir = "."
  }
