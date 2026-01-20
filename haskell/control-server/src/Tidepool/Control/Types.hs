-- | Shared types for control server.
module Tidepool.Control.Types
  ( ServerConfig(..)
  , defaultConfig
  ) where

import Data.Text (Text)

-- | Server configuration.
data ServerConfig = ServerConfig
  { projectDir :: FilePath
    -- ^ Project root directory (where .tidepool/ lives)
  , role       :: Maybe Text
    -- ^ Current agent role (e.g., "pm", "tl")
  }
  deriving stock (Show, Eq)

-- | Default configuration: current directory
defaultConfig :: ServerConfig
defaultConfig = ServerConfig
  { projectDir = "."
  , role       = Nothing
  }
