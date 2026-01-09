-- | Configuration for ClaudeCode executor.
module Tidepool.ClaudeCode.Config
  ( ClaudeCodeConfig(..)
  , defaultClaudeCodeConfig
  , mkClaudeCodeConfig
  ) where

import Data.Text (Text)


-- | Configuration for ClaudeCode executor.
--
-- Used to configure how mantle is invoked.
data ClaudeCodeConfig = ClaudeCodeConfig
  { ccZellijSession :: Text
    -- ^ Zellij session name to attach to
  , ccDefaultTimeout :: Int
    -- ^ Default timeout in seconds (0 = no timeout)
  , ccTempDir :: FilePath
    -- ^ Directory for temporary output files
  , ccZellijCcPath :: FilePath
    -- ^ Path to mantle binary
  }
  deriving (Show, Eq)


-- | Default configuration.
--
-- Uses @mantle@ from PATH, /tmp for output, 5 minute timeout.
-- Session defaults to "tidepool" but should be overridden.
defaultClaudeCodeConfig :: ClaudeCodeConfig
defaultClaudeCodeConfig = ClaudeCodeConfig
  { ccZellijSession = "tidepool"
  , ccDefaultTimeout = 300  -- 5 minutes
  , ccTempDir = "/tmp"
  , ccZellijCcPath = "mantle"
  }

-- | Create config with specific session name.
mkClaudeCodeConfig :: Text -> ClaudeCodeConfig
mkClaudeCodeConfig session = ClaudeCodeConfig
  { ccZellijSession = session
  , ccDefaultTimeout = 300  -- 5 minutes
  , ccTempDir = "/tmp"
  , ccZellijCcPath = "mantle"
  }
