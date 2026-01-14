-- | Configuration types for DevLog interpreter.
module Tidepool.DevLog.Config
  ( DevLogConfig (..)
  , DevLogOutput (..)
  , defaultDevLogConfig
  ) where

import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

import Tidepool.Effect.DevLog (Verbosity (..))

-- | Where to send log output.
data DevLogOutput
  = OutputStderr            -- ^ Write to stderr (for quick debugging)
  | OutputFile FilePath     -- ^ Write to session-scoped file in directory
  | OutputBoth FilePath     -- ^ Write to both stderr and file
  deriving (Show, Eq, Generic)

-- | DevLog interpreter configuration.
data DevLogConfig = DevLogConfig
  { dcVerbosity     :: Verbosity
    -- ^ Minimum verbosity level to emit

  , dcOutput        :: DevLogOutput
    -- ^ Where to write logs

  , dcSymlinkLatest :: Bool
    -- ^ Create latest.log symlink (only for file output)

  , dcSessionId     :: Maybe UUID
    -- ^ Override session ID (auto-generated if Nothing)

  , dcSessionName   :: Maybe Text
    -- ^ Optional session name for log header
  }
  deriving (Show, Eq, Generic)

-- | Default configuration: Normal verbosity, stderr output.
defaultDevLogConfig :: DevLogConfig
defaultDevLogConfig = DevLogConfig
  { dcVerbosity     = VNormal
  , dcOutput        = OutputStderr
  , dcSymlinkLatest = False
  , dcSessionId     = Nothing
  , dcSessionName   = Nothing
  }
