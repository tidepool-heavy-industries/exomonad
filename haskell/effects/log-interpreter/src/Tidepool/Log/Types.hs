{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Types for the Log interpreter.
module Tidepool.Log.Types
  ( -- * Log Entry
    LogEntry(..)
    -- * Configuration
  , LogConfig(..)
  , LogOutput(..)
  , defaultLogConfig
  ) where

import Data.Aeson (Value, ToJSON(..), object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Tidepool.Effect.Log (LogLevel(..), LogContext(..))

-- | A structured log entry.
data LogEntry = LogEntry
  { ts :: UTCTime
  , level :: LogLevel
  , msg :: Text
  , fields :: Maybe [(Text, Value)]
  , context :: Maybe LogContext
  }
  deriving (Show, Generic)

instance ToJSON LogEntry where
  toJSON le = object $
    [ "ts" .= le.ts
    , "level" .= le.level
    , "msg" .= le.msg
    ] ++
    (case le.fields of
       Nothing -> []
       Just fs -> map (\(k, v) -> Key.fromText k .= v) fs
    ) ++
    (case le.context of
       Nothing -> []
       Just (LogContext corrId chain _) ->
         [ "corrId" .= corrId
         , "chain" .= chain
         ]
    )

-- | Where to send log output.
data LogOutput
  = LogStderr               -- ^ Write to stderr
  | LogFile FilePath        -- ^ Write to file (creates session-scoped file)
  | LogBoth FilePath        -- ^ Write to both stderr and file
  deriving (Show, Eq, Generic)

-- | Log interpreter configuration.
data LogConfig = LogConfig
  { lcMinLevel      :: LogLevel
    -- ^ Minimum level to emit (default: Info)

  , lcOutput        :: LogOutput
    -- ^ Where to write logs (default: stderr)

  , lcHumanReadable :: Bool
    -- ^ Use human-readable format (default: True)
    -- When False, outputs JSON lines

  , lcSessionId     :: Maybe UUID
    -- ^ Override session ID (auto-generated if Nothing)

  , lcSessionName   :: Maybe Text
    -- ^ Optional session name for log header

  , lcSymlinkLatest :: Bool
    -- ^ Create latest.log symlink (only for file output)
  }
  deriving (Show, Eq, Generic)

-- | Default configuration: Info level, stderr, human-readable.
defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
  { lcMinLevel      = Info
  , lcOutput        = LogStderr
  , lcHumanReadable = True
  , lcSessionId     = Nothing
  , lcSessionName   = Nothing
  , lcSymlinkLatest = False
  }
