{-# LANGUAGE OverloadedStrings #-}

-- | Structured logging for control-server using fast-logger.
--
-- Provides timestamped, buffered logging to stderr and file with log levels.
-- Uses fast-logger for efficient buffered output.
--
-- Two initialization modes:
--   * 'withStderrLogger' - stderr only (for testing)
--   * 'withDualLogger' - stderr + session file (for production)
module Tidepool.Control.Logging
  ( -- * Logger types
    Logger
    -- * Initialization
  , withStderrLogger
  , withDualLogger
    -- * Logging functions
  , logInfo
  , logDebug
  , logError
  , logWarn
  ) where

import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Log.FastLogger
  ( LoggerSet
  , FormattedTime
  , newStderrLoggerSet
  , newFileLoggerSet
  , defaultBufSize
  , toLogStr
  , rmLoggerSet
  , pushLogStrLn
  , flushLogStr
  )
import System.Log.FastLogger.Date (newTimeCache, simpleTimeFormat)

-- | Logger contains dual logger sets (stderr + file) and time cache.
--
-- For stderr-only mode (testing), both fields point to the same LoggerSet.
data Logger = Logger
  { stderrLoggerSet :: LoggerSet
  , fileLoggerSet   :: LoggerSet
  , timeCache       :: IO FormattedTime
  }

-- | Generate session log file path with timestamp.
--
-- Format: @projectDir/.tidepool/logs/control-server-YYYY-MM-DDTHH-MM-SS.log@
sessionLogPath :: FilePath -> IO FilePath
sessionLogPath projectDir = do
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" now
  pure $ projectDir </> ".tidepool" </> "logs" </> ("control-server-" <> timestamp <> ".log")

-- | Run action with dual logger (stderr + file), ensuring cleanup.
--
-- Creates a logger that outputs to both stderr (real-time) and a timestamped
-- session log file (persistent). Log directory is created if it doesn't exist.
--
-- Session files are named: @control-server-YYYY-MM-DDTHH-MM-SS.log@
withDualLogger :: FilePath -> (Logger -> IO a) -> IO a
withDualLogger projectDir action = do
  let logsDir = projectDir </> ".tidepool" </> "logs"
  createDirectoryIfMissing True logsDir
  logPath <- sessionLogPath projectDir
  tc <- newTimeCache simpleTimeFormat
  bracket
    ((,) <$> newStderrLoggerSet defaultBufSize
         <*> newFileLoggerSet defaultBufSize logPath)
    (\(stderrLS, fileLS) -> do
        flushLogStr stderrLS >> flushLogStr fileLS
        rmLoggerSet stderrLS >> rmLoggerSet fileLS)
    (\(stderrLS, fileLS) -> action (Logger stderrLS fileLS tc))

-- | Run action with stderr logger, ensuring cleanup.
--
-- Creates a logger that outputs to stderr only (both logger set fields
-- point to the same stderr LoggerSet). Used for testing.
withStderrLogger :: (Logger -> IO a) -> IO a
withStderrLogger action = do
  tc <- newTimeCache simpleTimeFormat
  bracket
    (newStderrLoggerSet defaultBufSize)
    (\ls -> flushLogStr ls >> rmLoggerSet ls)
    (\ls -> action (Logger ls ls tc))

-- | Log at INFO level.
--
-- Format: @[timestamp] [INFO] message@
logInfo :: Logger -> Text -> IO ()
logInfo logger msg = logWithLevel logger "INFO" msg

-- | Log at DEBUG level.
--
-- Format: @[timestamp] [DEBUG] message@
logDebug :: Logger -> Text -> IO ()
logDebug logger msg = logWithLevel logger "DEBUG" msg

-- | Log at ERROR level.
--
-- Format: @[timestamp] [ERROR] message@
logError :: Logger -> Text -> IO ()
logError logger msg = logWithLevel logger "ERROR" msg

-- | Log at WARN level.
--
-- Format: @[timestamp] [WARN] message@
logWarn :: Logger -> Text -> IO ()
logWarn logger msg = logWithLevel logger "WARN" msg

-- | Internal: Log with specified level to both stderr and file.
logWithLevel :: Logger -> Text -> Text -> IO ()
logWithLevel (Logger stderrLS fileLS tc) level msg = do
  time <- tc
  let logMsg = toLogStr time <> " [" <> toLogStr (T.encodeUtf8 level) <> "] "
            <> toLogStr (T.encodeUtf8 msg)
  pushLogStrLn stderrLS logMsg
  pushLogStrLn fileLS logMsg
