{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Log effect interpreter using fast-logger.
--
-- = Usage
--
-- @
-- import Tidepool.Effect.Log
-- import Tidepool.Log.Interpreter
-- import Tidepool.Log.Types
--
-- main :: IO ()
-- main = do
--   let config = defaultLogConfig
--         { lcOutput = LogFile "./logs"
--         , lcSymlinkLatest = True
--         }
--   withLogInterpreter config $ \runLog -> do
--     runM $ runLog $ do
--       logInfo "Starting up"
--       logGraph GraphTransitionInfo { ... }
-- @
--
-- = Greppable Patterns
--
-- @
-- grep "GRAPH" logs/latest.log        # All graph transitions
-- grep "STATE\\." logs/latest.log     # All state changes
-- grep "LLM\\." logs/latest.log       # All LLM calls
-- grep "ERROR" logs/latest.log        # All errors
-- @
module Tidepool.Log.Interpreter
  ( -- * Interpreter
    runLogFastLogger
  , withLogInterpreter
    -- * Simple Interpreters (no fast-logger)
  , runLogToStderr
  , runLogStructured
    -- * Re-exports
  , module Tidepool.Log.Types
  ) where

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Freer (Eff, Member, interpret, sendM, LastMember)
import Control.Monad.Freer.Reader (Reader, ask)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>))
import System.IO (Handle, stderr, hFlush)
import qualified System.IO as IO
import System.Log.FastLogger
  ( LoggerSet, newStderrLoggerSet, newFileLoggerSet
  , pushLogStrLn, flushLogStr, rmLoggerSet
  , toLogStr, LogStr
  )
import System.Posix.Files (createSymbolicLink)
import Control.Exception (catch, SomeException)

import Tidepool.Effect.Log (Log(..), LogLevel(..), LogContext)
import Tidepool.Log.Types
import Tidepool.Log.Formatter

-- | Buffer size for fast-logger (4KB default).
defaultBufSize :: Int
defaultBufSize = 4096

-- | Run Log effect with fast-logger backend.
--
-- This is the high-performance interpreter. For bracket-style resource
-- management, use 'withLogInterpreter'.
runLogFastLogger
  :: (Member (Reader LogContext) es, LastMember IO es)
  => LogConfig
  -> LoggerSet
  -> Eff (Log ': es) a
  -> Eff es a
runLogFastLogger config loggerSet = interpret $ \case
  LogMsg level msg maybeFields
    | level >= config.lcMinLevel -> do
        ctx <- ask
        now <- sendM getCurrentTime
        let entry = LogEntry
              { ts = now
              , level = level
              , msg = msg
              , fields = maybeFields
              , context = Just ctx
              }
            formatted = formatLogEntry config.lcHumanReadable entry
        sendM $ pushLogStrLn loggerSet (toLogStr $ TE.encodeUtf8 formatted)
    | otherwise -> pure ()

-- | Bracket-style interpreter with automatic resource cleanup.
--
-- Creates logger set, runs action, flushes and cleans up.
--
-- @
-- withLogInterpreter config $ \runLog -> do
--   result <- runM $ runLog $ do
--     logInfo "Hello"
--   pure result
-- @
withLogInterpreter
  :: LogConfig
  -> ((forall es a. (Member (Reader LogContext) es, LastMember IO es)
       => Eff (Log ': es) a -> Eff es a) -> IO b)
  -> IO b
withLogInterpreter config action = do
  sessionId <- maybe UUID.nextRandom pure config.lcSessionId
  now <- getCurrentTime

  case config.lcOutput of
    LogStderr -> do
      loggerSet <- newStderrLoggerSet defaultBufSize
      bracket
        (pure loggerSet)
        (\ls -> flushLogStr ls >> rmLoggerSet ls)
        (\ls -> do
          -- Write session header
          let header = formatSessionHeader sessionId config.lcSessionName now
          pushLogStrLn ls (toLogStr $ TE.encodeUtf8 header)
          action (runLogFastLogger config ls)
        )

    LogFile logDir -> do
      createDirectoryIfMissing True logDir
      let filename = formatSessionFilename sessionId
          sessionFile = logDir </> filename
      loggerSet <- newFileLoggerSet defaultBufSize sessionFile
      bracket
        (pure loggerSet)
        (\ls -> do
          flushLogStr ls
          rmLoggerSet ls
          when config.lcSymlinkLatest $
            createLatestSymlink logDir filename
        )
        (\ls -> do
          let header = formatSessionHeader sessionId config.lcSessionName now
          pushLogStrLn ls (toLogStr $ TE.encodeUtf8 header)
          action (runLogFastLogger config ls)
        )

    LogBoth logDir -> do
      createDirectoryIfMissing True logDir
      let filename = formatSessionFilename sessionId
          sessionFile = logDir </> filename
      stderrSet <- newStderrLoggerSet defaultBufSize
      fileSet <- newFileLoggerSet defaultBufSize sessionFile
      bracket
        (pure (stderrSet, fileSet))
        (\(ss, fs) -> do
          flushLogStr ss >> rmLoggerSet ss
          flushLogStr fs >> rmLoggerSet fs
          when config.lcSymlinkLatest $
            createLatestSymlink logDir filename
        )
        (\(ss, fs) -> do
          let header = formatSessionHeader sessionId config.lcSessionName now
              headerStr = toLogStr $ TE.encodeUtf8 header
          pushLogStrLn ss headerStr
          pushLogStrLn fs headerStr
          action (runLogFastLoggerDual config ss fs)
        )

-- | Run Log effect writing to two logger sets (stderr + file).
runLogFastLoggerDual
  :: (Member (Reader LogContext) es, LastMember IO es)
  => LogConfig
  -> LoggerSet  -- ^ stderr
  -> LoggerSet  -- ^ file
  -> Eff (Log ': es) a
  -> Eff es a
runLogFastLoggerDual config stderrSet fileSet = interpret $ \case
  LogMsg level msg maybeFields
    | level >= config.lcMinLevel -> do
        ctx <- ask
        now <- sendM getCurrentTime
        let entry = LogEntry
              { ts = now
              , level = level
              , msg = msg
              , fields = maybeFields
              , context = Just ctx
              }
            formatted = toLogStr $ TE.encodeUtf8 $ formatLogEntry config.lcHumanReadable entry
        sendM $ do
          pushLogStrLn stderrSet formatted
          pushLogStrLn fileSet formatted
    | otherwise -> pure ()

-- ══════════════════════════════════════════════════════════════
-- SIMPLE INTERPRETERS (for backward compatibility)
-- ══════════════════════════════════════════════════════════════

-- | Simple JSON structured logging to a handle.
-- Requires Reader LogContext in the stack.
runLogStructured
  :: (Member (Reader LogContext) es, LastMember IO es)
  => Handle -> Eff (Log ': es) a -> Eff es a
runLogStructured handle = interpret $ \case
  LogMsg level msg maybeFields -> do
    ctx <- ask
    now <- sendM getCurrentTime
    let entry = LogEntry
          { ts = now
          , level = level
          , msg = msg
          , fields = maybeFields
          , context = Just ctx
          }
    sendM $ do
      IO.hPutStrLn handle $ T.unpack $ formatLogEntryJSON entry
      hFlush handle

-- | Convenient wrapper for logging to stderr.
runLogToStderr
  :: (Member (Reader LogContext) es, LastMember IO es)
  => Eff (Log ': es) a -> Eff es a
runLogToStderr = runLogStructured stderr

-- ══════════════════════════════════════════════════════════════
-- HELPERS
-- ══════════════════════════════════════════════════════════════

-- | Format session filename: session-{uuid8}.log
formatSessionFilename :: UUID -> FilePath
formatSessionFilename sessionId =
  "session-" <> take 8 (show sessionId) <> ".log"

-- | Create latest.log symlink.
createLatestSymlink :: FilePath -> FilePath -> IO ()
createLatestSymlink logDir filename = do
  let latestLink = logDir </> "latest.log"
  -- Remove existing symlink if present
  removeFile latestLink `catch` \(_ :: SomeException) -> pure ()
  -- Create relative symlink
  createSymbolicLink filename latestLink
