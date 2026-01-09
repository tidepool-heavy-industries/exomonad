-- | DevLog effect executor - session-scoped file logging.
--
-- = Usage
--
-- @
-- import Tidepool.Effect.DevLog
-- import Tidepool.DevLog.Executor
-- import Tidepool.DevLog.Config
--
-- main :: IO ()
-- main = do
--   let config = defaultDevLogConfig
--         { dcOutput = OutputFile "./logs"
--         , dcSymlinkLatest = True
--         }
--   runM $ runDevLog config $ do
--     devLogGraph GraphTransitionInfo { ... }
--     devLogLLMRequest VNormal LLMRequestInfo { ... }
-- @
--
-- = Greppable Patterns
--
-- @
-- grep "GRAPH" logs/latest.log        # All graph transitions
-- grep "STATE\\." logs/latest.log     # All state changes
-- grep "LLM\\." logs/latest.log       # All LLM calls
-- grep "ERROR" logs/latest.log        # All errors
-- grep "→ bargain" logs/latest.log    # Transitions to specific node
-- @
module Tidepool.DevLog.Executor
  ( -- * Executor
    runDevLog
  , runDevLogWithHandle

    -- * Re-exports
  , module Tidepool.DevLog.Config
  ) where

import Control.Monad (when)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>))
import System.IO (Handle, hPutStrLn, stderr, openFile, IOMode(..), hFlush, hClose)
import System.Posix.Files (createSymbolicLink)
import Control.Exception (catch, SomeException)

import Tidepool.Effect.DevLog
import Tidepool.DevLog.Config
import Tidepool.DevLog.Formatter

-- | Run DevLog effect with the given configuration.
--
-- Creates session-scoped log file if configured, writes header,
-- filters events by verbosity, and formats output.
runDevLog :: LastMember IO effs => DevLogConfig -> Eff (DevLog ': effs) a -> Eff effs a
runDevLog config action = do
  -- Generate session ID
  sessionId <- sendM $ maybe UUID.nextRandom pure config.dcSessionId
  now <- sendM getCurrentTime

  case config.dcOutput of
    OutputStderr -> do
      sendM $ TIO.hPutStr stderr $ formatSessionHeader sessionId config.dcSessionName now
      runDevLogWithHandle config stderr action

    OutputFile logDir -> do
      (handle, sessionFile) <- sendM $ openSessionFile logDir sessionId
      sendM $ TIO.hPutStr handle $ formatSessionHeader sessionId config.dcSessionName now
      result <- runDevLogWithHandle config handle action
      sendM $ do
        hClose handle
        when config.dcSymlinkLatest $
          createLatestSymlink logDir sessionFile
      pure result

    OutputBoth logDir -> do
      (handle, sessionFile) <- sendM $ openSessionFile logDir sessionId
      let header = formatSessionHeader sessionId config.dcSessionName now
      sendM $ do
        TIO.hPutStr stderr header
        TIO.hPutStr handle header
      result <- runDevLogWithHandleBoth config stderr handle action
      sendM $ do
        hClose handle
        when config.dcSymlinkLatest $
          createLatestSymlink logDir sessionFile
      pure result

-- | Core interpreter that writes to a handle.
runDevLogWithHandle
  :: LastMember IO effs
  => DevLogConfig
  -> Handle
  -> Eff (DevLog ': effs) a
  -> Eff effs a
runDevLogWithHandle config handle = interpret $ \case
  LogDevEvent event ->
    when (eventVerbosity event >= config.dcVerbosity) $ sendM $ do
      now <- getCurrentTime
      let formatted = formatEvent now event
      TIO.hPutStrLn handle formatted
      hFlush handle

-- | Interpreter that writes to both stderr and file.
runDevLogWithHandleBoth
  :: LastMember IO effs
  => DevLogConfig
  -> Handle  -- stderr
  -> Handle  -- file
  -> Eff (DevLog ': effs) a
  -> Eff effs a
runDevLogWithHandleBoth config stderrH fileH = interpret $ \case
  LogDevEvent event ->
    when (eventVerbosity event >= config.dcVerbosity) $ sendM $ do
      now <- getCurrentTime
      let formatted = formatEvent now event
      TIO.hPutStrLn stderrH formatted
      TIO.hPutStrLn fileH formatted
      hFlush stderrH
      hFlush fileH

-- | Extract verbosity requirement from event.
eventVerbosity :: DevLogEvent -> Verbosity
eventVerbosity = \case
  EventGraph v _      -> v
  EventState v _      -> v
  EventLLMRequest v _ -> v
  EventLLMResponse v _ -> v
  EventError _        -> VQuiet  -- Always emit errors
  EventRaw v _        -> v

-- | Open a new session log file.
openSessionFile :: FilePath -> UUID -> IO (Handle, FilePath)
openSessionFile logDir sessionId = do
  createDirectoryIfMissing True logDir
  now <- getCurrentTime
  let filename = formatSessionFilename sessionId now
      sessionFile = logDir </> filename
  handle <- openFile sessionFile WriteMode
  pure (handle, sessionFile)

-- | Format session filename: session-{uuid8}-{timestamp}.log
formatSessionFilename :: UUID -> a -> FilePath
formatSessionFilename sessionId _ =
  "session-" <> take 8 (show sessionId) <> ".log"

-- | Create latest.log symlink.
createLatestSymlink :: FilePath -> FilePath -> IO ()
createLatestSymlink logDir sessionFile = do
  let latestLink = logDir </> "latest.log"
  -- Remove existing symlink if present
  removeFile latestLink `catch` \(_ :: SomeException) -> pure ()
  -- Create relative symlink
  let target = takeFileName' sessionFile
  createSymbolicLink target latestLink
  where
    takeFileName' = reverse . takeWhile (/= '/') . reverse
