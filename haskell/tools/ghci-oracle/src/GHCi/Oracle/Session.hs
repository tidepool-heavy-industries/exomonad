-- | GHCi session management.
--
-- Handles spawning, communicating with, and restarting GHCi subprocess.
module GHCi.Oracle.Session
  ( -- * Session Types
    GHCiSession (..),
    SessionState (..),

    -- * Session Handle
    SessionHandle,
    newSessionHandle,
    getSessionState,

    -- * Session Lifecycle
    startSession,
    stopSession,
    restartSession,

    -- * Query Execution
    execQuery,
    withSession,
  )
where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forever, void, when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHCi.Oracle.Types (GHCiError (..), OracleConfig (..), isLoadError)
import System.Exit (ExitCode (..))
import System.IO
  ( BufferMode (..),
    Handle,
    hClose,
    hFlush,
    hGetLine,
    hPutStrLn,
    hSetBuffering,
  )
import System.Process
  ( CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    createProcess,
    getProcessExitCode,
    shell,
    terminateProcess,
    waitForProcess,
  )
import System.Timeout (timeout)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Active GHCi session with subprocess handles.
data GHCiSession = GHCiSession
  { -- | Write queries here
    gsStdin :: Handle,
    -- | Read responses here
    gsStdout :: Handle,
    -- | Read errors here
    gsStderr :: Handle,
    -- | Subprocess handle
    gsProcess :: ProcessHandle,
    -- | Background thread draining stderr
    gsStderrReader :: ThreadId
  }

-- | Session lifecycle state.
data SessionState
  = SessionNotStarted
  | SessionStarting
  | SessionRunning GHCiSession
  | SessionCrashed Text Int -- last output, exit code or -1
  | SessionStopped
  deriving stock (Show)

instance Show GHCiSession where
  show _ = "<GHCiSession>"

-- | Thread-safe handle to GHCi session.
data SessionHandle = SessionHandle
  { -- | Mutex for query serialization
    shMutex :: MVar (),
    -- | Current session state
    shState :: TVar SessionState,
    -- | Configuration
    shConfig :: OracleConfig,
    -- | Number of restarts attempted
    shRestartCount :: TVar Int,
    -- | Last output (for crash diagnostics)
    shLastOutput :: TVar Text
  }

-- | Custom prompt marker for boundary detection.
promptMarker :: Text
promptMarker = "<<<GHCI_READY>>>"

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLE CREATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Create a new session handle (does not start session).
newSessionHandle :: OracleConfig -> IO SessionHandle
newSessionHandle config = do
  mutex <- newMVar ()
  state <- newTVarIO SessionNotStarted
  restarts <- newTVarIO 0
  lastOut <- newTVarIO ""
  pure
    SessionHandle
      { shMutex = mutex,
        shState = state,
        shConfig = config,
        shRestartCount = restarts,
        shLastOutput = lastOut
      }

-- | Get current session state.
getSessionState :: SessionHandle -> IO SessionState
getSessionState = atomically . readTVar . shState

-- ════════════════════════════════════════════════════════════════════════════
-- SESSION LIFECYCLE
-- ════════════════════════════════════════════════════════════════════════════

-- | Start a new GHCi session.
--
-- 1. Spawns ghci subprocess
-- 2. Sets custom prompt for boundary detection
-- 3. Loads initial modules
-- 4. Waits for ready prompt
startSession :: SessionHandle -> IO (Either GHCiError ())
startSession handle = do
  let config = shConfig handle

  atomically $ writeTVar (shState handle) SessionStarting

  result <- try $ do
    -- Spawn process
    (Just stdin, Just stdout, Just stderr, ph) <-
      createProcess
        (shell $ ocGhciCommand config)
          { cwd = Just $ ocProjectRoot config,
            std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = CreatePipe
          }

    -- Set buffering for immediate IO
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    -- Start stderr reader thread (drains to prevent blocking)
    stderrTid <- forkIO $ stderrReader handle stderr

    let session =
          GHCiSession
            { gsStdin = stdin,
              gsStdout = stdout,
              gsStderr = stderr,
              gsProcess = ph,
              gsStderrReader = stderrTid
            }

    -- Set custom prompt for boundary detection
    setPrompt session

    -- Wait for initial prompt with timeout
    let timeoutUs = ocStartupTimeoutMs config * 1000
    promptResult <- timeout timeoutUs $ waitForPrompt session

    case promptResult of
      Nothing -> do
        terminateSession session
        pure $ Left $ GHCiTimeout ":startup" (ocStartupTimeoutMs config)
      Just () -> do
        -- Load initial modules
        loadResult <- loadInitialModules handle session (ocInitialModules config)
        case loadResult of
          Left err -> do
            terminateSession session
            pure $ Left err
          Right () -> do
            atomically $ writeTVar (shState handle) (SessionRunning session)
            pure $ Right ()

  case result of
    Left (e :: SomeException) -> do
      atomically $ writeTVar (shState handle) (SessionCrashed (T.pack $ show e) (-1))
      pure $ Left $ GHCiSessionCrashed (T.pack $ show e) Nothing
    Right inner -> pure inner

-- | Stop the GHCi session.
stopSession :: SessionHandle -> IO ()
stopSession handle = do
  state <- atomically $ readTVar (shState handle)
  case state of
    SessionRunning session -> do
      terminateSession session
      atomically $ writeTVar (shState handle) SessionStopped
    _ -> pure ()

-- | Restart session after crash.
restartSession :: SessionHandle -> IO (Either GHCiError ())
restartSession handle = do
  let config = shConfig handle

  -- Check restart limit
  count <- atomically $ do
    c <- readTVar (shRestartCount handle)
    if c >= ocMaxRestarts config
      then pure c
      else do
        writeTVar (shRestartCount handle) (c + 1)
        pure c

  if count >= ocMaxRestarts config
    then pure $ Left $ GHCiSessionCrashed "Max restart attempts reached" Nothing
    else do
      -- Stop existing session if any
      stopSession handle
      -- Start fresh
      startSession handle

-- ════════════════════════════════════════════════════════════════════════════
-- QUERY EXECUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Execute a query with the session lock.
withSession ::
  SessionHandle ->
  (GHCiSession -> IO (Either GHCiError a)) ->
  IO (Either GHCiError a)
withSession handle action = withMVar (shMutex handle) $ \_ -> do
  state <- atomically $ readTVar (shState handle)
  case state of
    SessionNotStarted -> do
      -- Auto-start on first use
      startResult <- startSession handle
      case startResult of
        Left err -> pure $ Left err
        Right () -> withSession' handle action
    SessionStarting ->
      -- Wait a bit and retry
      pure $ Left $ GHCiServerError "Session is starting, please retry"
    SessionRunning session -> do
      -- Check if process is still alive
      exitCode <- getProcessExitCode (gsProcess session)
      case exitCode of
        Just code -> do
          -- Process died - attempt restart
          lastOut <- atomically $ readTVar (shLastOutput handle)
          atomically $
            writeTVar
              (shState handle)
              (SessionCrashed lastOut (exitCodeToInt code))
          if ocRestartOnCrash (shConfig handle)
            then do
              restartResult <- restartSession handle
              case restartResult of
                Left err -> pure $ Left err
                Right () -> withSession' handle action
            else pure $ Left $ GHCiSessionCrashed lastOut (Just $ exitCodeToInt code)
        Nothing -> action session
    SessionCrashed lastOutput code -> do
      if ocRestartOnCrash (shConfig handle)
        then do
          restartResult <- restartSession handle
          case restartResult of
            Left err -> pure $ Left err
            Right () -> withSession' handle action
        else pure $ Left $ GHCiSessionCrashed lastOutput (Just code)
    SessionStopped ->
      pure $ Left $ GHCiServerError "Session has been stopped"

-- | Internal helper after state check.
withSession' :: SessionHandle -> (GHCiSession -> IO (Either GHCiError a)) -> IO (Either GHCiError a)
withSession' handle action = do
  state <- atomically $ readTVar (shState handle)
  case state of
    SessionRunning session -> action session
    _ -> pure $ Left $ GHCiServerError "Session not running"

-- | Execute a single GHCi command.
execQuery :: SessionHandle -> GHCiSession -> Text -> IO (Either GHCiError Text)
execQuery handle session query = do
  let config = shConfig handle
      timeoutMs = ocQueryTimeoutMs config

  -- Send query
  hPutStrLn (gsStdin session) (T.unpack query)
  hFlush (gsStdin session)

  -- Store query for diagnostics
  atomically $ writeTVar (shLastOutput handle) query

  -- Read response with timeout
  result <- timeout (timeoutMs * 1000) $ readUntilPrompt handle session

  case result of
    Nothing -> do
      -- CRITICAL: On timeout, mark session as crashed to force restart.
      -- Otherwise the next query would receive stale output from this one.
      lastOut <- atomically $ readTVar (shLastOutput handle)
      atomically $ writeTVar (shState handle) (SessionCrashed lastOut (-1))
      pure $ Left $ GHCiTimeout query timeoutMs
    Just output -> do
      -- Check for error patterns in output
      if isErrorOutput output
        then pure $ Left $ GHCiParseError query output
        else pure $ Right output

-- ════════════════════════════════════════════════════════════════════════════
-- INTERNAL HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Set custom prompt for reliable boundary detection.
setPrompt :: GHCiSession -> IO ()
setPrompt session = do
  hPutStrLn (gsStdin session) $ ":set prompt \"" ++ T.unpack promptMarker ++ "\\n\""
  hPutStrLn (gsStdin session) $ ":set prompt-cont \"" ++ T.unpack promptMarker ++ "-cont\\n\""
  hFlush (gsStdin session)

-- | Wait for the custom prompt marker (initial startup).
waitForPrompt :: GHCiSession -> IO ()
waitForPrompt session = go
  where
    go = do
      line <- hGetLine (gsStdout session)
      if promptMarker `T.isPrefixOf` T.pack line
        then pure ()
        else go

-- | Read output until we see the prompt marker.
readUntilPrompt :: SessionHandle -> GHCiSession -> IO Text
readUntilPrompt handle session = go []
  where
    go acc = do
      line <- hGetLine (gsStdout session)
      let lineT = T.pack line
      -- Store for diagnostics
      atomically $ modifyTVar' (shLastOutput handle) (<> "\n" <> lineT)
      if promptMarker `T.isPrefixOf` lineT
        then pure $ T.intercalate "\n" (reverse acc)
        else go (lineT : acc)

-- | Background thread for stderr.
stderrReader :: SessionHandle -> Handle -> IO ()
stderrReader handle h = void $ try @SomeException $ forever $ do
  line <- hGetLine h
  let lineT = T.pack line
  -- Store for diagnostics
  atomically $ modifyTVar' (shLastOutput handle) (<> "\n[stderr] " <> lineT)
  when (ocVerbose (shConfig handle)) $
    TIO.putStrLn $
      "[ghci stderr] " <> lineT

-- | Terminate a session cleanly.
terminateSession :: GHCiSession -> IO ()
terminateSession session = do
  killThread (gsStderrReader session)
  -- Try graceful quit first
  void $ try @SomeException $ do
    hPutStrLn (gsStdin session) ":quit"
    hFlush (gsStdin session)
  -- Force terminate after brief wait
  void $ try @SomeException $ terminateProcess (gsProcess session)
  void $ try @SomeException $ waitForProcess (gsProcess session)
  void $ try @SomeException $ hClose (gsStdin session)
  void $ try @SomeException $ hClose (gsStdout session)
  void $ try @SomeException $ hClose (gsStderr session)

-- | Load initial modules.
loadInitialModules :: SessionHandle -> GHCiSession -> [Text] -> IO (Either GHCiError ())
loadInitialModules _ _ [] = pure $ Right ()
loadInitialModules handle session (m : ms) = do
  result <- execQuery handle session (":load " <> m)
  case result of
    Left err -> pure $ Left err
    Right output
      | isLoadError output -> pure $ Left $ GHCiLoadError m output
      | otherwise -> loadInitialModules handle session ms

-- | Check if output indicates an error.
isErrorOutput :: Text -> Bool
isErrorOutput t =
  "<interactive>:" `T.isInfixOf` t && "error:" `T.isInfixOf` t

-- | Convert ExitCode to Int.
exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess = 0
exitCodeToInt (ExitFailure n) = n
