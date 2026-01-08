-- | Control socket server for Claude Code hook interception.
--
-- Provides a Unix domain socket server that receives hook events
-- from zellij-cc and dispatches them to registered callbacks.
--
-- @
-- withControlSocket callbacks $ \\socketPath -> do
--   -- socketPath is passed to zellij-cc via --control-socket
--   runClaudeCodeWithSocket socketPath ...
-- @
module Tidepool.ClaudeCode.ControlSocket
  ( -- * Socket Server
    withControlSocket
  , ControlSocketConfig(..)
  , defaultControlSocketConfig

    -- * Socket Path
  , createSocketPath
  ) where

import Control.Concurrent (forkIO, ThreadId, killThread)
import Control.Exception (bracket, catch, SomeException, finally, try)
import Control.Monad (forever)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Network.Socket
  ( Socket
  , Family(AF_UNIX)
  , SocketType(Stream)
  , SockAddr(SockAddrUnix)
  , socket
  , bind
  , listen
  , accept
  , close
  , gracefulClose
  )
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (removeFile, getTemporaryDirectory)
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)

import Tidepool.ClaudeCode.Types
  ( ControlMessage(..)
  , ControlResponse(..)
  , HookInput(..)
  , HookDecision(..)
  , hookDecisionToOutput
  )
import Tidepool.ClaudeCode.Hooks (HookCallbacks(..))


-- | Configuration for the control socket server.
data ControlSocketConfig = ControlSocketConfig
  { cscSocketPath :: Maybe FilePath
    -- ^ Custom socket path (default: auto-generated in /tmp)
  , cscBacklog :: Int
    -- ^ Listen backlog (default: 5)
  }
  deriving stock (Show, Eq)

-- | Default configuration with auto-generated socket path.
defaultControlSocketConfig :: ControlSocketConfig
defaultControlSocketConfig = ControlSocketConfig
  { cscSocketPath = Nothing
  , cscBacklog = 5
  }


-- | Create a unique socket path based on PID.
createSocketPath :: IO FilePath
createSocketPath = do
  tmpDir <- getTemporaryDirectory
  pid <- getProcessID
  pure $ tmpDir </> ("tidepool-control-" <> show pid <> ".sock")


-- | Run action with a control socket server.
--
-- Creates a Unix domain socket, starts a listener thread, runs the
-- action with the socket path, then cleans up.
--
-- The socket path is passed to the action so it can be provided to
-- zellij-cc via the @--control-socket@ flag.
--
-- @
-- withControlSocket myCallbacks $ \\socketPath -> do
--   -- Run claude code with hooks enabled
--   runWithSocket socketPath
-- @
withControlSocket
  :: HookCallbacks       -- ^ Callbacks for hook events
  -> (FilePath -> IO a)  -- ^ Action to run with socket path
  -> IO a
withControlSocket callbacks action =
  withControlSocketConfig defaultControlSocketConfig callbacks action


-- | Run action with a control socket server using custom config.
withControlSocketConfig
  :: ControlSocketConfig  -- ^ Configuration
  -> HookCallbacks        -- ^ Callbacks for hook events
  -> (FilePath -> IO a)   -- ^ Action to run with socket path
  -> IO a
withControlSocketConfig config callbacks action = do
  socketPath <- case cscSocketPath config of
    Just p -> pure p
    Nothing -> createSocketPath

  -- Clean up any stale socket from previous run
  removeFile socketPath `catch` \(_ :: SomeException) -> pure ()

  bracket
    (startServer socketPath config callbacks)
    (stopServer socketPath)
    (\_ -> action socketPath)


-- | Server state (just the listener thread for now).
data ServerState = ServerState
  { ssListenerThread :: ThreadId
  , ssSocket :: Socket
  }


-- | Start the socket server.
--
-- Note: No race condition here - the socket is in listening state (and can
-- queue connections via kernel backlog) before we fork the accept loop.
-- Clients can connect as soon as 'listen' returns.
startServer :: FilePath -> ControlSocketConfig -> HookCallbacks -> IO ServerState
startServer socketPath config callbacks = do
  -- Create socket
  sock <- socket AF_UNIX Stream 0

  -- Bind and listen - socket is ready to accept after this
  bind sock (SockAddrUnix socketPath)
  listen sock (cscBacklog config)

  -- Start listener thread (socket already listening, no race)
  tid <- forkIO $ listenLoop sock callbacks

  pure $ ServerState tid sock


-- | Stop the socket server and clean up.
stopServer :: FilePath -> ServerState -> IO ()
stopServer socketPath state = do
  -- Kill listener thread
  killThread (ssListenerThread state)

  -- Close socket
  close (ssSocket state)

  -- Remove socket file
  removeFile socketPath `catch` \(_ :: SomeException) -> pure ()


-- | Main listener loop - accepts connections and handles them.
listenLoop :: Socket -> HookCallbacks -> IO ()
listenLoop sock callbacks = forever $ do
  -- Accept connection
  (clientSock, _) <- accept sock

  -- Handle in current thread (hooks are blocking anyway)
  -- Could use forkIO for concurrent hook handling if needed
  handleConnection clientSock callbacks
    `finally` gracefulClose clientSock 1000


-- | Handle a single connection from zellij-cc.
--
-- Protocol: Read newline-delimited JSON, process, write response + newline.
handleConnection :: Socket -> HookCallbacks -> IO ()
handleConnection sock callbacks = do
  -- Read message (accumulate until newline)
  msg <- readMessage sock

  case eitherDecode (LBS.fromStrict msg) of
    Left err -> do
      -- Parse error - send error response
      let response = HookResponse
            { crOutput = hookDecisionToOutput "Error" (HookBlock $ T.pack err)
            , crExitCode = 1
            }
      sendResponse sock response

    Right controlMsg -> do
      -- Dispatch to appropriate handler
      response <- handleMessage callbacks controlMsg
      sendResponse sock response


-- | Read a message from the socket (until newline).
readMessage :: Socket -> IO BS.ByteString
readMessage sock = go mempty
  where
    go acc = do
      chunk <- recv sock 4096
      if BS.null chunk
        then pure acc  -- EOF
        else do
          let combined = acc <> chunk
          if BS.elem '\n' combined
            then pure $ BS.takeWhile (/= '\n') combined
            else go combined


-- | Send a response to the socket.
sendResponse :: Socket -> ControlResponse -> IO ()
sendResponse sock response = do
  let json = LBS.toStrict (encode response) <> "\n"
  sendAll sock json


-- | Handle a control message and return response.
handleMessage :: HookCallbacks -> ControlMessage -> IO ControlResponse
handleMessage callbacks msg = case msg of
  HookEvent input -> handleHookEvent callbacks input
  McpToolCall callId toolName args -> handleMcpToolCall callId toolName args


-- | Handle a hook event.
--
-- Catches exceptions from user callbacks and returns an error response
-- instead of propagating (which would kill the listener thread).
handleHookEvent :: HookCallbacks -> HookInput -> IO ControlResponse
handleHookEvent callbacks input = do
  let eventName = hiHookEventName input

  -- Catch exceptions from user callbacks
  decisionResult <- try $ dispatchHook callbacks input

  case decisionResult of
    Left (e :: SomeException) -> do
      -- Callback threw - return error response but don't kill listener
      let errMsg = "Callback exception: " <> T.pack (show e)
      pure $ HookResponse (hookDecisionToOutput eventName (HookBlock errMsg)) 1

    Right decision -> do
      let output = hookDecisionToOutput eventName decision
          exitCode = case decision of
            HookDeny _ -> 2
            HookBlock _ -> 2
            _ -> 0
      pure $ HookResponse output exitCode


-- | Dispatch to the appropriate callback based on event type.
dispatchHook :: HookCallbacks -> HookInput -> IO HookDecision
dispatchHook callbacks input = case hiHookEventName input of
  "PreToolUse" ->
    case (hiToolName input, hiToolInput input) of
      (Just name, Just toolInput) -> hcOnPreToolUse callbacks name toolInput
      _ -> pure HookAllow  -- Missing fields, allow

  "PostToolUse" ->
    case (hiToolName input, hiToolInput input, hiToolResponse input) of
      (Just name, Just toolInput, Just toolResponse) ->
        hcOnPostToolUse callbacks name toolInput toolResponse
      _ -> pure HookAllow

  "PermissionRequest" ->
    case (hiToolName input, hiToolInput input) of
      (Just name, Just toolInput) -> hcOnPermissionRequest callbacks name toolInput
      _ -> pure HookAllow

  "Notification" ->
    hcOnNotification callbacks
      (fromMaybe "unknown" $ hiNotificationType input)
      (hiMessage input)

  "Stop" -> hcOnStop callbacks

  "SubagentStop" -> hcOnSubagentStop callbacks

  "PreCompact" ->
    hcOnPreCompact callbacks (fromMaybe "manual" $ hiTrigger input)

  "SessionStart" ->
    hcOnSessionStart callbacks (fromMaybe "startup" $ hiSource input)

  "SessionEnd" ->
    hcOnSessionEnd callbacks (fromMaybe "unknown" $ hiReason input)

  "UserPromptSubmit" ->
    hcOnUserPromptSubmit callbacks (fromMaybe "" $ hiPrompt input)

  _ -> pure HookAllow  -- Unknown event, allow


-- | Handle MCP tool call (placeholder for future implementation).
handleMcpToolCall :: Text -> Text -> a -> IO ControlResponse
handleMcpToolCall callId toolName _args = do
  -- MCP tools not implemented yet - return error
  pure $ McpToolResponse
    { crId = callId
    , crResult = Nothing
    , crError = Just $ "MCP tool not implemented: " <> toolName
    }
