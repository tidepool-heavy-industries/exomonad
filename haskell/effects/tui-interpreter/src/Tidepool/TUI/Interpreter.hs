{-# LANGUAGE BlockArguments #-}
-- | TUI effect interpreter - bidirectional communication with Rust TUI sidebar.
--
-- This interpreter connects to a TUI sidebar via Unix socket or TCP, sends
-- UISpec messages, and receives Interaction events. It's designed for use
-- within graph node handlers.
--
-- == Example Usage
--
-- @
-- import Tidepool.TUI.Interpreter (withTUIConnection, runTUI)
-- import Network.Socket (withSocketsDo)
--
-- main :: IO ()
-- main = withSocketsDo $ do
--   withTUIConnection "localhost" 7433 $ \handle -> do
--     runM $ runTUI handle $ do
--       interaction <- showUI $ UISpec {...}
--       case interaction of
--         ButtonClicked _ "submit" -> ...
-- @
module Tidepool.TUI.Interpreter
  ( -- * TUI Handle
    TUIHandle
  , newTUIHandle
  , closeTUIHandle

    -- * Connection Management
  , withTUIConnection
  , withTUIUnixConnection
  , connectTUI
  , connectTUIUnix

    -- * Interpreter
  , runTUI
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TChan, TVar, atomically, newTChanIO, newTVarIO, readTChan, writeTChan, writeTVar)
import Control.Exception (bracket, SomeException)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (encode, decode, ToJSON(..), object, (.=))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text (Text)
import Network.Socket
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Tidepool.Effect.TUI
import GHC.Generics (Generic)

-- | Handle for bidirectional TUI communication.
--
-- Contains:
-- - TCP socket connection to Rust TUI sidebar
-- - Send channel for outgoing messages (UISpec, UIUpdate, Close)
-- - Receive channel for incoming Interaction events
-- - Active UI tracking (which UI is currently shown)
-- - Session identifier
data TUIHandle = TUIHandle
  { tuiSocket :: Socket
  , tuiSendChan :: TChan TUIMessage
  , tuiRecvChan :: TChan Interaction
  , tuiActiveUI :: TVar (Maybe Text)
  , tuiSessionId :: Text
  }

-- | Messages sent from interpreter to TUI sidebar.
data TUIMessage
  = PushUI UISpec       -- ^ Show a new UI
  | ReplaceUI UISpec    -- ^ Replace currently shown UI
  | UpdateUIMsg UIUpdate   -- ^ Update an element in current UI (renamed to avoid collision)
  | PopUI               -- ^ Close current UI
  deriving (Show, Eq, Generic)

instance ToJSON TUIMessage where
  toJSON (PushUI spec) = object
    [ "type" .= ("PushUI" :: Text)
    , "spec" .= spec
    ]
  toJSON (ReplaceUI spec) = object
    [ "type" .= ("ReplaceUI" :: Text)
    , "spec" .= spec
    ]
  toJSON (UpdateUIMsg update) = object
    [ "type" .= ("UpdateUI" :: Text)
    , "update" .= update
    ]
  toJSON PopUI = object
    [ "type" .= ("PopUI" :: Text)
    ]

-- ══════════════════════════════════════════════════════════════
-- CONNECTION MANAGEMENT
-- ══════════════════════════════════════════════════════════════

-- | Create a new TUI handle from a connected socket.
--
-- Spawns background threads for sending and receiving messages.
-- The handle should be closed with 'closeTUIHandle' when done.
newTUIHandle :: Text -> Socket -> IO TUIHandle
newTUIHandle sessionId sock = do
  sendChan <- newTChanIO
  recvChan <- newTChanIO
  activeUI <- newTVarIO Nothing

  -- Spawn sender thread
  void $ forkIO $ senderThread sock sendChan

  -- Spawn receiver thread
  void $ forkIO $ receiverThread sock recvChan

  pure $ TUIHandle sock sendChan recvChan activeUI sessionId

-- | Close a TUI handle and clean up resources.
closeTUIHandle :: TUIHandle -> IO ()
closeTUIHandle h = close h.tuiSocket

-- | Connect to a TUI sidebar via TCP.
--
-- Returns a connected socket ready for use with 'newTUIHandle'.
connectTUI :: HostName -> ServiceName -> IO Socket
connectTUI host port = do
  let hints = defaultHints { addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock (addrAddress addr)
  pure sock

-- | Connect to a TUI sidebar via Unix socket.
connectTUIUnix :: FilePath -> IO Socket
connectTUIUnix path = do
  sock <- socket AF_UNIX Stream 0
  connect sock (SockAddrUnix path)
  pure sock

-- | Bracket-style connection management for TCP.
--
-- Connects to TUI, creates handle, runs action, and cleans up.
--
-- @
-- withTUIConnection "localhost" "7433" $ \handle -> do
--   runM $ runTUI handle $ ...
-- @
withTUIConnection :: HostName -> ServiceName -> (TUIHandle -> IO a) -> IO a
withTUIConnection host port action = bracket acquire release action
  where
    acquire = do
      sock <- connectTUI host port
      newTUIHandle "default-session" sock
    release = closeTUIHandle

-- | Bracket-style connection management for Unix socket.
withTUIUnixConnection :: FilePath -> (TUIHandle -> IO a) -> IO a
withTUIUnixConnection path action = bracket acquire release action
  where
    acquire = do
      sock <- connectTUIUnix path
      newTUIHandle "default-session" sock
    release = closeTUIHandle

-- ══════════════════════════════════════════════════════════════
-- BACKGROUND THREADS
-- ══════════════════════════════════════════════════════════════

-- | Sender thread: reads from send channel, writes to socket.
senderThread :: Socket -> TChan TUIMessage -> IO ()
senderThread sock sendChan = E.handle (\(_ :: SomeException) -> pure ()) $ forever $ do
  msg <- atomically $ readTChan sendChan
  let json = encode msg
  let line = json <> "\n"  -- NDJSON: newline-delimited
  sendAll sock line

-- | Receiver thread: reads from socket, writes to receive channel.
receiverThread :: Socket -> TChan Interaction -> IO ()
receiverThread sock recvChan = E.handle (\(_ :: SomeException) -> pure ()) $ forever $ do
  line <- recvLine sock
  case decode line of
    Just interaction -> atomically $ writeTChan recvChan interaction
    Nothing -> pure ()  -- Ignore malformed messages

-- | Read one NDJSON line from socket.
recvLine :: Socket -> IO ByteString
recvLine sock = go mempty
  where
    go acc = do
      chunk <- recv sock 4096
      if LBS.null chunk
        then pure acc  -- Connection closed
        else case LBS8.elemIndex '\n' chunk of
          Just idx ->
            let (line, _rest) = LBS.splitAt (idx + 1) chunk
            in pure (acc <> line)
          Nothing -> go (acc <> chunk)

-- ══════════════════════════════════════════════════════════════
-- INTERPRETER
-- ══════════════════════════════════════════════════════════════

-- | Interpret the TUI effect by communicating with a connected TUI sidebar.
--
-- This interpreter:
-- - Sends UISpec messages when 'showUI' is called
-- - Waits for Interaction responses
-- - Sends UIUpdate messages when 'updateUI' is called (non-blocking)
-- - Sends close message when 'closeUI' is called
--
-- @
-- result <- runM $ runTUI handle $ do
--   interaction <- showUI formSpec
--   case interaction of
--     ButtonClicked _ "submit" -> pure "submitted"
--     _ -> pure "cancelled"
-- @
runTUI :: LastMember IO effs
       => TUIHandle -> Eff (TUI ': effs) a -> Eff effs a
runTUI h = interpret $ \case
  ShowUI spec -> sendM $ do
    -- Send UI spec to TUI sidebar
    atomically $ do
      writeTChan h.tuiSendChan (PushUI spec)
      writeTVar h.tuiActiveUI (Just spec.uiId)

    -- Wait for interaction response
    atomically $ readTChan h.tuiRecvChan

  UpdateUI update -> sendM $ do
    -- Send update message (non-blocking)
    atomically $ writeTChan h.tuiSendChan (UpdateUIMsg update)

  CloseUI -> sendM $ do
    -- Send close message
    atomically $ do
      writeTChan h.tuiSendChan PopUI
      writeTVar h.tuiActiveUI Nothing
