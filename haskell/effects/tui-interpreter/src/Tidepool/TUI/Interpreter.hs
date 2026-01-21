{-# LANGUAGE BlockArguments #-}
-- | TUI effect interpreter - bidirectional communication with Rust TUI sidebar.
--
-- This interpreter connects to a TUI sidebar via Unix socket or TCP, sends
-- PopupDefinition messages, and receives PopupResult responses. Uses the
-- popup-tui pattern: one request → one response.
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
--       result <- showUI $ PopupDefinition {...}
--       case result.prButton of
--         "submit" -> ...
--         _ -> ...
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
import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan, writeTChan)
import Control.Exception (bracket, SomeException)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (encode, decode, object)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text (Text)
import Network.Socket
import Network.Socket.ByteString.Lazy (recv, sendAll)
import System.Timeout (timeout)
import Tidepool.Effect.TUI

-- | Handle for bidirectional TUI communication.
--
-- Contains:
-- - Socket connection to Rust TUI sidebar
-- - Send channel for outgoing PopupDefinition messages
-- - Receive channel for incoming PopupResult responses
-- - Session identifier
--
-- Note: popup-tui uses simple request-response. No UI stack, no active UI tracking.
data TUIHandle = TUIHandle
  { tuiSocket :: Socket
  , tuiSendChan :: TChan PopupDefinition
  , tuiRecvChan :: TChan PopupResult
  , tuiSessionId :: Text
  }

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

  -- Spawn sender thread
  void $ forkIO $ senderThread sock sendChan

  -- Spawn receiver thread
  void $ forkIO $ receiverThread sock recvChan

  pure $ TUIHandle sock sendChan recvChan sessionId

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
senderThread :: Socket -> TChan PopupDefinition -> IO ()
senderThread sock sendChan = E.handle (\(_ :: SomeException) -> pure ()) $ forever $ do
  msg <- atomically $ readTChan sendChan
  let json = encode msg
  let line = json <> "\n"  -- NDJSON: newline-delimited
  sendAll sock line

-- | Receiver thread: reads from socket, writes to receive channel.
receiverThread :: Socket -> TChan PopupResult -> IO ()
receiverThread sock recvChan = E.handle (\(_ :: SomeException) -> pure ()) $ forever $ do
  line <- recvLine sock
  case decode line of
    Just result -> atomically $ writeTChan recvChan result
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
-- Uses the popup-tui pattern: send PopupDefinition, receive PopupResult.
-- This is a simple request-response model with no streaming interactions.
--
-- @
-- result <- runM $ runTUI handle $ do
--   popupResult <- showUI $ PopupDefinition
--     { pdTitle = "Config"
--     , pdComponents = [mkSlider "budget" "Budget" 10 100 50 Nothing]
--     }
--   case popupResult.prButton of
--     "submit" -> pure "submitted"
--     _ -> pure "cancelled"
-- @
runTUI :: LastMember IO effs
       => TUIHandle -> Eff (TUI ': effs) a -> Eff effs a
runTUI h = interpret $ \case
  ShowUI definition -> sendM $ do
    -- Send PopupDefinition to TUI sidebar
    atomically $ writeTChan h.tuiSendChan definition

    -- Wait for PopupResult response (blocking) with timeout
    -- Timeout: 300 seconds (300,000,000 microseconds)
    result <- timeout 300_000_000 $ atomically $ readTChan h.tuiRecvChan
    case result of
      Just r -> pure r
      Nothing -> do
        putStrLn "Error: TUI timeout after 300s"
        pure $ PopupResult "timeout" (object [])
