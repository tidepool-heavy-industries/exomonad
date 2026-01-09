-- | Wire protocol client for GHCi Oracle.
--
-- Handles connection and message exchange with ghci-oracle server.
module Tidepool.GHCi.Protocol
  ( -- * Connection
    GHCiConnection(..)
  , connect
  , disconnect

    -- * Communication
  , sendRequest
  , receiveResponse

    -- * Length Encoding (for testing)
  , encodeLen
  , decodeLen
  ) where

import Control.Concurrent.MVar
import Control.Exception (bracketOnError, try, SomeException)
import Data.Aeson (encode, eitherDecode')
import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Network.Socket
  ( Socket
  , AddrInfo(..)
  , SocketType(..)
  , close
  , defaultHints
  , getAddrInfo
  , socket
  , withSocketsDo
  )
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS

import Tidepool.Effect.GHCi (GHCiRequest(..), GHCiResponse(..), GHCiError(..))


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Connection to GHCi Oracle server.
data GHCiConnection = GHCiConnection
  { gcSocket :: Socket
    -- ^ Socket to server
  , gcLock :: MVar ()
    -- ^ Mutex to serialize requests
  }


-- ════════════════════════════════════════════════════════════════════════════
-- CONNECTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Connect to GHCi Oracle server.
connect :: String -> Int -> IO (Either GHCiError GHCiConnection)
connect host port = withSocketsDo $ do
  result <- try @SomeException $ do
    let hints = defaultHints { addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) (Just host) (Just $ show port)
    addr <- case addrs of
      (a:_) -> pure a
      []    -> ioError (userError "getAddrInfo: no addresses found")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bracketOnError (pure sock) close $ \s -> do
      NS.connect s (addrAddress addr)
      lock <- newMVar ()
      pure GHCiConnection { gcSocket = s, gcLock = lock }

  case result of
    Left _ -> pure $ Left $ GHCiNotConnected
    Right conn -> pure $ Right conn


-- | Disconnect from server.
disconnect :: GHCiConnection -> IO ()
disconnect conn = close (gcSocket conn)


-- ════════════════════════════════════════════════════════════════════════════
-- COMMUNICATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Send a request to the server.
sendRequest :: GHCiConnection -> GHCiRequest -> IO (Either GHCiError GHCiResponse)
sendRequest conn req = withMVar (gcLock conn) $ \_ -> do
  result <- try @SomeException $ do
    -- Send request
    let payload = BL.toStrict $ encode req
        len = BS.length payload
        lenBytes = encodeLen len
    NBS.sendAll (gcSocket conn) (lenBytes <> payload)

    -- Receive response
    receiveResponse conn

  case result of
    Left e -> pure $ Left $ GHCiServerError $ "Communication error: " <> T.pack (show e)
    Right resp -> pure resp


-- | Receive a response from the server.
receiveResponse :: GHCiConnection -> IO (Either GHCiError GHCiResponse)
receiveResponse conn = do
  -- Read length prefix (4 bytes)
  lenBytes <- recvExact (gcSocket conn) 4
  if BS.length lenBytes < 4
    then pure $ Left GHCiNotConnected
    else do
      let len = decodeLen lenBytes
      if len > 1000000  -- 1MB sanity limit
        then pure $ Left $ GHCiServerError $ "Message too large: " <> T.pack (show len)
        else do
          payload <- recvExact (gcSocket conn) len
          if BS.length payload < len
            then pure $ Left $ GHCiServerError "Incomplete response"
            else case eitherDecode' (BL.fromStrict payload) of
              Left err -> pure $ Left $ GHCiServerError $ "JSON decode error: " <> T.pack err
              Right resp -> pure $ Right resp


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Encode length as 4 big-endian bytes.
encodeLen :: Int -> ByteString
encodeLen n = BS.pack
  [ fromIntegral (n `shiftR` 24)
  , fromIntegral (n `shiftR` 16)
  , fromIntegral (n `shiftR` 8)
  , fromIntegral n
  ]


-- | Decode 4 big-endian bytes to length.
decodeLen :: ByteString -> Int
decodeLen bs = case BS.unpack bs of
  [b0, b1, b2, b3] ->
    fromIntegral b0 * 16777216 +
    fromIntegral b1 * 65536 +
    fromIntegral b2 * 256 +
    fromIntegral b3
  _ -> 0


-- | Receive exactly n bytes from socket.
recvExact :: Socket -> Int -> IO ByteString
recvExact sock n = go n []
  where
    go 0 acc = pure $ BS.concat (reverse acc)
    go remaining acc = do
      chunk <- NBS.recv sock (min remaining 4096)
      if BS.null chunk
        then pure $ BS.concat (reverse acc)  -- Connection closed
        else go (remaining - BS.length chunk) (chunk : acc)
