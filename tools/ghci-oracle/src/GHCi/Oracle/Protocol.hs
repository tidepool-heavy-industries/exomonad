-- | Wire protocol handling for GHCi Oracle server.
--
-- Handles JSON encoding/decoding over socket connections.
-- Uses length-prefixed messages for framing.
module GHCi.Oracle.Protocol
  ( -- * Message Framing
    sendResponse
  , receiveRequest

    -- * Helpers
  , encodeMessage
  , decodeMessage
  ) where

import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode')
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as NBS

import GHCi.Oracle.Types


-- ════════════════════════════════════════════════════════════════════════════
-- MESSAGE FRAMING
-- ════════════════════════════════════════════════════════════════════════════

-- Message format:
-- 4 bytes: length (big-endian)
-- N bytes: JSON payload


-- | Send a response over the socket.
sendResponse :: Socket -> GHCiResponse -> IO ()
sendResponse sock resp = do
  let payload = BL.toStrict $ encode resp
      len = BS.length payload
      lenBytes = encodeLen len
  NBS.sendAll sock (lenBytes <> payload)


-- | Receive a request from the socket.
receiveRequest :: Socket -> IO (Either Text GHCiRequest)
receiveRequest sock = do
  -- Read length prefix (4 bytes)
  lenBytes <- recvExact sock 4
  if BS.length lenBytes < 4
    then pure $ Left "Connection closed"
    else do
      let len = decodeLen lenBytes
      if len > 1000000  -- 1MB sanity limit
        then pure $ Left $ "Message too large: " <> T.pack (show len)
        else do
          payload <- recvExact sock len
          if BS.length payload < len
            then pure $ Left "Incomplete message"
            else case eitherDecode' (BL.fromStrict payload) of
              Left err -> pure $ Left $ "JSON decode error: " <> T.pack err
              Right req -> pure $ Right req


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Encode any value to length-prefixed JSON.
encodeMessage :: ToJSON a => a -> ByteString
encodeMessage val =
  let payload = BL.toStrict $ encode val
      len = BS.length payload
  in encodeLen len <> payload


-- | Decode length-prefixed JSON.
decodeMessage :: FromJSON a => ByteString -> Either Text a
decodeMessage bs
  | BS.length bs < 4 = Left "Message too short"
  | otherwise =
      let len = decodeLen (BS.take 4 bs)
          payload = BS.drop 4 bs
      in if BS.length payload < len
           then Left "Incomplete message"
           else case eitherDecode' (BL.fromStrict $ BS.take len payload) of
             Left err -> Left $ T.pack err
             Right val -> Right val


-- | Encode length as 4 big-endian bytes.
encodeLen :: Int -> ByteString
encodeLen n = BS.pack
  [ fromIntegral (n `shiftR` 24)
  , fromIntegral (n `shiftR` 16)
  , fromIntegral (n `shiftR` 8)
  , fromIntegral n
  ]
  where
    shiftR :: Int -> Int -> Int
    shiftR x s = x `div` (2 ^ s)


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
