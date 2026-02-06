module ExoMonad.PDK where

import ExoMonad.PDK.Bindings
import ExoMonad.PDK.Memory
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BSL

class FromBytes a where
  fromBytes :: B.ByteString -> Either String a

class ToBytes a where
  toBytes :: a -> B.ByteString

instance FromBytes B.ByteString where
  fromBytes = Right

instance ToBytes B.ByteString where
  toBytes = id

instance FromBytes BSL.ByteString where
  fromBytes = Right . BSL.fromStrict

instance ToBytes BSL.ByteString where
  toBytes = BSL.toStrict

input :: FromBytes a => IO a
input = do
  len <- extismInputLength
  bs <- readInputBytes len
  case fromBytes bs of
    Left e -> error e
    Right x -> return x

output :: ToBytes a => a -> IO ()
output x = do
  let bs = toBytes x
  Memory offs len <- alloc bs
  extismSetOutput offs len

setError :: ToBytes a => a -> IO ()
setError x = do
  let bs = toBytes x
  Memory offs _len <- alloc bs
  extismSetError offs
