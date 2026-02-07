module ExoMonad.PDK.Memory where

import ExoMonad.PDK.Bindings
import Data.Word
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as BI
import Foreign.Ptr
import Foreign.Storable

data Memory = Memory MemoryOffset MemoryLength
  deriving (Show, Eq)

-- | Create a Memory block from an offset and length
findMemory :: MemoryOffset -> IO Memory
findMemory 0 = return $ Memory 0 0
findMemory offs = do
  len <- extismLength offs
  return $ Memory offs len

-- | Allocate a new Memory block
memAlloc :: Int -> IO Memory
memAlloc n = do
  offs <- extismAlloc (fromIntegral n)
  return $ Memory offs (fromIntegral n)

-- | Get the offset of a Memory block
memoryOffset :: Memory -> MemoryOffset
memoryOffset (Memory offs _) = offs

-- | Get the length of a Memory block
memoryLength :: Memory -> MemoryLength
memoryLength (Memory _ len) = len

-- | Free a Memory block
free :: Memory -> IO ()
free (Memory 0 _) = return ()
free (Memory _ 0) = return ()
free (Memory offs _) = extismFree offs

-- | Write a ByteString into a Memory block
writeBytes :: MemoryOffset -> MemoryLength -> B.ByteString -> IO ()
writeBytes offs len bs = do
  let count = min (fromIntegral len) (B.length bs)
  mapM_ (\i -> extismStoreU8 (offs + fromIntegral i) (B.index bs i)) [0..count-1]

-- | Allocate and write a ByteString
alloc :: B.ByteString -> IO Memory
alloc bs = do
  mem@(Memory offs len) <- memAlloc (B.length bs)
  writeBytes offs len bs
  return mem

-- | Read bytes from Memory
readBytes :: MemoryOffset -> MemoryLength -> IO B.ByteString
readBytes offs len = readLoop extismLoadU8 extismLoadU64 (offs + len) offs []

-- | Read input bytes
readInputBytes :: InputLength -> IO B.ByteString
readInputBytes len = readLoop extismInputLoadU8 extismInputLoadU64 len 0 []

-- | Internal loop for reading bytes.
--
-- Reads byte-by-byte using the u8 function only. The u64 optimization
-- caused alignment issues on wasm32 (poke of Word64 to unaligned Ptr
-- produced garbled bytes, breaking protobuf varint parsing).
readLoop :: (Word64 -> IO Word8) -> (Word64 -> IO Word64) -> Word64 -> Word64 -> [B.ByteString] -> IO B.ByteString
readLoop f1 _f8 total index acc =
  if index >= total
    then return $ B.concat . reverse $ acc
    else do
      b <- f1 index
      readLoop f1 _f8 total (index + 1) (B.singleton b : acc)

-- | Helper to convert Word64 to ByteString (little-endian)
word64ToBS :: Word64 -> B.ByteString
word64ToBS w = BI.unsafeCreate 8 $ \p -> poke (castPtr p) w

-- | Load ByteString from Memory
load :: Memory -> IO (Either String B.ByteString)
load (Memory 0 _) = return $ Right B.empty
load (Memory offs len) = Right <$> readBytes offs len
