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

-- | Read bytes from Memory using direct byte packing.
-- Previously used B.concat of singleton ByteStrings which caused issues
-- with proto3-wire's decoder on wasm32.
readBytes :: MemoryOffset -> MemoryLength -> IO B.ByteString
readBytes offs len = do
  bytes <- readBytesLoop extismLoadU8 offs (fromIntegral len) 0 []
  return $ B.pack (reverse bytes)

-- | Read input bytes using direct byte packing.
readInputBytes :: InputLength -> IO B.ByteString
readInputBytes len = do
  bytes <- readBytesLoop extismInputLoadU8 0 (fromIntegral len) 0 []
  return $ B.pack (reverse bytes)

-- | Read bytes one at a time into a list of Word8, then pack into ByteString.
-- This avoids the B.concat-of-singletons approach which produced ByteStrings
-- that proto3-wire couldn't parse on wasm32.
readBytesLoop :: (Word64 -> IO Word8) -> Word64 -> Int -> Int -> [Word8] -> IO [Word8]
readBytesLoop f1 baseOff total index acc
  | index >= total = return acc
  | otherwise = do
      b <- f1 (baseOff + fromIntegral index)
      readBytesLoop f1 baseOff total (index + 1) (b : acc)

-- | Helper to convert Word64 to ByteString (little-endian)
word64ToBS :: Word64 -> B.ByteString
word64ToBS w = BI.unsafeCreate 8 $ \p -> poke (castPtr p) w

-- | Load ByteString from Memory
load :: Memory -> IO (Either String B.ByteString)
load (Memory 0 _) = return $ Right B.empty
load (Memory offs len) = Right <$> readBytes offs len
