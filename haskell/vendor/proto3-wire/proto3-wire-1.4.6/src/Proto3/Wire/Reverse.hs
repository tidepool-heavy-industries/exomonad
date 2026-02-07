{-
  Copyright 2020 Awake Networks

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-}

-- | This module differs from the "Data.ByteString.Builder" module by
-- writing the octets in reverse order, which lets us compute the length
-- of a submessage by writing that submessage and measuring its length
-- before we write a variadic integer prefix encoding that length.
--
-- Example use:
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (withLengthOf (word64Base128LEVar . fromIntegral) (word32BE 42 <> charUtf8 'λ')))
-- [6,0,0,0,42,206,187]

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}

module Proto3.Wire.Reverse
    ( -- * `BuildR` type
      BuildR

      -- * Create `BuildR`s
    , etaBuildR
    , ensure
    , withLengthOf
    , byteString
    , lazyByteString
    , shortByteString
    , word8
    , int8
    , word16BE
    , word16LE
    , int16BE
    , int16LE
    , word32BE
    , word32LE
    , int32BE
    , int32LE
    , word64BE
    , word64LE
    , int64BE
    , int64LE
    , floatBE
    , floatLE
    , doubleBE
    , doubleLE
    , char7
    , string7
    , char8
    , string8
    , charUtf8
    , stringUtf8
    , textUtf8
    , lazyTextUtf8
    , shortTextUtf8
    , wordBase128LEVar
    , wordBase128LEVar_inline
    , word32Base128LEVar
    , word32Base128LEVar_inline
    , word64Base128LEVar
    , word64Base128LEVar_inline
    , vectorBuildR

    -- * Consume `BuildR`s
    , runBuildR
    , toLazyByteString

    -- * Helpful combinators
    , foldlRVector
    , repeatedBuildR
    , repeatedFixedPrimR

    -- * Exported for testing purposes only.
    , testWithUnused
    ) where

import           Data.Bits                     ( (.&.) )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Internal      as BI
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.ByteString.Short         as BS
import qualified Data.ByteString.Short.Internal as BSI
import qualified Data.ByteString.Unsafe        as BU
import           Data.Char                     ( ord )
import           Data.Int                      ( Int8, Int16, Int32, Int64 )
import qualified Data.Text                     as T
import qualified Data.Text.Internal            as TI
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Short               as TS
import           Data.Vector.Generic           ( Vector )
import           Data.Word                     ( Word8, Word16, Word32, Word64 )
import           Foreign                       ( castPtr, copyBytes )
import           GHC.Exts                      ( Addr#, Int(..), Int# )
#if !MIN_VERSION_bytestring(0,11,0)
import           GHC.Exts                      ( plusAddr# )
#endif
import           GHC.ForeignPtr                ( ForeignPtr(..), ForeignPtrContents )
import           GHC.TypeLits                  ( KnownNat )
import           Proto3.Wire.Encode.Repeated   ( Repeated(..), ToRepeated(..) )
import           Proto3.Wire.Reverse.Internal
import qualified Proto3.Wire.Reverse.Prim      as Prim

#if MIN_VERSION_text(2,0,0)
import           Control.Monad.ST.Unsafe       (unsafeSTToIO)
import qualified Data.Text.Array               as TA
#else
import qualified Data.Text.Internal.Fusion     as TIF
#endif

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :module Proto3.Wire.Reverse

-- | Create a lazy `BL.ByteString` from a `BuildR`
--
-- > toLazyByteString (x <> y) = toLazyByteString x <> toLazyByteString y
-- >
-- > toLazyByteString mempty = mempty
--
-- >>> toLazyByteString (stringUtf8 "ABC")
-- "ABC"
toLazyByteString :: BuildR -> BL.ByteString
toLazyByteString = snd . runBuildR

-- | Convert a strict `B.ByteString` to a `BuildR`
--
-- > byteString (x <> y) = byteString x <> byteString y
-- >
-- > byteString mempty = mempty
--
-- >>> byteString "ABC"
-- Proto3.Wire.Reverse.lazyByteString "ABC"
byteString :: B.ByteString -> BuildR
#if MIN_VERSION_bytestring(0,11,0)
byteString (BI.BS (ForeignPtr ad ct) (I# len)) = byteStringImpl ad ct len
#else
byteString (BI.PS (ForeignPtr ad ct) (I# off) (I# len)) = byteStringImpl (plusAddr# ad off) ct len
#endif
{-# INLINE byteString #-}

byteStringImpl :: Addr# -> ForeignPtrContents -> Int# -> BuildR
byteStringImpl ad ct len =
#if MIN_VERSION_bytestring(0,11,0)
  let bs = BI.BS (ForeignPtr ad ct) (I# len) in
#else
  let bs = BI.PS (ForeignPtr ad ct) 0 (I# len) in
#endif
  withUnused $ \unused ->
    if I# len <= unused
      then
        unsafeConsume (I# len) $ \dst ->
          BU.unsafeUseAsCString bs $ \src ->
            copyBytes dst (castPtr src) (I# len)
      else
        prependChunk bs
{-# NOINLINE [1] byteStringImpl #-}
{-# RULES
    "byteStringImpl/empty" forall ad ct . byteStringImpl ad ct 0# = mempty
  #-}

-- | Convert a lazy `BL.ByteString` to a `BuildR`
--
-- Warning: evaluating the length will force the lazy `BL.ByteString`'s chunks,
-- and they will remain allocated until you finish using the builder.
--
-- > lazyByteString (x <> y) = lazyByteString x <> lazyByteString y
-- >
-- > lazyByteString mempty = mempty
--
-- > lazyByteString . toLazyByteString = id
-- >
-- > toLazyByteString . lazyByteString = id
--
-- >>> lazyByteString "ABC"
-- Proto3.Wire.Reverse.lazyByteString "ABC"
lazyByteString :: BL.ByteString -> BuildR
lazyByteString = etaBuildR $ scan (ReverseChunks BL.empty)
  where
    scan :: ReverseChunks -> BL.ByteString -> BuildR
    scan r BLI.Empty = prepend r
    scan (ReverseChunks r) (BLI.Chunk c cs) =
      scan (ReverseChunks (BLI.Chunk c r)) cs

    prepend :: ReverseChunks -> BuildR
    prepend (ReverseChunks BLI.Empty) = mempty
    prepend (ReverseChunks (BLI.Chunk c cs)) = withUnused $ \unused ->
      let len = B.length c in
      if len <= unused
        then
          (prepend (ReverseChunks cs) <>) $
            unsafeConsume len $ \dst ->
              BU.unsafeUseAsCString c $ \src ->
                copyBytes dst (castPtr src) len
        else
          prependReverseChunks (ReverseChunks(BLI.Chunk c cs))
{-# NOINLINE [1] lazyByteString #-}
{-# RULES
    "lazyByteString/empty" lazyByteString BLI.Empty = mempty
  #-}

-- | Convert a `BS.ShortByteString` to a `BuildR`
--
-- > shortByteString (x <> y) = shortByteString x <> shortByteString y
-- >
-- > shortByteString mempty = mempty
--
-- >>> shortByteString "ABC"
-- Proto3.Wire.Reverse.lazyByteString "ABC"
shortByteString :: BS.ShortByteString -> BuildR
shortByteString bs = withUnused $ \unused ->
    let len = BS.length bs in
    if len <= unused
      then
        writeChunk bs 0 len
      else
        let rest = len - unused in
        writeChunk bs 0 rest <> reallocate rest <> writeChunk bs rest unused
  where
    writeChunk src off len =
      unsafeConsume len $ \dst ->
        BSI.copyToPtr src off dst len

-- | Convert a `Word8` to a `BuildR`
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word8 42))
-- [42]
word8 :: Word8 -> BuildR
word8 = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.word8 x))
{-# INLINE word8 #-}

-- | Convert a `Int8` to a `BuildR`
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (int8 (-5)))
-- [251]
int8 :: Int8 -> BuildR
int8 = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.int8 x))
{-# INLINE int8 #-}

-- | Convert a `Word16` to a `BuildR` by storing the bytes in big-endian order
--
-- In other words, the most significant byte is stored first and the least
-- significant byte is stored last
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word16BE 42))
-- [0,42]
word16BE :: Word16 -> BuildR
word16BE = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.word16BE x))
{-# INLINE word16BE #-}

-- | Convert a `Word16` to a `BuildR` by storing the bytes in little-endian
-- order
--
-- In other words, the least significant byte is stored first and the most
-- significant byte is stored last
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word16LE 42))
-- [42,0]
word16LE :: Word16 -> BuildR
word16LE = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.word16LE x))
{-# INLINE word16LE #-}

-- | Convert an `Int16` to a `BuildR` by storing the bytes in big-endian order
--
-- In other words, the most significant byte is stored first and the least
-- significant byte is stored last
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (int16BE (-5)))
-- [255,251]
int16BE :: Int16 -> BuildR
int16BE = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.int16BE x))
{-# INLINE int16BE #-}

-- | Convert an `Int16` to a `BuildR` by storing the bytes in little-endian
-- order
--
-- In other words, the least significant byte is stored first and the most
-- significant byte is stored last
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (int16LE (-5)))
-- [251,255]
int16LE :: Int16 -> BuildR
int16LE = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.int16LE x))
{-# INLINE int16LE #-}

-- | Convert a `Word32` to a `BuildR` by storing the bytes in big-endian order
--
-- In other words, the most significant byte is stored first and the least
-- significant byte is stored last
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32BE 42))
-- [0,0,0,42]
word32BE :: Word32 -> BuildR
word32BE = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.word32BE x))
{-# INLINE word32BE #-}

-- | Convert a `Word32` to a `BuildR` by storing the bytes in little-endian
-- order
--
-- In other words, the least significant byte is stored first and the most
-- significant byte is stored last
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32LE 42))
-- [42,0,0,0]
word32LE :: Word32 -> BuildR
word32LE = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.word32LE x))
{-# INLINE word32LE #-}

-- | Convert an `Int32` to a `BuildR` by storing the bytes in big-endian order
--
-- In other words, the most significant byte is stored first and the least
-- significant byte is stored last
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (int32BE (-5)))
-- [255,255,255,251]
int32BE :: Int32 -> BuildR
int32BE = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.int32BE x))
{-# INLINE int32BE #-}

-- | Convert an `Int32` to a `BuildR` by storing the bytes in little-endian
-- order
--
-- In other words, the least significant byte is stored first and the most
-- significant byte is stored last
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (int32LE (-5)))
-- [251,255,255,255]
int32LE :: Int32 -> BuildR
int32LE = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.int32LE x))
{-# INLINE int32LE #-}

-- | Convert a `Word64` to a `BuildR` by storing the bytes in big-endian order
--
-- In other words, the most significant byte is stored first and the least
-- significant byte is stored last
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64BE 42))
-- [0,0,0,0,0,0,0,42]
word64BE :: Word64 -> BuildR
word64BE = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.word64BE x))
{-# INLINE word64BE #-}

-- | Convert a `Word64` to a `BuildR` by storing the bytes in little-endian
-- order
--
-- In other words, the least significant byte is stored first and the most
-- significant byte is stored last
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64LE 42))
-- [42,0,0,0,0,0,0,0]
word64LE :: Word64 -> BuildR
word64LE = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.word64LE x))
{-# INLINE word64LE #-}

-- | Convert an `Int64` to a `BuildR` by storing the bytes in big-endian order
--
-- In other words, the most significant byte is stored first and the least
-- significant byte is stored last
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (int64BE (-5)))
-- [255,255,255,255,255,255,255,251]
int64BE :: Int64 -> BuildR
int64BE = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.int64BE x))
{-# INLINE int64BE #-}

-- | Convert an `Int64` to a `BuildR` by storing the bytes in little-endian
-- order
--
-- In other words, the least significant byte is stored first and the most
-- significant byte is stored last
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (int64LE (-5)))
-- [251,255,255,255,255,255,255,255]
int64LE :: Int64 -> BuildR
int64LE = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.int64LE x))
{-# INLINE int64LE #-}

-- | Convert a `Float` to a `BuildR` by storing the bytes in IEEE-754 format in
-- big-endian order
--
-- In other words, the most significant byte is stored first and the least
-- significant byte is stored last
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (floatBE 4.2))
-- [64,134,102,102]
floatBE :: Float -> BuildR
floatBE = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.floatBE x))
{-# INLINE floatBE #-}

-- | Convert a `Float` to a `BuildR` by storing the bytes in IEEE-754 format in
-- little-endian order
--
-- In other words, the least significant byte is stored first and the most
-- significant byte is stored last
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (floatLE 4.2))
-- [102,102,134,64]
floatLE :: Float -> BuildR
floatLE = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.floatLE x))
{-# INLINE floatLE #-}

-- | Convert a `Double` to a `BuildR` by storing the bytes in IEEE-754 format
-- in big-endian order
--
-- In other words, the most significant byte is stored first and the least
-- significant byte is stored last
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (doubleBE 4.2))
-- [64,16,204,204,204,204,204,205]
doubleBE :: Double -> BuildR
doubleBE = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.doubleBE x))
{-# INLINE doubleBE #-}

-- | Convert a `Double` to a `BuildR` by storing the bytes in IEEE-754 format
-- in little-endian order
--
-- In other words, the least significant byte is stored first and the most
-- significant byte is stored last
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (doubleLE 4.2))
-- [205,204,204,204,204,204,16,64]
doubleLE :: Double -> BuildR
doubleLE = \x -> Prim.liftBoundedPrim (Prim.liftFixedPrim (Prim.doubleLE x))
{-# INLINE doubleLE #-}

-- | Convert an @ASCII@ `Char` to a `BuildR`
--
-- __Careful:__ If you provide a Unicode character that is not part of the
-- @ASCII@ alphabet this will only encode the lowest 7 bits
--
-- >>> char7 ';'
-- Proto3.Wire.Reverse.lazyByteString ";"
-- >>> char7 'λ' -- Example of truncation
-- Proto3.Wire.Reverse.lazyByteString ";"
char7 :: Char -> BuildR
char7 = word8 . (0x7F .&.) . fromIntegral . ord
{-# INLINE char7 #-}

-- | Convert an @ASCII@ `String` to a `BuildR`
--
-- __Careful:__ If you provide a Unicode `String` that has non-@ASCII@
-- characters then this will only encode the lowest 7 bits of each character
--
-- > string7 (x <> y) = string7 x <> string7 y
-- >
-- > string7 mempty = mempty
--
-- >>> string7 "ABC"
-- Proto3.Wire.Reverse.lazyByteString "ABC"
-- >>> string7 "←↑→↓" -- Example of truncation
-- Proto3.Wire.Reverse.lazyByteString "\DLE\DC1\DC2\DC3"
string7 :: String -> BuildR
string7 = foldMap char7
  -- TO DO: 'Data.ByteString.Builder' goes to considerably more effort.
  -- Could we do better here?

-- | Convert an @ISO/IEC 8859-1@ `Char` to a `BuildR`
--
-- __Careful:__ If you provide a Unicode character that is not part of the
-- @ISO/IEC 8859-1@ alphabet then this will only encode the lowest 8 bits
--
-- >>> char8 ';'
-- Proto3.Wire.Reverse.lazyByteString ";"
-- >>> char8 'λ' -- Example of truncation
-- Proto3.Wire.Reverse.lazyByteString "\187"
char8 :: Char -> BuildR
char8 = word8 . fromIntegral . ord
{-# INLINE char8 #-}

-- | Convert an @ISO/IEC 8859-1@ `String` to a `BuildR`
--
-- __Careful:__ If you provide a Unicode `String` that has non-@ISO/IEC 8859-1@
-- characters then this will only encode the lowest 8 bits of each character
--
-- > string8 (x <> y) = string8 x <> string8 y
-- >
-- > string8 mempty = mempty
--
-- >>> string8 "ABC"
-- Proto3.Wire.Reverse.lazyByteString "ABC"
-- >>> string8 "←↑→↓" -- Example of truncation
-- Proto3.Wire.Reverse.lazyByteString "\144\145\146\147"
string8 :: String -> BuildR
string8 = foldMap char8
  -- TO DO: 'Data.ByteString.Builder' goes to considerably more effort.
  -- Could we do better here?

-- | Convert a Unicode `Char` to a `BuildR` using a @UTF-8@ encoding
--
-- >>> charUtf8 'A'
-- Proto3.Wire.Reverse.lazyByteString "A"
-- >>> charUtf8 'λ'
-- Proto3.Wire.Reverse.lazyByteString "\206\187"
-- >>> charUtf8 (Data.Char.chr 0x7FF)
-- Proto3.Wire.Reverse.lazyByteString "\223\191"
-- >>> charUtf8 (Data.Char.chr 0x800)
-- Proto3.Wire.Reverse.lazyByteString "\224\160\128"
-- >>> charUtf8 (Data.Char.chr 0xFFFF)
-- Proto3.Wire.Reverse.lazyByteString "\239\191\191"
-- >>> charUtf8 (Data.Char.chr 0x10000)
-- Proto3.Wire.Reverse.lazyByteString "\240\144\128\128"
-- >>> charUtf8 (Data.Char.chr 0x10FFFF)
-- Proto3.Wire.Reverse.lazyByteString "\244\143\191\191"
charUtf8 :: Char -> BuildR
charUtf8 = \x -> Prim.liftBoundedPrim (Prim.charUtf8 x)
{-# INLINE charUtf8 #-}

-- | Convert a Unicode `String` to a `BuildR` using a @UTF-8@ encoding
--
-- > stringUtf8 (x <> y) = stringUtf8 x <> stringUtf8 y
-- >
-- > stringUtf8 mempty = mempty
--
-- >>> stringUtf8 "ABC"
-- Proto3.Wire.Reverse.lazyByteString "ABC"
-- >>> stringUtf8 "←↑→↓"
-- Proto3.Wire.Reverse.lazyByteString "\226\134\144\226\134\145\226\134\146\226\134\147"
-- >>> Data.ByteString.Lazy.hPutStr System.IO.stdout (toLazyByteString (stringUtf8 "←↑→↓\n"))
-- ←↑→↓
stringUtf8 :: String -> BuildR
stringUtf8 = foldMap charUtf8
  -- TO DO: 'Data.ByteString.Builder' goes to considerably more effort.
  -- Could we do better here?

-- | Convert a Unicode strict `T.Text` to a `BuildR` using a @UTF-8@ encoding
--
-- > textUtf8 (x <> y) = textUtf8 x <> textUtf8 y
-- >
-- > textUtf8 mempty = mempty
--
-- >>> textUtf8 "ABC"
-- Proto3.Wire.Reverse.lazyByteString "ABC"
-- >>> textUtf8 "←↑→↓"
-- Proto3.Wire.Reverse.lazyByteString "\226\134\144\226\134\145\226\134\146\226\134\147"
textUtf8 :: T.Text -> BuildR
#if MIN_VERSION_text(2,0,0)
textUtf8 = etaBuildR $ \(TI.Text arr off word8Count) ->
  -- For version 2 of the "text" package, the in-memory
  -- representation is UTF-8.  We can just write it out.
  withUnused $ \unused ->
      if word8Count <= unused
        then
          writeChunk arr off word8Count
        else
          let rest = word8Count - unused in
          writeChunk arr off rest <>
            reallocate rest <> writeChunk arr (off + rest) unused
    where
      writeChunk src off len =
        unsafeConsume len $ \dst -> unsafeSTToIO $
          TA.copyToPointer src off dst len
#else
textUtf8 = etaBuildR $ \txt@(TI.Text _ _ word16Count) ->
  -- For version 1 of the "text" package, the in-memory
  -- representation is UTF-16.  We must transcode to UTF-8.
  case TIF.reverseStream txt of
    TIF.Stream next t0 _ -> ensure bound (go t0)
      where
        -- Any non-surrogate UTF-16 word encodes a 'Char' whose UTF-8
        -- encoding involves at most 3 octets.  Any surrogate pair is
        -- two UTF-16 words that give rise to 4 octets.  Therefore we
        -- will see at most 3 UTF-8 bytes per UTF-16 word of input.
        --
        -- This is a significant overallocation in the ASCII case,
        -- where we see only one UTF-8 byte per UTF-16 word of input.
        -- If such overallocation becomes a problem, we could implement
        -- a prescan that computes the exact size required.
        --
        -- However, we anticipate that in most cases we will be
        -- building from many text chunks that individually much
        -- smaller than the overall size of the combined result,
        -- making overallocation relatively harmless.
        bound = 3 * word16Count

        go = etaBuildR $ \t1 -> case next t1 of
          TIF.Done -> mempty
          TIF.Skip t2 -> go t2
          TIF.Yield !ch t2 ->
            go t2 <> Prim.unsafeBuildBoundedPrim (Prim.charUtf8 ch)
#endif

-- | Convert a Unicode lazy `TL.Text` to a `BuildR` using a @UTF-8@ encoding
--
-- > lazyTextUtf8 (x <> y) = lazyTextUtf8 x <> lazyTextUtf8 y
-- >
-- > lazyTextUtf8 mempty = mempty
--
-- >>> lazyTextUtf8 "ABC"
-- Proto3.Wire.Reverse.lazyByteString "ABC"
-- >>> lazyTextUtf8 "←↑→↓"
-- Proto3.Wire.Reverse.lazyByteString "\226\134\144\226\134\145\226\134\146\226\134\147"
lazyTextUtf8 :: TL.Text -> BuildR
lazyTextUtf8 = TL.foldrChunks ((<>) . textUtf8) mempty

-- | Convert a `TS.ShortText` to a `BuildR` using a @UTF-8@ encoding.
--
-- > shortTextUtf8 (x <> y) = shortTextUtf8 x <> shortTextUtf8 y
-- >
-- > shortTextUtf8 mempty = mempty
--
-- >>> shortTextUtf8 "ABC"
-- Proto3.Wire.Reverse.lazyByteString "ABC"
-- >>> shortTextUtf8 "←↑→↓"
-- Proto3.Wire.Reverse.lazyByteString "\226\134\144\226\134\145\226\134\146\226\134\147"
shortTextUtf8 :: TS.ShortText -> BuildR
shortTextUtf8 = shortByteString . TS.toShortByteString

-- | Convert a `Word` to a `BuildR` using this variable-length encoding:
--
--   1. Convert the given value to a base 128 representation
--   without unnecessary digits (that is, omit zero digits
--   unless they are less significant than nonzero digits).
--
--   2. Present those base-128 digits in order of increasing
--   significance (that is, in little-endian order).
--
--   3. Add 128 to every digit except the most significant digit,
--   yielding a sequence of octets terminated by one that is <= 127.
--
-- This encoding is used in the wire format of Protocol Buffers version 3.
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar 42))
-- [42]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar 5376))
-- [128,42]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar (Data.Bits.shiftL 1 7 - 1)))
-- [127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar (Data.Bits.shiftL 1 7)))
-- [128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar (Data.Bits.shiftL 1 14 - 1)))
-- [255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar (Data.Bits.shiftL 1 14)))
-- [128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar (Data.Bits.shiftL 1 21 - 1)))
-- [255,255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar (Data.Bits.shiftL 1 21)))
-- [128,128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar (Data.Bits.shiftL 1 28 - 1)))
-- [255,255,255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar (Data.Bits.shiftL 1 28)))
-- [128,128,128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar (Data.Bits.shiftL 1 32 - 1)))
-- [255,255,255,255,15]
wordBase128LEVar :: Word -> BuildR
wordBase128LEVar = \x -> Prim.liftBoundedPrim (Prim.wordBase128LEVar x)
{-# INLINE wordBase128LEVar #-}

-- | Like 'wordBase128LEVar' but inlined, which may bloat your code.  On
-- the other hand, inlining an application to a constant may shrink your code.
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar_inline 42))
-- [42]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar_inline 5376))
-- [128,42]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar_inline (Data.Bits.shiftL 1 7 - 1)))
-- [127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar_inline (Data.Bits.shiftL 1 7)))
-- [128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar_inline (Data.Bits.shiftL 1 14 - 1)))
-- [255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar_inline (Data.Bits.shiftL 1 14)))
-- [128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar_inline (Data.Bits.shiftL 1 21 - 1)))
-- [255,255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar_inline (Data.Bits.shiftL 1 21)))
-- [128,128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar_inline (Data.Bits.shiftL 1 28 - 1)))
-- [255,255,255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar_inline (Data.Bits.shiftL 1 28)))
-- [128,128,128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (wordBase128LEVar_inline (Data.Bits.shiftL 1 32 - 1)))
-- [255,255,255,255,15]
wordBase128LEVar_inline :: Word -> BuildR
wordBase128LEVar_inline = \x ->
  Prim.liftBoundedPrim (Prim.wordBase128LEVar_inline x)
{-# INLINE wordBase128LEVar_inline #-}

-- | Convert a `Word32` to a `BuildR` using this variable-length encoding:
--
--   1. Convert the given value to a base 128 representation
--   without unnecessary digits (that is, omit zero digits
--   unless they are less significant than nonzero digits).
--
--   2. Present those base-128 digits in order of increasing
--   significance (that is, in little-endian order).
--
--   3. Add 128 to every digit except the most significant digit,
--   yielding a sequence of octets terminated by one that is <= 127.
--
-- This encoding is used in the wire format of Protocol Buffers version 3.
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar 42))
-- [42]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar 5376))
-- [128,42]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar (Data.Bits.shiftL 1 7 - 1)))
-- [127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar (Data.Bits.shiftL 1 7)))
-- [128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar (Data.Bits.shiftL 1 14 - 1)))
-- [255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar (Data.Bits.shiftL 1 14)))
-- [128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar (Data.Bits.shiftL 1 21 - 1)))
-- [255,255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar (Data.Bits.shiftL 1 21)))
-- [128,128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar (Data.Bits.shiftL 1 28 - 1)))
-- [255,255,255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar (Data.Bits.shiftL 1 28)))
-- [128,128,128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar (Data.Bits.shiftL 1 32 - 1)))
-- [255,255,255,255,15]
word32Base128LEVar :: Word32 -> BuildR
word32Base128LEVar = \x -> Prim.liftBoundedPrim (Prim.word32Base128LEVar x)
{-# INLINE word32Base128LEVar #-}

-- | Like 'word32Base128LEVar' but inlined, which may bloat your code.  On
-- the other hand, inlining an application to a constant may shrink your code.
--
-- Currently 'word32Base128LEVar' is fully inline, so this makes no difference,
-- but in future we might make different default space/speed tradeoffs.
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar_inline 42))
-- [42]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar_inline 5376))
-- [128,42]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar_inline (Data.Bits.shiftL 1 7 - 1)))
-- [127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar_inline (Data.Bits.shiftL 1 7)))
-- [128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar_inline (Data.Bits.shiftL 1 14 - 1)))
-- [255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar_inline (Data.Bits.shiftL 1 14)))
-- [128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar_inline (Data.Bits.shiftL 1 21 - 1)))
-- [255,255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar_inline (Data.Bits.shiftL 1 21)))
-- [128,128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar_inline (Data.Bits.shiftL 1 28 - 1)))
-- [255,255,255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar_inline (Data.Bits.shiftL 1 28)))
-- [128,128,128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word32Base128LEVar_inline (Data.Bits.shiftL 1 32 - 1)))
-- [255,255,255,255,15]
word32Base128LEVar_inline :: Word32 -> BuildR
word32Base128LEVar_inline = \x ->
  Prim.liftBoundedPrim (Prim.word32Base128LEVar_inline x)
{-# INLINE word32Base128LEVar_inline #-}

-- | Like 'word32Base128LEVar' but for 64-bit inputs.
--
-- Inlines when the value fits within 32 bits, but see
-- also 'word64Base128LEVar_inline', which always inlines.
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar 42))
-- [42]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar 5376))
-- [128,42]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar (Data.Bits.shiftL 1 7 - 1)))
-- [127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar (Data.Bits.shiftL 1 7)))
-- [128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar (Data.Bits.shiftL 1 14 - 1)))
-- [255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar (Data.Bits.shiftL 1 14)))
-- [128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar (Data.Bits.shiftL 1 21 - 1)))
-- [255,255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar (Data.Bits.shiftL 1 21)))
-- [128,128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar (Data.Bits.shiftL 1 28 - 1)))
-- [255,255,255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar (Data.Bits.shiftL 1 28)))
-- [128,128,128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar (Data.Bits.shiftL 1 32 - 1)))
-- [255,255,255,255,15]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar (Data.Bits.shiftL 1 32)))
-- [128,128,128,128,16]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar (Data.Bits.shiftL 1 56 - 1)))
-- [255,255,255,255,255,255,255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar (Data.Bits.shiftL 1 56)))
-- [128,128,128,128,128,128,128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar (Data.Bits.shiftL 1 60 - 1)))
-- [255,255,255,255,255,255,255,255,15]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar (Data.Bits.shiftL 1 60)))
-- [128,128,128,128,128,128,128,128,16]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar (Data.Bits.shiftL 1 63 - 1)))
-- [255,255,255,255,255,255,255,255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar (Data.Bits.shiftL 1 63)))
-- [128,128,128,128,128,128,128,128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar (- (1 :: Data.Word.Word64))))
-- [255,255,255,255,255,255,255,255,255,1]
word64Base128LEVar :: Word64 -> BuildR
word64Base128LEVar = \x -> Prim.liftBoundedPrim (Prim.word64Base128LEVar x)
{-# INLINE word64Base128LEVar #-}

-- | Like 'word64Base128LEVar' but inlined, which may bloat your code.  On
-- the other hand, inlining an application to a constant may shrink your code.
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline 42))
-- [42]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline 5376))
-- [128,42]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline (Data.Bits.shiftL 1 7 - 1)))
-- [127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline (Data.Bits.shiftL 1 7)))
-- [128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline (Data.Bits.shiftL 1 14 - 1)))
-- [255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline (Data.Bits.shiftL 1 14)))
-- [128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline (Data.Bits.shiftL 1 21 - 1)))
-- [255,255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline (Data.Bits.shiftL 1 21)))
-- [128,128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline (Data.Bits.shiftL 1 28 - 1)))
-- [255,255,255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline (Data.Bits.shiftL 1 28)))
-- [128,128,128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline (Data.Bits.shiftL 1 32 - 1)))
-- [255,255,255,255,15]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline (Data.Bits.shiftL 1 32)))
-- [128,128,128,128,16]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline (Data.Bits.shiftL 1 56 - 1)))
-- [255,255,255,255,255,255,255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline (Data.Bits.shiftL 1 56)))
-- [128,128,128,128,128,128,128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline (Data.Bits.shiftL 1 60 - 1)))
-- [255,255,255,255,255,255,255,255,15]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline (Data.Bits.shiftL 1 60)))
-- [128,128,128,128,128,128,128,128,16]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline (Data.Bits.shiftL 1 63 - 1)))
-- [255,255,255,255,255,255,255,255,127]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline (Data.Bits.shiftL 1 63)))
-- [128,128,128,128,128,128,128,128,128,1]
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (word64Base128LEVar_inline (- (1 :: Data.Word.Word64))))
-- [255,255,255,255,255,255,255,255,255,1]
word64Base128LEVar_inline :: Word64 -> BuildR
word64Base128LEVar_inline = \x ->
  Prim.liftBoundedPrim (Prim.word64Base128LEVar_inline x)
{-# INLINE word64Base128LEVar_inline #-}

-- | Essentially 'foldMap', but iterates right to left for efficiency.
--
-- See also: 'repeatedBuildR'
vectorBuildR :: Vector v a => (a -> BuildR) -> v a -> BuildR
vectorBuildR f = etaBuildR (foldlRVector (\acc x -> acc <> f x) mempty)
{-# INLINE vectorBuildR #-}

-- | Concatenates the given builders, iterating right to left where practical.
--
-- For example:
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (repeatedBuildR (map word8 [42,67])))
-- [42,67]
--
-- See also: 'repeatedFixedPrimR', 'vectorBuildR'
repeatedBuildR :: ToRepeated c BuildR => c -> BuildR
repeatedBuildR = etaBuildR (foldr (flip (<>)) mempty . reverseRepeated . toRepeated)
{-# INLINE repeatedBuildR #-}

-- | Concatenates the given fixed-width primitives, iterating right to left where practical
-- and consolidating space checks if the number of primitives has been predicted.
--
-- For example:
--
-- >>> Data.ByteString.Lazy.unpack (toLazyByteString (repeatedFixedPrimR (map Proto3.Wire.Reverse.Prim.word8 [42,67])))
-- [42,67]
--
-- See also: 'repeatedBuildR'
repeatedFixedPrimR :: (ToRepeated c (Prim.FixedPrim w), KnownNat w) => c -> BuildR
repeatedFixedPrimR = etaBuildR $ \c ->
  let ReverseRepeated prediction prims = toRepeated c in
  case prediction of
    Nothing -> foldr (\p a -> a <> Prim.liftBoundedPrim (Prim.liftFixedPrim p)) mempty prims
    Just count -> Prim.unsafeReverseFoldMapFixedPrim id count prims
{-# INLINE repeatedFixedPrimR #-}

-- | Exported for testing purposes only.
testWithUnused :: (Int -> BuildR) -> BuildR
testWithUnused = withUnused
{-# WARNING testWithUnused "Exported for testing purposes only." #-}
