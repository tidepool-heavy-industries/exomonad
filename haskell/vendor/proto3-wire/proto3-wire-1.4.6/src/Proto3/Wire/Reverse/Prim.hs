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

-- | Implementation details of the "Data.ByteString.Reverse" module.
-- Breaking changes will be more frequent in this module; use with caution.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Proto3.Wire.Reverse.Prim
  ( -- * Combine types such as `BoundedPrim` and `FixedPrim`.
    AssocPlusNat(..)
  , CommPlusNat(..)
  , PChoose(..)
  , Max
  , AssocMaxNat(..)
  , CommMaxNat(..)

    -- * Architectural attributes.
  , StoreMethod(..)
  , storeMethod
  , ByteOrder(..)
  , systemByteOrder

    -- * Bounded primitives.
  , BoundedPrim(..)
  , liftBoundedPrim
  , composeBoundedPrim
  , unsafeBuildBoundedPrim

    -- * Fixed-width primitives.
  , FixedPrim
  , liftFixedPrim
  , word8
  , word16
  , word16Native
  , word16BE
  , word16LE
  , word32
  , word32Native
  , word32BE
  , word32LE
  , word64
  , word64Native
  , word64BE
  , word64LE
  , int8
  , int16
  , int16Native
  , int16BE
  , int16LE
  , int32
  , int32Native
  , int32BE
  , int32LE
  , int64
  , int64Native
  , int64BE
  , int64LE
  , float
  , floatNative
  , floatBE
  , floatLE
  , double
  , doubleNative
  , doubleBE
  , doubleLE
  , charUtf8
  , wordBase128LEVar
  , wordBase128LEVar_inline
  , word32Base128LEVar
  , word32Base128LEVar_inline
  , word64Base128LEVar
  , word64Base128LEVar_inline
  , vectorFixedPrim
  , unsafeReverseFoldMapFixedPrim
  ) where

import           Data.Bits                     ( Bits(..) )
import           Data.Bool                     ( bool )
import           Data.Char                     ( ord )
import           Data.Int                      ( Int8, Int16, Int32, Int64 )
import           Data.Kind                     ( Type )
import qualified Data.Vector.Generic
import           Foreign                       ( Storable(..) )
import           GHC.Exts                      ( Addr#, Int#, Proxy#,
                                                 RealWorld, State#, (+#),
                                                 and#, inline, or#,
                                                 plusAddr#, plusWord#, proxy#,
                                                 uncheckedShiftRL# )
import           GHC.IO                        ( IO(..) )
import           GHC.Int                       ( Int(..) )
import           GHC.Ptr                       ( Ptr(..) )
import           GHC.TypeLits                  ( KnownNat, Nat,
                                                 type (+), natVal' )
import           GHC.Word.Compat
import           Parameterized.Data.Semigroup  ( PNullary, PSemigroup(..),
                                                 (&<>) )
import           Parameterized.Data.Monoid     ( PMEmpty(..) )
import           Proto3.Wire.Reverse.Internal
import           Proto3.Wire.Reverse.Width     ( AssocPlusNat(..),
                                                 CommPlusNat(..),
                                                 PChoose(..),
                                                 Max, AssocMaxNat(..),
                                                 CommMaxNat(..) )

#include <MachDeps.h>  /* for WORDS_BIGENDIAN and WORD_SIZE_IN_BITS */

import GHC.Exts (Word#)
type WORD64 = Word#

-- $setup
-- >>> :set -XOverloadedStrings

-- | Are we restricted to aligned writes only?
data StoreMethod = StoreAligned | StoreUnaligned
  deriving (Eq, Show)

-- | 'StoreUnaligned' if the Cabal file defines @UNALIGNED_POKES@, which it
-- does on architectures where that approach is known to be safe and faster
-- then writing bytes one by one.  Otherwise 'StoreAligned'.
storeMethod :: StoreMethod
#if defined(UNALIGNED_POKES)
storeMethod = StoreUnaligned
#else
storeMethod = StoreAligned
#endif

-- | Specifies order in which the bytes of an integer are encoded.
data ByteOrder
  = BigEndian     -- ^ Most significant byte first.
  | LittleEndian  -- ^ Least significant byte first.
  deriving (Eq, Show)

-- | The 'ByteOrder' native to the current architecture.
--
-- For example, the order of the bytes when you poke a 'Word32'.
systemByteOrder :: ByteOrder
-- WORDS_BIGENDIAN is defined for big-endian architectures
-- by the GHC header <MachDeps.h>.
#if defined(WORDS_BIGENDIAN)
systemByteOrder = BigEndian
#else
systemByteOrder = LittleEndian
#endif

-- | A 'BuildR' together with a type-level bound on the number of bytes
-- written and a requirement that the current buffer already contain at
-- least that many bytes.
--
-- As in the "bytestring" package, the purpose of a bounded primitive is to
-- improve speed by consolidating the space checks of several small builders.
newtype BoundedPrim (w :: Nat) = BoundedPrim BuildR

type role BoundedPrim nominal

type instance PNullary BoundedPrim width = BoundedPrim width

instance (w1 + w2) ~ w3 =>
         PSemigroup BoundedPrim w1 w2 w3
  where
    pmappend = composeBoundedPrim
    {-# INLINE CONLIKE pmappend #-}

instance AssocPlusNat BoundedPrim u v w
  where
    assocLPlusNat = assocLPlusNatBoundedPrim
    {-# INLINE CONLIKE assocLPlusNat #-}

    assocRPlusNat = assocRPlusNatBoundedPrim
    {-# INLINE CONLIKE assocRPlusNat #-}

instance CommPlusNat BoundedPrim u v
  where
    commPlusNat _ (BoundedPrim f) = BoundedPrim f
    {-# INLINE CONLIKE commPlusNat #-}

instance PMEmpty BoundedPrim 0
  where
    pmempty = BoundedPrim mempty
#ifdef ghcjs_HOST_OS
    {-# NOINLINE pmempty #-}
#else
    {-# INLINE CONLIKE pmempty #-}
#endif

instance Max u v ~ w =>
         PChoose BoundedPrim u v w
  where
    pbool = \(BoundedPrim f) (BoundedPrim g) -> BoundedPrim . bool f g
    {-# INLINE CONLIKE pbool #-}

instance AssocMaxNat BoundedPrim u v w
  where
    assocLMaxNat = \_ (BoundedPrim f) -> BoundedPrim f
    {-# INLINE CONLIKE assocLMaxNat #-}

    assocRMaxNat = \_ (BoundedPrim f) -> BoundedPrim f
    {-# INLINE CONLIKE assocRMaxNat #-}

instance CommMaxNat BoundedPrim u v
  where
    commMaxNat = \_ (BoundedPrim f) -> BoundedPrim f
    {-# INLINE CONLIKE commMaxNat #-}

-- | Like 'assocLPlusNat' but can be used in rules without
-- causing GHC to think the class dictionary is recursive.
assocLPlusNatBoundedPrim ::
  forall u v w .
  Proxy# '(u, v, w) -> BoundedPrim (u + (v + w)) -> BoundedPrim ((u + v) + w)
assocLPlusNatBoundedPrim = \_ (BoundedPrim f) -> BoundedPrim f
{-# INLINE CONLIKE assocLPlusNatBoundedPrim #-}

-- | Like 'assocRPlusNat' but can be used in rules without
-- causing GHC to think the class dictionary is recursive.
assocRPlusNatBoundedPrim ::
  forall u v w .
  Proxy# '(u, v, w) -> BoundedPrim ((u + v) + w) -> BoundedPrim (u + (v + w))
assocRPlusNatBoundedPrim = \_ (BoundedPrim f) -> BoundedPrim f
{-# INLINE CONLIKE assocRPlusNatBoundedPrim #-}

-- | Needed for rewrite rules; normally you would use 'pmappend' or '(&<>)'.
composeBoundedPrim :: BoundedPrim v -> BoundedPrim w -> BoundedPrim (v + w)
composeBoundedPrim =
  \(BoundedPrim f) (BoundedPrim g) -> BoundedPrim (f <> g)
{-# INLINE CONLIKE [1] composeBoundedPrim #-}

-- | Executes the bounded primitive WITHOUT first ensuring it has enough space.
unsafeBuildBoundedPrim :: BoundedPrim w -> BuildR
unsafeBuildBoundedPrim (BoundedPrim build) = build

-- | Executes the given bounded primitive
-- after obtaining the space it requires.
liftBoundedPrim :: forall w . KnownNat w => BoundedPrim w -> BuildR
liftBoundedPrim = case fromInteger (natVal' (proxy# :: Proxy# w)) of
  I# w -> unsafeLiftBoundedPrim w
{-# INLINE CONLIKE liftBoundedPrim #-}

-- | Needed for rewrite rules; normally you would use 'liftBoundedPrim'.
unsafeLiftBoundedPrim :: Int# -> BoundedPrim w -> BuildR
unsafeLiftBoundedPrim = \w (BoundedPrim f) -> ensure# w f
{-# INLINE CONLIKE [1] unsafeLiftBoundedPrim #-}

{-# RULES

"appendBuildR/unsafeLiftBoundedPrim" forall w1 w2 f1 f2 .
    appendBuildR (unsafeLiftBoundedPrim w1 f1) (unsafeLiftBoundedPrim w2 f2)
  = unsafeLiftBoundedPrim (w1 +# w2) (composeBoundedPrim f1 f2)

"appendBuildR/unsafeLiftBoundedPrim/assoc_r" forall w1 w2 f1 f2 b .
    appendBuildR (unsafeLiftBoundedPrim w1 f1)
                 (appendBuildR (unsafeLiftBoundedPrim w2 f2) b)
  = appendBuildR (unsafeLiftBoundedPrim (w1 +# w2) (composeBoundedPrim f1 f2)) b

"appendBuildR/unsafeLiftBoundedPrim/assoc_l" forall w1 w2 f1 f2 b .
    appendBuildR (appendBuildR b (unsafeLiftBoundedPrim w1 f1))
                 (unsafeLiftBoundedPrim w2 f2)
  = appendBuildR b (unsafeLiftBoundedPrim (w1 +# w2) (composeBoundedPrim f1 f2))

  #-}

-- | Similar to a 'BoundedPrim' but also consolidates address updates in
-- order to take advantage of machine instructions that write at an offset.
--
-- The additional input is an offset from the current address
-- that specifies the beginning of the region being encoded.
--
-- (If GHC learns to consolidate address offsets automatically
-- then we might be able to just use 'BoundedPrim' instead.)
newtype FixedPrim (w :: Nat) = FixedPrim
  ( Addr# -> Int# -> State# RealWorld -> Int# ->
    (# Addr#, Int#, State# RealWorld #)
  )

type role FixedPrim nominal

type instance PNullary FixedPrim width = FixedPrim width

instance ((w1 + w2) ~ w3, KnownNat w1) =>
         PSemigroup FixedPrim w1 w2 w3
  where
    pmappend = \(FixedPrim f) (FixedPrim g) ->
      case fromInteger (natVal' (proxy# :: Proxy# w1)) of
        I# w1 -> FixedPrim
          ( \v0 u0 s0 o -> case g v0 u0 s0 (o +# w1) of
             (# v1, u1, s1 #) -> f v1 u1 s1 o )
    {-# INLINE CONLIKE pmappend #-}

instance AssocPlusNat FixedPrim u v w
  where
    assocLPlusNat = \_ (FixedPrim f) -> FixedPrim f
    {-# INLINE CONLIKE assocLPlusNat #-}

    assocRPlusNat = \_ (FixedPrim f) -> FixedPrim f
    {-# INLINE CONLIKE assocRPlusNat #-}

instance CommPlusNat FixedPrim u v
  where
    commPlusNat = \_ (FixedPrim f) -> FixedPrim f
    {-# INLINE CONLIKE commPlusNat #-}

instance PMEmpty FixedPrim 0
  where
    pmempty = FixedPrim (\v u s _ -> (# v, u, s #))
    {-# INLINE CONLIKE pmempty #-}

-- | Executes the given fixed primitive and adjusts the current address.
liftFixedPrim :: forall w . KnownNat w => FixedPrim w -> BoundedPrim w
liftFixedPrim = \(FixedPrim f) -> BoundedPrim (BuildR (g f))
  where
    !(I# o) = - fromInteger (natVal' (proxy# :: Proxy# w))
    g = \f v0 u0 s0 -> case f v0 u0 s0 o of
      (# v1, u1, s1 #) -> (# plusAddr# v1 o, u1 +# o, s1 #)
    {-# INLINE g #-}
{-# INLINE CONLIKE [1] liftFixedPrim #-}

{-# RULES

"composeBoundedPrim/liftFixedPrim"
    forall (f1 :: KnownNat w1 => FixedPrim w1)
           (f2 :: KnownNat (w1 + w2) => FixedPrim w2).
    composeBoundedPrim (liftFixedPrim f1) (liftFixedPrim f2)
  = liftFixedPrim (pmappend f1 f2)

"composeBoundedPrim/liftFixedPrim/assoc_r"
    forall (f1 :: KnownNat w1 => FixedPrim w1)
           (f2 :: KnownNat (w1 + w2) => FixedPrim w2)
           (b3 :: BoundedPrim w3) .
    composeBoundedPrim (liftFixedPrim f1)
                       (composeBoundedPrim (liftFixedPrim f2) b3)
  = assocRPlusNatBoundedPrim (proxy# :: Proxy# '(w1, w2, w3))
      (composeBoundedPrim (liftFixedPrim (pmappend f1 f2)) b3)

"composeBoundedPrim/liftFixedPrim/assoc_l"
    forall (b1 :: BoundedPrim w1)
           (f2 :: KnownNat w2 => FixedPrim w2)
           (f3 :: KnownNat (w2 + w3) => FixedPrim w3) .
    composeBoundedPrim (composeBoundedPrim b1 (liftFixedPrim f2))
                       (liftFixedPrim f3)
  = assocLPlusNatBoundedPrim (proxy# :: Proxy# '(w1, w2, w3))
      (composeBoundedPrim b1 (liftFixedPrim (pmappend f2 f3)))

"withLengthOf#/unsafeLiftBoundedPrim/liftFixedPrim" forall f w g .
    withLengthOf# f (unsafeLiftBoundedPrim w (liftFixedPrim g))
  = appendBuildR (f w) (unsafeLiftBoundedPrim w (liftFixedPrim g))

  #-}

-- | Required:
--
-- > fromInteger (natVal' (proxy# :: Proxy# (StorableWidth a))) =
-- >   sizeOf (undefined :: x)
type family StorableWidth (a :: Type) :: Nat

type instance StorableWidth Word8 = 1
type instance StorableWidth Word16 = 2
type instance StorableWidth Word32 = 4
type instance StorableWidth Word64 = 8

type instance StorableWidth Int8 = 1
type instance StorableWidth Int16 = 2
type instance StorableWidth Int32 = 4
type instance StorableWidth Int64 = 8

type instance StorableWidth Float = 4
type instance StorableWidth Double = 8

-- | WARNING: The write may be unaligned; check 'storeMethod' first.
primPoke :: Storable x => x -> FixedPrim (StorableWidth x)
primPoke !x = FixedPrim p
  where
    p v u s0 o =
      let IO q = pokeByteOff (Ptr v) (I# o) x
      in case q s0 of (# s1, (_ :: ()) #) -> (# v, u, s1 #)

-- | Fixed-width primitive that writes a single byte as-is.
word8 :: Word8 -> FixedPrim 1
word8 = primPoke
  -- Byte order and alignment do not matter for a single byte.

-- | Shifts right by @s@ bits, then writes the least significant byte.
word8Shift :: Int -> Word -> FixedPrim 1
word8Shift s x = word8 (fromIntegral (shiftR x s))

-- | Shifts right by @s@ bits, then writes the least significant 16-bit word.
word16Shift :: ByteOrder -> Int -> Word -> FixedPrim 2
word16Shift bo = case bo of
    BigEndian    -> \(!s) (!x) -> p (s + h) x &<> p s x
    LittleEndian -> \(!s) (!x) -> p s x &<> p (s + h) x
  where
    h = 8
    p = word8Shift

-- | Writes the least significant 32-bit word, one byte at a time.
word32Shift :: ByteOrder -> Word -> FixedPrim 4
word32Shift bo = case bo of
    BigEndian    -> \(!x) -> p h x &<> p 0 x
    LittleEndian -> \(!x) -> p 0 x &<> p h x
  where
    h = 16
    p = word16Shift bo

-- | Writes one byte at a time.
word64Shift :: ByteOrder -> Word64 -> FixedPrim 8
word64Shift bo = case bo of
    BigEndian    -> \(!x) -> p (h x) &<> p x
    LittleEndian -> \(!x) -> p x &<> p (h x)
  where
    h x = shiftR x 32
    p = word32Shift bo . fromIntegral @Word64 @Word

-- | Fixed-width primitive that writes a 16-bit word
-- in the specified byte order.
word16 :: ByteOrder -> Word16 -> FixedPrim 2
word16 !bo !x = case storeMethod of
  StoreAligned -> word16Shift bo 0 (fromIntegral x)
  StoreUnaligned
    | systemByteOrder == bo -> primPoke x
    | otherwise -> primPoke (byteSwap16 x)

-- | Fixed-width primitive that writes a 16-bit word
-- in native byte order.
word16Native :: Word16 -> FixedPrim 2
word16Native = word16 systemByteOrder

-- | Fixed-width primitive that writes a 16-bit word
-- in big-endian byte order.
word16BE :: Word16 -> FixedPrim 2
word16BE = word16 BigEndian

-- | Fixed-width primitive that writes a 16-bit word
-- in little-endian byte order.
word16LE :: Word16 -> FixedPrim 2
word16LE = word16 LittleEndian

-- | Fixed-width primitive that writes a 32-bit word
-- in the specified byte order.
word32 :: ByteOrder -> Word32 -> FixedPrim 4
word32 !bo !x = case storeMethod of
  StoreAligned -> word32Shift bo (fromIntegral x)
  StoreUnaligned
    | systemByteOrder == bo -> primPoke x
    | otherwise -> primPoke (byteSwap32 x)

-- | Fixed-width primitive that writes a 32-bit word
-- in native byte order.
word32Native :: Word32 -> FixedPrim 4
word32Native = word32 systemByteOrder

-- | Fixed-width primitive that writes a 32-bit word
-- in big-endian byte order.
word32BE :: Word32 -> FixedPrim 4
word32BE = word32 BigEndian

-- | Fixed-width primitive that writes a 32-bit word
-- in little-endian byte order.
word32LE :: Word32 -> FixedPrim 4
word32LE = word32 LittleEndian

-- | Fixed-width primitive that writes a 64-bit word
-- in the specified byte order.
word64 :: ByteOrder -> Word64 -> FixedPrim 8
word64 !bo !x = case storeMethod of
  StoreAligned -> word64Shift bo (fromIntegral x)
  StoreUnaligned
    | systemByteOrder == bo -> primPoke x
    | otherwise -> primPoke (byteSwap64 x)

-- | Fixed-width primitive that writes a 64-bit word
-- in native byte order.
word64Native :: Word64 -> FixedPrim 8
word64Native = word64 systemByteOrder

-- | Fixed-width primitive that writes a 64-bit word
-- in big-endian byte order.
word64BE :: Word64 -> FixedPrim 8
word64BE = word64 BigEndian

-- | Fixed-width primitive that writes a 64-bit word
-- in little-endian byte order.
word64LE :: Word64 -> FixedPrim 8
word64LE = word64 LittleEndian

-- | @'word8' . 'fromIntegral'@
int8 :: Int8 -> FixedPrim 1
int8 = word8 . fromIntegral

-- | @\bo -> 'word16' bo . 'fromIntegral'@
int16 :: ByteOrder -> Int16 -> FixedPrim 2
int16 !bo = word16 bo . fromIntegral

-- | @'word16Native' . 'fromIntegral'@
int16Native :: Int16 -> FixedPrim 2
int16Native = word16Native . fromIntegral

-- | @'word16BE' . 'fromIntegral'@
int16BE :: Int16 -> FixedPrim 2
int16BE = word16BE . fromIntegral

-- | @'word16LE' . 'fromIntegral'@
int16LE :: Int16 -> FixedPrim 2
int16LE = word16LE . fromIntegral

-- | @\bo -> 'word32' bo . 'fromIntegral'@
int32 :: ByteOrder -> Int32 -> FixedPrim 4
int32 bo = word32 bo . fromIntegral

-- | @'word32Native' . 'fromIntegral'@
int32Native :: Int32 -> FixedPrim 4
int32Native = word32Native . fromIntegral

-- | @'word32BE' . 'fromIntegral'@
int32BE :: Int32 -> FixedPrim 4
int32BE = word32BE . fromIntegral

-- | @'word32LE' . 'fromIntegral'@
int32LE :: Int32 -> FixedPrim 4
int32LE = word32LE . fromIntegral

-- | @\bo -> 'word64' bo . 'fromIntegral'@
int64 :: ByteOrder -> Int64 -> FixedPrim 8
int64 bo = word64 bo . fromIntegral

-- | @'word64Native' . 'fromIntegral'@
int64Native :: Int64 -> FixedPrim 8
int64Native = word64Native . fromIntegral

-- | @'word64BE' . 'fromIntegral'@
int64BE :: Int64 -> FixedPrim 8
int64BE = word64BE . fromIntegral

-- | @'word64LE' . 'fromIntegral'@
int64LE :: Int64 -> FixedPrim 8
int64LE = word64LE . fromIntegral

-- | Fixed-width primitive that writes a 'Float'
-- in the specified byte order.
float :: ByteOrder -> Float -> FixedPrim 4
float BigEndian = floatBE
float LittleEndian = floatLE

-- | Fixed-width primitive that writes a 'Float'
-- in native byte order.
floatNative :: Float -> FixedPrim 4
floatNative = float systemByteOrder

-- | Fixed-width primitive that writes a 'Float'
-- in big-endian byte order.
floatBE :: Float -> FixedPrim 4
floatBE !x = FixedPrim g
  where
    g v u s0 o = case floatToWord32 (Ptr v) (I# u) x of
      IO h -> case h s0 of
        (# s1, y #) ->
          let FixedPrim f = word32BE y
          in f v u s1 o

-- | Fixed-width primitive that writes a 'Float'
-- in little-endian byte order.
floatLE :: Float -> FixedPrim 4
floatLE !x = FixedPrim g
  where
    g v u s0 o = case floatToWord32 (Ptr v) (I# u) x of
      IO h -> case h s0 of
        (# s1, y #) ->
          let FixedPrim f = word32LE y
          in f v u s1 o

-- | Fixed-width primitive that writes a 'Double'
-- in the specified byte order.
double :: ByteOrder -> Double -> FixedPrim 8
double BigEndian = doubleBE
double LittleEndian = doubleLE

-- | Fixed-width primitive that writes a 'Double'
-- in native byte order.
doubleNative :: Double -> FixedPrim 8
doubleNative = double systemByteOrder

-- | Fixed-width primitive that writes a 'Double'
-- in big-endian byte order.
doubleBE :: Double -> FixedPrim 8
doubleBE !x = FixedPrim g
  where
    g v u s0 o = case doubleToWord64 (Ptr v) (I# u) x of
      IO h -> case h s0 of
        (# s1, y #) ->
          let FixedPrim f = word64BE y
          in f v u s1 o

-- | Fixed-width primitive that writes a 'Double'
-- in little-endian byte order.
doubleLE :: Double -> FixedPrim 8
doubleLE !x = FixedPrim g
  where
    g v u s0 o = case doubleToWord64 (Ptr v) (I# u) x of
      IO h -> case h s0 of
        (# s1, y #) ->
          let FixedPrim f = word64LE y
          in f v u s1 o

-- | Bounded-width primitive that writes a 'Char'
-- according to the UTF-8 encoding.
charUtf8 :: Char -> BoundedPrim 4
charUtf8 = \ch -> case fromIntegral (ord ch) of W# x -> wordUtf8 x
  where
    wordUtf8 :: Word# -> BoundedPrim 4
    wordUtf8 =
      choose 0x7F p1 $
      choose 0x7FF p2 $
      choose 0xFFFF p3 $
      (\y -> liftFixedPrim (p4 y))
    {-# INLINE wordUtf8 #-}

    choose ::
      forall v w .
      (KnownNat v, KnownNat w) =>
      Word ->
      (Word# -> FixedPrim v) ->
      (Word# -> BoundedPrim w) ->
      Word# -> BoundedPrim (Max w v)
    choose = \t f g x -> pif (W# x <= t) (liftFixedPrim (f x)) (g x)
      -- We have observed GHC v8.6.5 jumping on the 'False' branch
      -- and falling through on the 'True' branch.  We set up our
      -- condition to favor lower character codes.
    {-# INLINE choose #-}

    lsb ::
      KnownNat n =>
      (Word# -> FixedPrim n) ->
      Word# ->
      FixedPrim (n + 1)
    lsb = \p x -> p (uncheckedShiftRL# x 6#) &<>
                  word8 (W8# (plusWord# 0x80## (and# x 0x3F##)))
    {-# INLINE lsb #-}

    p1 :: Word# -> FixedPrim 1
    p2 :: Word# -> FixedPrim 2
    p3 :: Word# -> FixedPrim 3
    p4 :: Word# -> FixedPrim 4

    p1 x = word8 (W8# x)
    p2 = lsb (\x -> word8 (W8# (plusWord# 0xC0## x)))
    p3 = lsb (lsb (\x -> word8 (W8# (plusWord# 0xE0## x))))
    p4 = lsb (lsb (lsb (\x -> word8 (W8# (plusWord# 0xF0## x)))))

    {-# INLINE p1 #-}
    {-# INLINE p2 #-}
    {-# INLINE p3 #-}
    {-# INLINE p4 #-}
{-# INLINE charUtf8 #-}

-- | The bounded primitive implementing
-- `Proto3.Wire.Reverse.wordBase128LEVar`.
#if WORD_SIZE_IN_BITS < 64
wordBase128LEVar :: Word -> BoundedPrim 5
wordBase128LEVar (W# w) = word32Base128LEVar (W32# w)
#else
wordBase128LEVar :: Word -> BoundedPrim 10
wordBase128LEVar (W# w) = word64Base128LEVar (W64# w)
#endif
{-# INLINE wordBase128LEVar #-}

-- | Like 'wordBase128LEVar' but inlined, possibly bloating your code.  On
-- the other hand, inlining an application to a constant may shrink your code.
#if WORD_SIZE_IN_BITS < 64
wordBase128LEVar_inline :: Word -> BoundedPrim 5
wordBase128LEVar_inline (W# w) = word32Base128LEVar_inline (W32# w)
#else
wordBase128LEVar_inline :: Word -> BoundedPrim 10
wordBase128LEVar_inline (W# w) = word64Base128LEVar_inline (W64# w)
#endif
{-# INLINE wordBase128LEVar_inline #-}

-- | The bounded primitive implementing
-- `Proto3.Wire.Reverse.word32Base128LEVar`.
word32Base128LEVar :: Word32 -> BoundedPrim 5
word32Base128LEVar (W32# x0) =
  ( wordBase128LEVar_choose 1 wordBase128LE_p1 $
    wordBase128LEVar_choose 2 wordBase128LE_p2 $
    wordBase128LEVar_choose 3 wordBase128LE_p3 $
    wordBase128LEVar_choose 4 wordBase128LE_p4 $
    (\x -> liftFixedPrim (wordBase128LE_p5 0## x))
  ) x0

-- | Like 'word32Base128LEVar' but inlined, which currently means
-- that it is just the same as 'word32Base128LEVar', which we inline.
word32Base128LEVar_inline :: Word32 -> BoundedPrim 5
word32Base128LEVar_inline = \(W32# x0) ->
  ( wordBase128LEVar_choose 1 wordBase128LE_p1 $
    wordBase128LEVar_choose 2 wordBase128LE_p2 $
    wordBase128LEVar_choose 3 wordBase128LE_p3 $
    wordBase128LEVar_choose 4 wordBase128LE_p4 $
    (\x -> liftFixedPrim (wordBase128LE_p5 0## x))
  ) x0
{-# INLINE word32Base128LEVar_inline #-}

wordBase128LEVar_choose ::
  forall v w .
  (KnownNat v, KnownNat w) =>
  Int ->
  (Word# -> Word# -> FixedPrim v) ->
  (Word# -> BoundedPrim w) ->
  Word# -> BoundedPrim (Max w v)
wordBase128LEVar_choose = \d f g x ->
  pif (W# x <= shiftL 1 (7 * d) - 1) (liftFixedPrim (f 0## x)) (g x)
  -- We have observed GHC v8.6.5 jumping on the 'False' branch
  -- and falling through on the 'True' branch.  We set up our
  -- condition to favor lower numeric values.
{-# INLINE wordBase128LEVar_choose #-}

wordBase128LE_msb ::
  forall n .
  KnownNat n =>
  (Word# -> Word# -> FixedPrim n) ->
  Word# -> Word# -> FixedPrim (n + 1)
wordBase128LE_msb = \p m x ->
    p 0x80## x &<> word8 (W8# (or# m (uncheckedShiftRL# x s)))
  where
    !(I# s) = 7 * fromInteger (natVal' (proxy# :: Proxy# n))
{-# INLINE wordBase128LE_msb #-}

wordBase128LE_p1 :: Word# -> Word# -> FixedPrim 1
wordBase128LE_p1 = \m x -> word8 (W8# (or# m x))
{-# INLINE wordBase128LE_p1 #-}

wordBase128LE_p2 :: Word# -> Word# -> FixedPrim 2
wordBase128LE_p2 = wordBase128LE_msb wordBase128LE_p1
{-# INLINE wordBase128LE_p2 #-}

wordBase128LE_p3 :: Word# -> Word# -> FixedPrim 3
wordBase128LE_p3 = wordBase128LE_msb wordBase128LE_p2
{-# INLINE wordBase128LE_p3 #-}

wordBase128LE_p4 :: Word# -> Word# -> FixedPrim 4
wordBase128LE_p4 = wordBase128LE_msb wordBase128LE_p3
{-# INLINE wordBase128LE_p4 #-}

wordBase128LE_p5 :: Word# -> Word# -> FixedPrim 5
wordBase128LE_p5 = wordBase128LE_msb wordBase128LE_p4
{-# INLINE wordBase128LE_p5 #-}

-- | Writes 1 or 2 base-128 digits in little-endian order;
-- in the 2-digit case the high bit of the containing byte of
-- the low digit is set, and the other byte has a clear high bit.
--
-- WARNING: The argument is ASSUMED to be in [0 .. 2^14 - 1].
word14Base128LEVar :: Word# -> BoundedPrim 2
word14Base128LEVar = \x0 ->
  ( wordBase128LEVar_choose 1 wordBase128LE_p1 $
    (\x -> liftFixedPrim (wordBase128LE_p2 0## x))
  ) x0
{-# INLINE word14Base128LEVar #-}

-- | Writes four base-128 digits, one per byte, with
-- the high bit of each byte set, in little-endian order.
--
-- There is no requirement that the argument be @< 2^28@.
word28Base128LE :: Word# -> FixedPrim 4
word28Base128LE = wordBase128LE_p4 0x80##
{-# INLINE word28Base128LE #-}

-- | The bounded primitive implementing
-- `Proto3.Wire.Reverse.word64Base128LEVar`.
word64Base128LEVar :: Word64 -> BoundedPrim 10
word64Base128LEVar = \(W64# x) ->
    pif (W64# x <= fromIntegral (maxBound :: Word32))
          (word32Base128LEVar (fromIntegral (W64# x)))
          (word64Base128LEVar_big x)

-- | Like 'word64Base128LEVar' but inlined, possibly bloating your code.  On
-- the other hand, inlining an application to a constant may shrink your code.
word64Base128LEVar_inline :: Word64 -> BoundedPrim 10
word64Base128LEVar_inline = \(W64# x) ->
    pif (W64# x <= fromIntegral (maxBound :: Word32))
          (word32Base128LEVar_inline (fromIntegral (W64# x)))
          (inline (word64Base128LEVar_big x))
{-# INLINE word64Base128LEVar_inline #-}

-- | The input must be at least 2^32.
word64Base128LEVar_big :: WORD64 -> BoundedPrim 10
word64Base128LEVar_big x = pif (W64# x <= shiftL 1 60 - 1) p60 p64
  where
    p60 = liftFixedPrim (word28Base128LE x32) &<>
          word32Base128LEVar (W32# (shR 28))

    p64 = ( liftFixedPrim (word28Base128LE x32) &<>
            liftFixedPrim (word28Base128LE (shR 28)) ) &<>
          word14Base128LEVar (shR 56)

    x32 = case fromIntegral (W64# x) of W32# y -> y

    shR s = case fromIntegral (shiftR (W64# x) s) of W32# y -> y
{-# NOINLINE word64Base128LEVar_big #-}

-- | The analog of `Proto3.Wire.Reverse.vectorBuildR` for when fixed-width
-- primitives encode the elements of the vector.  In this special case we
-- can predict the overall length.
vectorFixedPrim ::
  forall w v a .
  (KnownNat w, Data.Vector.Generic.Vector v a) =>
  (a -> FixedPrim w) ->
  v a ->
  BuildR
vectorFixedPrim f = etaBuildR $ \v ->
    let op acc x = acc <> unsafeBuildBoundedPrim (liftFixedPrim (f x))
    in ensure (w * Data.Vector.Generic.length v) (foldlRVector op mempty v)
  where
    w = fromInteger (natVal' (proxy# :: Proxy# w))
{-# INLINE vectorFixedPrim #-}

-- | Generalizes 'vectorFixedPrim' by mapping the elements
-- of a sequence of known length to fixed-width primitives
-- and concatenating those primitives /in reverse order/.
--
-- This action is unsafe because a length prediction that is too short
-- causes undefined behavior--possibly a crash.  By contrast, a length
-- prediction that is too long merely overallocates output space.
--
-- (To see how this function could be used to implement 'vectorFixedPrim',
-- note that we can iterate over a vector from right to left by reversing
-- the vector and then performing a right fold on the reversed vector;
-- vector fusion rules should eliminate the intermediate vector.)
unsafeReverseFoldMapFixedPrim ::
  forall w t a .
  (KnownNat w, Foldable t) =>
  (a -> FixedPrim w) ->
  -- | Maximum number of elements.  Overapproximation causes
  -- overallocation; passing an undercount may cause a crash.
  Int ->
  t a ->
  BuildR
unsafeReverseFoldMapFixedPrim f !n = etaBuildR $ \xs ->
    ensure (w * n) (foldr (\x acc -> acc <> unsafeBuildBoundedPrim (liftFixedPrim (f x))) mempty xs)
  where
    w = fromInteger (natVal' (proxy# :: Proxy# w))
{-# INLINE unsafeReverseFoldMapFixedPrim #-}
