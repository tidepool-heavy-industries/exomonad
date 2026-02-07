{-
  Copyright 2016 Awake Networks

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

-- | Low level functions for writing the protobufs wire format.
--
-- Because protobuf messages are encoded as a collection of fields,
-- one can use the 'Monoid' instance for 'MessageBuilder' to encode multiple
-- fields.
--
-- One should be careful to make sure that 'FieldNumber's appear in
-- increasing order.
--
-- In protocol buffers version 3, all fields are optional. To omit a value
-- for a field, simply do not append it to the 'MessageBuilder'. One can
-- create functions for wrapping optional fields with a 'Maybe' type.
--
-- Similarly, repeated fields can be encoded by concatenating several values
-- with the same 'FieldNumber'.
--
-- For example:
--
-- > strings :: Foldable f => FieldNumber -> f String -> MessageBuilder
-- > strings = foldMap . string
-- >
-- > 1 `strings` Just "some string" <>
-- > 2 `strings` [ "foo", "bar", "baz" ]

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Proto3.Wire.Encode
    ( -- * `MessageBuilder` type
      MessageBuilder
    , reverseMessageBuilder
    , etaMessageBuilder
    , vectorMessageBuilder
    , messageLength
    , toLazyByteString
    , unsafeFromLazyByteString
    , unsafeFromByteString
    , unsafeFromShortByteString

      -- * Standard Integers
    , int32
    , int64
      -- * Unsigned Integers
    , uint32
    , uint64
      -- * Signed Integers
    , sint32
    , sint64
      -- * Non-varint Numbers
    , fixed32
    , fixed64
    , sfixed32
    , sfixed64
    , float
    , double
    , enum
    , bool
      -- * Strings
    , bytes
    , bytesIfNonempty
    , string
    , text
    , shortText
    , byteString
    , lazyByteString
    , shortByteString
      -- * Embedded Messages
    , embedded
      -- * Folds
    , repeatedMessageBuilder
      -- * Packed repeated fields
    , packedVarints
    , packedVarintsV
    , packedInt32R
    , packedInt64R
    , packedUInt32R
    , packedUInt64R
    , packedSInt32R
    , packedSInt64R
    , packedBoolsR
    , packedBoolsV
    , packedFixed32
    , packedFixed32R
    , packedFixed32V
    , packedFixed64
    , packedFixed64R
    , packedFixed64V
    , packedSFixed32R
    , packedSFixed64R
    , packedFloats
    , packedFloatsR
    , packedFloatsV
    , packedDoubles
    , packedDoublesR
    , packedDoublesV
      -- * ZigZag codec
    , zigZagEncode
    ) where

import           Data.Bits                     ( (.|.), shiftL, shiftR, xor,
                                                 FiniteBits, finiteBitSize )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Short         as BS
import           Data.Coerce                   ( coerce )
import           Data.Int                      ( Int32, Int64 )
import qualified Data.Text.Lazy                as Text.Lazy
import qualified Data.Text.Short               as Text.Short
import           Data.Vector.Generic           ( Vector )
import           Data.Word                     ( Word8, Word32, Word64 )
import           GHC.TypeLits                  ( KnownNat, Nat, type (+) )
import           Parameterized.Data.Semigroup  ( PNullary, PSemigroup(..),
                                                 (&<>) )
import           Parameterized.Data.Monoid     ( PMEmpty(..) )
import           Proto3.Wire.Encode.Repeated   ( ToRepeated, mapRepeated )
import qualified Proto3.Wire.Reverse           as RB
import qualified Proto3.Wire.Reverse.Prim      as Prim
import           Proto3.Wire.Class
import           Proto3.Wire.Types

-- $setup
-- >>> :set -XOverloadedStrings -XOverloadedLists -XTypeApplications
-- >>> :module Proto3.Wire.Encode Proto3.Wire.Class Data.Word

-- | zigzag-encoded numeric type.
zigZagEncode :: (Num a, FiniteBits a) => a -> a
zigZagEncode i = (i `shiftL` 1) `xor` (i `shiftR` n)
  where n = finiteBitSize i - 1
{-# INLINE zigZagEncode #-}

-- | A `MessageBuilder` represents a serialized protobuf message
--
-- Use the utilities provided by this module to create `MessageBuilder`s
--
-- You can concatenate two messages using the `Monoid` instance for
-- `MessageBuilder`
--
-- Use `toLazyByteString` when you're done assembling the `MessageBuilder`
newtype MessageBuilder = MessageBuilder { unMessageBuilder :: RB.BuildR }
  deriving newtype (Monoid, Semigroup)

instance Show MessageBuilder where
  showsPrec prec builder =
      showParen (prec > 10)
        (showString "Proto3.Wire.Encode.unsafeFromLazyByteString " . shows bytes')
    where
      bytes' = toLazyByteString builder

-- | Convert a message builder to a 'RB.BuildR'.
reverseMessageBuilder :: MessageBuilder -> RB.BuildR
reverseMessageBuilder = unMessageBuilder

-- | Eta-expands a function that produces a 'MessageBuilder', so that
-- its input is not evaluated until the builder state is presented.
--
-- This odd combinator seems to help performance at times, though
-- it may change behavior on nonterminating values of type @a@.
etaMessageBuilder :: forall a . (a -> MessageBuilder) -> a -> MessageBuilder
etaMessageBuilder = coerce (RB.etaBuildR @a)

-- | Essentially 'foldMap', but iterates right to left for efficiency.
vectorMessageBuilder ::
  forall v a . Vector v a => (a -> MessageBuilder) -> v a -> MessageBuilder
vectorMessageBuilder = coerce (RB.vectorBuildR @v @a)

-- | O(n): Retrieve the length of a message, in bytes.
messageLength :: MessageBuilder -> Word
messageLength = fromIntegral . fst . RB.runBuildR . unMessageBuilder

-- | Convert a message to a lazy `BL.ByteString`
toLazyByteString :: MessageBuilder -> BL.ByteString
toLazyByteString = RB.toLazyByteString . unMessageBuilder

-- | This lets you cast an arbitrary 'BL.ByteString' to a `MessageBuilder`, whether
-- or not the `ByteString` corresponds to a valid serialized protobuf message
--
-- Do not use this function unless you know what you're doing because it lets
-- you assemble malformed protobuf `MessageBuilder`s.
unsafeFromLazyByteString :: BL.ByteString -> MessageBuilder
unsafeFromLazyByteString = coerce RB.lazyByteString

-- | Like 'unsafeFromLazyByteString' only for strict 'B.ByteString's.
unsafeFromByteString :: B.ByteString -> MessageBuilder
unsafeFromByteString = coerce RB.byteString

-- | Like 'unsafeFromLazyByteString' only for 'BS.ShortByteString's.
unsafeFromShortByteString :: BS.ShortByteString -> MessageBuilder
unsafeFromShortByteString = coerce RB.shortByteString

newtype MessageBoundedPrim w
  = MessageBoundedPrim { unMessageBoundedPrim :: Prim.BoundedPrim w }

type instance PNullary MessageBoundedPrim width = MessageBoundedPrim width

instance (w1 + w2) ~ w3 =>
         PSemigroup MessageBoundedPrim w1 w2 w3
  where
    pmappend = coerce (pmappend @Nat @Prim.BoundedPrim)
    {-# INLINE CONLIKE pmappend #-}

instance Prim.AssocPlusNat MessageBoundedPrim u v w
  where
    assocLPlusNat = \p -> coerce (Prim.assocLPlusNat @Prim.BoundedPrim p)
    {-# INLINE CONLIKE assocLPlusNat #-}

    assocRPlusNat = \p -> coerce (Prim.assocRPlusNat @Prim.BoundedPrim p)
    {-# INLINE CONLIKE assocRPlusNat #-}

instance Prim.CommPlusNat MessageBoundedPrim u v
  where
    commPlusNat = \p -> coerce (Prim.commPlusNat @Prim.BoundedPrim p)
    {-# INLINE CONLIKE commPlusNat #-}

instance PMEmpty MessageBoundedPrim 0
  where
    pmempty = coerce (pmempty @Nat @Prim.BoundedPrim)
    {-# INLINE CONLIKE pmempty #-}

instance Prim.Max u v ~ w =>
         Prim.PChoose MessageBoundedPrim u v w
  where
    pbool = coerce (Prim.pbool @Prim.BoundedPrim)
    {-# INLINE CONLIKE pbool #-}

instance Prim.AssocMaxNat MessageBoundedPrim u v w
  where
    assocLMaxNat = \p -> coerce (Prim.assocLMaxNat @Prim.BoundedPrim p)
    {-# INLINE CONLIKE assocLMaxNat #-}

    assocRMaxNat = \p -> coerce (Prim.assocRMaxNat @Prim.BoundedPrim p)
    {-# INLINE CONLIKE assocRMaxNat #-}

instance Prim.CommMaxNat MessageBoundedPrim u v
  where
    commMaxNat = \p -> coerce (Prim.commMaxNat @Prim.BoundedPrim p)
    {-# INLINE CONLIKE commMaxNat #-}

liftBoundedPrim :: KnownNat w => MessageBoundedPrim w -> MessageBuilder
liftBoundedPrim (MessageBoundedPrim p) = MessageBuilder (Prim.liftBoundedPrim p)
{-# INLINE liftBoundedPrim #-}

base128Varint32 :: Word32 -> MessageBoundedPrim 5
base128Varint32 = MessageBoundedPrim . Prim.word32Base128LEVar
{-# INLINE base128Varint32 #-}

base128Varint64 :: Word64 -> MessageBoundedPrim 10
base128Varint64 = MessageBoundedPrim . Prim.word64Base128LEVar
{-# INLINE base128Varint64 #-}

base128Varint64_inline :: Word64 -> MessageBoundedPrim 10
base128Varint64_inline = MessageBoundedPrim . Prim.word64Base128LEVar_inline
{-# INLINE base128Varint64_inline #-}

wireType :: WireType -> Word8
wireType Varint = 0
wireType Fixed32 = 5
wireType Fixed64 = 1
wireType LengthDelimited = 2

fieldHeader :: FieldNumber -> WireType -> MessageBoundedPrim 10
fieldHeader = \num wt -> base128Varint64_inline
    ((getFieldNumber num `shiftL` 3) .|. fromIntegral (wireType wt))
{-# INLINE fieldHeader #-}

-- | Encode a 32-bit "standard" integer
--
-- For example:
--
-- >>> 1 `int32` 42
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b*"
-- >>> 1 `int64` (-42)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b\214\255\255\255\255\255\255\255\255\SOH"
--
-- NOTE: Protobuf encoding converts an @int32@ to a 64-bit unsigned value
-- before encoding it, not a 32-bit value (which would be more efficient).
--
-- To quote the specification: "If you use int32 or int64 as the type for
-- a negative number, the resulting varint is always ten bytes long..."
-- <https://developers.google.com/protocol-buffers/docs/encoding#varints>
int32 :: FieldNumber -> Int32 -> MessageBuilder
int32 = \(!num) i -> liftBoundedPrim $
    fieldHeader num Varint &<> base128Varint64 (fromIntegral i)
{-# INLINE int32 #-}

-- | Encode a 64-bit "standard" integer
--
-- For example:
--
-- >>> 1 `int32` 42
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b*"
-- >>> 1 `int64` (-42)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b\214\255\255\255\255\255\255\255\255\SOH"
int64 :: FieldNumber -> Int64 -> MessageBuilder
int64 = \(!num) i -> liftBoundedPrim $
    fieldHeader num Varint &<> base128Varint64 (fromIntegral i)
{-# INLINE int64 #-}

-- | Encode a 32-bit unsigned integer
--
-- For example:
--
-- >>> 1 `uint32` 42
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b*"
uint32 :: FieldNumber -> Word32 -> MessageBuilder
uint32 = \(!num) i -> liftBoundedPrim $
    fieldHeader num Varint &<> base128Varint32 i
{-# INLINE uint32 #-}

-- | Encode a 64-bit unsigned integer
--
-- For example:
--
-- >>> 1 `uint64` 42
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b*"
uint64 :: FieldNumber -> Word64 -> MessageBuilder
uint64 = \(!num) i -> liftBoundedPrim $
    fieldHeader num Varint &<> base128Varint64 i
{-# INLINE uint64 #-}

-- | Encode a 32-bit signed integer
--
-- For example:
--
-- >>> 1 `sint32` (-42)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\bS"
-- >>> 1 `sint32` maxBound
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b\254\255\255\255\SI"
-- >>> 1 `sint32` minBound
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b\255\255\255\255\SI"
sint32 :: FieldNumber -> Int32 -> MessageBuilder
sint32 = \(!num) i ->
  uint32 num (fromIntegral (zigZagEncode i))
{-# INLINE sint32 #-}

-- | Encode a 64-bit signed integer
--
-- For example:
--
-- >>> 1 `sint64` (-42)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\bS"
-- >>> 1 `sint64` maxBound
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b\254\255\255\255\255\255\255\255\255\SOH"
-- >>> 1 `sint64` minBound
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b\255\255\255\255\255\255\255\255\255\SOH"
sint64 :: FieldNumber -> Int64 -> MessageBuilder
sint64 = \(!num) i ->
  uint64 num (fromIntegral (zigZagEncode i))
{-# INLINE sint64 #-}

-- | Encode a fixed-width 32-bit integer
--
-- For example:
--
-- >>> 1 `fixed32` 42
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\r*\NUL\NUL\NUL"
fixed32 :: FieldNumber -> Word32 -> MessageBuilder
fixed32 = \(!num) i -> liftBoundedPrim $
    fieldHeader num Fixed32 &<>
    MessageBoundedPrim (Prim.liftFixedPrim (Prim.word32LE i))
{-# INLINE fixed32 #-}

-- | Encode a fixed-width 64-bit integer
--
-- For example:
--
-- >>> 1 `fixed64` 42
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\t*\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
fixed64 :: FieldNumber -> Word64 -> MessageBuilder
fixed64 = \(!num) i -> liftBoundedPrim $
    fieldHeader num Fixed64 &<>
    MessageBoundedPrim (Prim.liftFixedPrim (Prim.word64LE i))
{-# INLINE fixed64 #-}

-- | Encode a fixed-width signed 32-bit integer
--
-- For example:
--
-- > 1 `sfixed32` (-42)
sfixed32 :: FieldNumber -> Int32 -> MessageBuilder
sfixed32 = \(!num) i -> liftBoundedPrim $
    fieldHeader num Fixed32 &<>
    MessageBoundedPrim (Prim.liftFixedPrim (Prim.int32LE i))
{-# INLINE sfixed32 #-}

-- | Encode a fixed-width signed 64-bit integer
--
-- For example:
--
-- >>> 1 `sfixed64` (-42)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\t\214\255\255\255\255\255\255\255"
sfixed64 :: FieldNumber -> Int64 -> MessageBuilder
sfixed64 = \(!num) i -> liftBoundedPrim $
    fieldHeader num Fixed64 &<>
    MessageBoundedPrim (Prim.liftFixedPrim (Prim.int64LE i))
{-# INLINE sfixed64 #-}

-- | Encode a floating point number
--
-- For example:
--
-- >>> 1 `float` 3.14
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\r\195\245H@"
float :: FieldNumber -> Float -> MessageBuilder
float = \(!num) f -> liftBoundedPrim $
    fieldHeader num Fixed32 &<>
    MessageBoundedPrim (Prim.liftFixedPrim (Prim.floatLE f))
{-# INLINE float #-}

-- | Encode a double-precision number
--
-- For example:
--
-- >>> 1 `double` 3.14
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\t\US\133\235Q\184\RS\t@"
double :: FieldNumber -> Double -> MessageBuilder
double = \(!num) d -> liftBoundedPrim $
    fieldHeader num Fixed64 &<>
    MessageBoundedPrim (Prim.liftFixedPrim (Prim.doubleLE d))
{-# INLINE double #-}

-- | Encode a value with an enumerable type.
--
-- You should instantiate 'ProtoEnum' for a type in
-- order to emulate enums appearing in .proto files.
--
-- For example:
--
-- >>> :{
--     data Shape = Circle | Square | Triangle deriving (Bounded, Enum)
--     instance ProtoEnum Shape
--     data Gap = Gap0 | Gap3
--     instance ProtoEnum Gap where
--       toProtoEnumMay i = case i of
--         0 -> Just Gap0
--         3 -> Just Gap3
--         _ -> Nothing
--       fromProtoEnum g = case g of
--         Gap0 -> 0
--         Gap3 -> 3
-- :}
--
-- >>> 1 `enum` Triangle <> 2 `enum` Gap3
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b\STX\DLE\ETX"
enum :: ProtoEnum e => FieldNumber -> e -> MessageBuilder
enum = \(!num) e -> liftBoundedPrim $
    fieldHeader num Varint &<>
    base128Varint32 (fromIntegral @Int32 @Word32 (fromProtoEnum e))
{-# INLINE enum #-}

-- | Encode a boolean value
--
-- For example:
--
-- >>> 1 `bool` True
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b\SOH"
bool :: FieldNumber -> Bool -> MessageBuilder
bool = \(!num) b -> liftBoundedPrim $
    fieldHeader num Varint &<>
    MessageBoundedPrim
      (Prim.liftFixedPrim (Prim.word8 (fromIntegral (fromEnum b))))
      -- Using word8 instead of a varint encoder shrinks the width bound.
{-# INLINE bool #-}

-- | Encode a sequence of octets as a field of type 'bytes'.
--
-- But unless the field is @optional@ or part of a @oneof@,
-- you may wish to to use 'bytesIfNonempty' to skip the field
-- when the payload built by the argument turns out to be empty.
--
-- >>> 1 `bytes` (Proto3.Wire.Reverse.stringUtf8 "")
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\NUL"
-- >>> 1 `bytes` (Proto3.Wire.Reverse.stringUtf8 "testing")
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\atesting"
bytes :: FieldNumber -> RB.BuildR -> MessageBuilder
bytes !num = embedded num . MessageBuilder
{-# INLINE bytes #-}

-- | Like 'bytes' but omits the field if it would be empty, which
-- is useful when the field is not @optional@ and is not part of
-- a @oneof@, and therefore may be omitted entirely when empty.
--
-- >>> 1 `bytesIfNonempty` (Proto3.Wire.Reverse.stringUtf8 "")
-- Proto3.Wire.Encode.unsafeFromLazyByteString ""
-- >>> 1 `bytesIfNonempty` (Proto3.Wire.Reverse.stringUtf8 "testing")
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\atesting"
bytesIfNonempty :: FieldNumber -> RB.BuildR -> MessageBuilder
bytesIfNonempty !num bb =
    MessageBuilder (RB.withLengthOf prefix bb)
  where
    prefix len
      | 0 < len = Prim.liftBoundedPrim $
          unMessageBoundedPrim (fieldHeader num LengthDelimited) &<>
          Prim.wordBase128LEVar (fromIntegral @Int @Word len)
      | otherwise =
          mempty
    {-# INLINE prefix #-}
{-# INLINE bytesIfNonempty #-}

-- | Encode a UTF-8 string.
--
-- For example:
--
-- >>> 1 `string` "testing"
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\atesting"
string :: FieldNumber -> String -> MessageBuilder
string !num = embedded num . MessageBuilder . RB.stringUtf8
{-# INLINE string #-}

-- | Encode lazy `Text` as UTF-8
--
-- For example:
--
-- >>> 1 `text` "testing"
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\atesting"
text :: FieldNumber -> Text.Lazy.Text -> MessageBuilder
text !num = embedded num . MessageBuilder . RB.lazyTextUtf8
{-# INLINE text #-}

-- | Encode a `Text.Short.ShortText` as UTF-8.
--
-- For example:
--
-- >>> 1 `shortText` "testing"
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\atesting"
shortText :: FieldNumber -> Text.Short.ShortText -> MessageBuilder
shortText !num = embedded num . MessageBuilder . RB.shortTextUtf8
{-# INLINE shortText #-}

-- | Encode a collection of bytes in the form of a strict 'B.ByteString'.
--
-- For example:
--
-- >>> 1 `byteString` "testing"
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\atesting"
byteString :: FieldNumber -> B.ByteString -> MessageBuilder
byteString !num = embedded num . unsafeFromByteString
{-# INLINE byteString #-}

-- | Encode a lazy bytestring.
--
-- For example:
--
-- >>> 1 `lazyByteString` "testing"
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\atesting"
lazyByteString :: FieldNumber -> BL.ByteString -> MessageBuilder
lazyByteString !num = embedded num . unsafeFromLazyByteString
{-# INLINE lazyByteString #-}

-- | Encode a `BS.ShortByteString`.
--
-- For example:
--
-- >>> 1 `shortByteString` "testing"
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\atesting"
shortByteString :: FieldNumber -> BS.ShortByteString -> MessageBuilder
shortByteString !num = embedded num . unsafeFromShortByteString
{-# INLINE shortByteString #-}

-- | Concatenates the given builders, which typically build fields within the same message.
--
-- For example:
--
-- >>> repeatedMessageBuilder @[MessageBuilder] [1 `bool` True, 2 `int32` 42]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\b\SOH\DLE*"
repeatedMessageBuilder :: ToRepeated c MessageBuilder => c -> MessageBuilder
repeatedMessageBuilder =
  etaMessageBuilder (MessageBuilder . RB.repeatedBuildR . mapRepeated reverseMessageBuilder)
{-# INLINE repeatedMessageBuilder #-}

-- | Encodes a packed repeated field whose elements may vary in width.
packedVariableWidthFieldR ::
  ToRepeated c a => (a -> RB.BuildR) -> FieldNumber -> c -> MessageBuilder
packedVariableWidthFieldR f !num =
  etaMessageBuilder (embedded num . MessageBuilder . RB.repeatedBuildR . mapRepeated f)
{-# INLINE packedVariableWidthFieldR #-}

-- | Encodes a packed repeated field whose elements never vary in width.
packedFixedWidthFieldR ::
  (ToRepeated c a, KnownNat w) => (a -> Prim.FixedPrim w) -> FieldNumber -> c -> MessageBuilder
packedFixedWidthFieldR f !num =
  etaMessageBuilder (embedded num . MessageBuilder . RB.repeatedFixedPrimR . mapRepeated f)
{-# INLINE packedFixedWidthFieldR #-}

-- | Encode varints in the space-efficient packed format.
-- But consider 'packedVarintsV' or 'packedVarintsR', either of which may be faster.
--
-- >>> packedVarints 1 [1, 2, 3]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\ETX\SOH\STX\ETX"
packedVarints :: Foldable f => FieldNumber -> f Word64 -> MessageBuilder
packedVarints !num = etaMessageBuilder (embedded num . payload)
  where
    payload = foldMap (liftBoundedPrim . base128Varint64)
{-# INLINE packedVarints #-}

-- | A faster but more specialized variant of 'packedVarints'.
--
-- Generalizes 'packedVarintsV', provided that any new instance
-- of 'Vector' is given a corresponding instance of 'ToRepeated'.
packedVarints64R :: ToRepeated c Word64 => FieldNumber -> c -> MessageBuilder
packedVarints64R = packedVariableWidthFieldR RB.word64Base128LEVar
{-# INLINE packedVarints64R #-}

-- | Like 'packedVarints64R' but supports only 32-bit inputs,
-- which reduces on executable size in situations where we do
-- not need to support larger values.
packedVarints32R :: ToRepeated c Word32 => FieldNumber -> c -> MessageBuilder
packedVarints32R = packedVariableWidthFieldR RB.word32Base128LEVar
{-# INLINE packedVarints32R #-}

-- | A faster but more specialized variant of:
--
-- > \f num -> packedVarints num . fmap f
--
-- >>> packedVarintsV (subtract 10) 1 ([11, 12, 13] :: Data.Vector.Vector Word64)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\ETX\SOH\STX\ETX"
packedVarintsV ::
  Vector v a => (a -> Word64) -> FieldNumber -> v a -> MessageBuilder
packedVarintsV f !num = embedded num . payload
  where
    payload = vectorMessageBuilder (liftBoundedPrim . base128Varint64 . f)
{-# INLINE packedVarintsV #-}

-- | Encodes a packed repeated @int32@ field.
--
-- >>> packedInt32R @[_] 1 [42, -42]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\v*\214\255\255\255\255\255\255\255\255\SOH"
--
-- NOTE: Protobuf encoding converts an @int32@ to a 64-bit unsigned value
-- before encoding it, not a 32-bit value (which would be more efficient).
--
-- To quote the specification: "If you use int32 or int64 as the type for
-- a negative number, the resulting varint is always ten bytes long..."
-- <https://developers.google.com/protocol-buffers/docs/encoding#varints>
packedInt32R :: ToRepeated c Int32 => FieldNumber -> c -> MessageBuilder
packedInt32R !num xs =
  packedVarints64R num (mapRepeated (fromIntegral @Int32 @Word64) xs)
{-# INLINE packedInt32R #-}

-- | Encodes a packed repeated @int64@ field.
--
-- For example:
--
-- >>> packedInt64R @[_] 1 [42, -42]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\v*\214\255\255\255\255\255\255\255\255\SOH"
packedInt64R :: ToRepeated c Int64 => FieldNumber -> c -> MessageBuilder
packedInt64R !num xs =
  packedVarints64R num (mapRepeated (fromIntegral @Int64 @Word64) xs)
{-# INLINE packedInt64R #-}

-- | Encodes a packed repeated @uint32@ field.
--
-- For example:
--
-- >>> packedUInt32R @[_] 1 [42, 43, maxBound]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\a*+\255\255\255\255\SI"
packedUInt32R :: ToRepeated c Word32 => FieldNumber -> c -> MessageBuilder
packedUInt32R = packedVarints32R
{-# INLINE packedUInt32R #-}

-- | Encodes a packed repeated @uint64@ field.
--
-- For example:
--
-- >>> packedUInt64R @[_] 1 [42, 43, maxBound]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\f*+\255\255\255\255\255\255\255\255\255\SOH"
packedUInt64R :: ToRepeated c Word64 => FieldNumber -> c -> MessageBuilder
packedUInt64R = packedVarints64R
{-# INLINE packedUInt64R #-}

-- | Encodes a packed repeated @sint32@ field.
--
-- For example:
--
-- >>> packedSInt32R @[_] 1 [-42, maxBound, minBound]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\vS\254\255\255\255\SI\255\255\255\255\SI"
packedSInt32R :: ToRepeated c Int32 => FieldNumber -> c -> MessageBuilder
packedSInt32R !num xs =
  packedVarints32R num (mapRepeated (fromIntegral @Int32 @Word32 . zigZagEncode) xs)
{-# INLINE packedSInt32R #-}

-- | Encodes a packed repeated @sint64@ field.
--
-- For example:
--
-- >>> packedSInt64R @[_] 1 [-42, maxBound, minBound]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\NAKS\254\255\255\255\255\255\255\255\255\SOH\255\255\255\255\255\255\255\255\255\SOH"
packedSInt64R :: ToRepeated c Int64 => FieldNumber -> c -> MessageBuilder
packedSInt64R !num xs =
  packedVarints64R num (mapRepeated (fromIntegral @Int64 @Word64 . zigZagEncode) xs)
{-# INLINE packedSInt64R #-}

-- | A faster but more specialized variant of:
--
-- > \f -> packedVarintsR (fromIntegral . fromEnum . f)
--
-- Generalizes 'packedBoolsV', provided that any new instance
-- of 'Vector' is given a corresponding instance of 'ToRepeated'.
--
-- >>> packedBoolsR @[_] 1 [True, False]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\STX\SOH\NUL"
packedBoolsR :: ToRepeated c Bool => FieldNumber -> c -> MessageBuilder
packedBoolsR = packedFixedWidthFieldR (Prim.word8 . fromIntegral . fromEnum)
{-# INLINE packedBoolsR #-}

-- | A faster but more specialized variant of:
--
-- > \f -> packedVarintsV (fromIntegral . fromEnum . f)
--
-- >>> packedBoolsV not 1 ([False, True] :: Data.Vector.Vector Bool)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\STX\SOH\NUL"
packedBoolsV ::
  Vector v a => (a -> Bool) -> FieldNumber -> v a -> MessageBuilder
packedBoolsV f !num = embedded num . MessageBuilder . payload
  where
    payload = Prim.vectorFixedPrim (Prim.word8 . fromIntegral . fromEnum . f)
{-# INLINE packedBoolsV #-}

-- | Encode fixed-width Word32s in the space-efficient packed format.
-- But consider 'packedFixed32V' or 'packedFixed32R', either of which may be faster.
--
-- >>> packedFixed32 1 [1, 2, 3]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\f\SOH\NUL\NUL\NUL\STX\NUL\NUL\NUL\ETX\NUL\NUL\NUL"
packedFixed32 :: Foldable f => FieldNumber -> f Word32 -> MessageBuilder
packedFixed32 !num = etaMessageBuilder (embedded num . payload)
  where
    payload = foldMap (MessageBuilder . RB.word32LE)
{-# INLINE packedFixed32 #-}

-- | A faster but more specialized variant of 'packedFixed32'.
--
-- Generalizes 'packedFixed32V', provided that any new instance
-- of 'Vector' is given a corresponding instance of 'ToRepeated'.
--
-- >>> packedFixed32R @[_] 1 [1, 2, 3]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\f\SOH\NUL\NUL\NUL\STX\NUL\NUL\NUL\ETX\NUL\NUL\NUL"
packedFixed32R :: ToRepeated c Word32 => FieldNumber -> c -> MessageBuilder
packedFixed32R = packedFixedWidthFieldR Prim.word32LE
{-# INLINE packedFixed32R #-}

-- | A faster but more specialized variant of:
--
-- > \f num -> packedFixed32 num . fmap f
--
-- >>> packedFixed32V (subtract 10) 1 ([11, 12, 13] :: Data.Vector.Vector Word32)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\f\SOH\NUL\NUL\NUL\STX\NUL\NUL\NUL\ETX\NUL\NUL\NUL"
packedFixed32V ::
  Vector v a => (a -> Word32) -> FieldNumber -> v a -> MessageBuilder
packedFixed32V f !num = etaMessageBuilder (embedded num . payload)
  where
    payload = MessageBuilder . Prim.vectorFixedPrim (Prim.word32LE . f)
{-# INLINE packedFixed32V #-}

-- But consider 'packedFixed64V' or 'packedFixed64R', either of which may be faster.
--
-- >>> packedFixed64 1 [1, 2, 3]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\CAN\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
packedFixed64 :: Foldable f => FieldNumber -> f Word64 -> MessageBuilder
packedFixed64 !num = etaMessageBuilder (embedded num . payload)
  where
    payload = foldMap (MessageBuilder . RB.word64LE)
{-# INLINE packedFixed64 #-}

-- | A faster but more specialized variant of 'packedFixed64'.
--
-- Generalizes 'packedFixed64V', provided that any new instance
-- of 'Vector' is given a corresponding instance of 'ToRepeated'.
--
-- >>> packedFixed64R @[_] 1 [1, 2, 3]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\CAN\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
packedFixed64R :: ToRepeated c Word64 => FieldNumber -> c -> MessageBuilder
packedFixed64R = packedFixedWidthFieldR Prim.word64LE
{-# INLINE packedFixed64R #-}

-- | A faster but more specialized variant of:
--
-- > \f num -> packedFixed64 num . fmap f
--
-- >>> packedFixed64V (subtract 10) 1 ([11, 12, 13] :: Data.Vector.Vector Word64)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\CAN\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
packedFixed64V ::
  Vector v a => (a -> Word64) -> FieldNumber -> v a -> MessageBuilder
packedFixed64V f !num = etaMessageBuilder (embedded num . payload)
  where
    payload = MessageBuilder . Prim.vectorFixedPrim (Prim.word64LE . f)
{-# INLINE packedFixed64V #-}

-- | Encodes a packed repeated @sfixed32@ field.
--
-- For example:
--
-- >>> packedSFixed32R @[_] 1 [1, -2, 3]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\f\SOH\NUL\NUL\NUL\254\255\255\255\ETX\NUL\NUL\NUL"
packedSFixed32R :: ToRepeated c Int32 => FieldNumber -> c -> MessageBuilder
packedSFixed32R = packedFixedWidthFieldR Prim.int32LE
{-# INLINE packedSFixed32R #-}

-- | Encodes a packed repeated @sfixed64@ field.
--
-- For example:
--
-- >>> packedSFixed64R @[_] 1 [1, -2, 3]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\CAN\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\254\255\255\255\255\255\255\255\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
packedSFixed64R :: ToRepeated c Int64 => FieldNumber -> c -> MessageBuilder
packedSFixed64R = packedFixedWidthFieldR Prim.int64LE
{-# INLINE packedSFixed64R #-}

-- | Encode floats in the space-efficient packed format.
-- But consider 'packedFloatsV' or 'packedFloatsR', either of which may be faster.
--
-- >>> 1 `packedFloats` [1, 2, 3]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\f\NUL\NUL\128?\NUL\NUL\NUL@\NUL\NUL@@"
packedFloats :: Foldable f => FieldNumber -> f Float -> MessageBuilder
packedFloats !num = etaMessageBuilder (embedded num . payload)
  where
    payload = foldMap (MessageBuilder . RB.floatLE)
{-# INLINE packedFloats #-}

-- | A faster but more specialized variant of 'packedFloats'.
--
-- Generalizes 'packedFloatsV', provided that any new instance
-- of 'Vector' is given a corresponding instance of 'ToRepeated'.
--
-- >>> packedFloatsR @[_] 1 [1, 2, 3]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\f\NUL\NUL\128?\NUL\NUL\NUL@\NUL\NUL@@"
packedFloatsR :: ToRepeated c Float => FieldNumber -> c -> MessageBuilder
packedFloatsR = packedFixedWidthFieldR Prim.floatLE
{-# INLINE packedFloatsR #-}

-- | A faster but more specialized variant of:
--
-- > \f num -> packedFloats num . fmap f
--
-- >>> packedFloatsV (subtract 10) 1 ([11, 12, 13] :: Data.Vector.Vector Float)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\f\NUL\NUL\128?\NUL\NUL\NUL@\NUL\NUL@@"
packedFloatsV ::
  Vector v a => (a -> Float) -> FieldNumber -> v a -> MessageBuilder
packedFloatsV f !num = etaMessageBuilder (embedded num . payload)
  where
    payload = MessageBuilder . Prim.vectorFixedPrim (Prim.floatLE . f)
{-# INLINE packedFloatsV #-}

-- | Encode doubles in the space-efficient packed format.
-- But consider 'packedDoublesV' or 'packedDoublesR', either of which may be faster.
--
-- >>> 1 `packedDoubles` [1, 2, 3]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\CAN\NUL\NUL\NUL\NUL\NUL\NUL\240?\NUL\NUL\NUL\NUL\NUL\NUL\NUL@\NUL\NUL\NUL\NUL\NUL\NUL\b@"
packedDoubles :: Foldable f => FieldNumber -> f Double -> MessageBuilder
packedDoubles !num = etaMessageBuilder (embedded num . payload)
  where
    payload = foldMap (MessageBuilder . RB.doubleLE)
{-# INLINE packedDoubles #-}

-- | A faster but more specialized variant of 'packedDoubles'.
--
-- Generalizes 'packedDoublesV', provided that any new instance
-- of 'Vector' is given a corresponding instance of 'ToRepeated'.
--
-- >>> packedDoublesR @[_] 1 [1, 2, 3]
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\CAN\NUL\NUL\NUL\NUL\NUL\NUL\240?\NUL\NUL\NUL\NUL\NUL\NUL\NUL@\NUL\NUL\NUL\NUL\NUL\NUL\b@"
packedDoublesR :: ToRepeated c Double => FieldNumber -> c -> MessageBuilder
packedDoublesR = packedFixedWidthFieldR Prim.doubleLE
{-# INLINE packedDoublesR #-}

-- | A faster but more specialized variant of:
--
-- > \f num -> packedDoubles num . fmap f
--
-- >>> packedDoublesV (subtract 10) 1 ([11, 12, 13] :: Data.Vector.Vector Double)
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\CAN\NUL\NUL\NUL\NUL\NUL\NUL\240?\NUL\NUL\NUL\NUL\NUL\NUL\NUL@\NUL\NUL\NUL\NUL\NUL\NUL\b@"
packedDoublesV ::
  Vector v a => (a -> Double) -> FieldNumber -> v a -> MessageBuilder
packedDoublesV f !num = etaMessageBuilder (embedded num . payload)
  where
    payload = MessageBuilder . Prim.vectorFixedPrim (Prim.doubleLE . f)
{-# INLINE packedDoublesV #-}

-- | Encode an embedded message.
--
-- The message is represented as a 'MessageBuilder', so it is possible to chain
-- encoding functions.
--
-- For example:
--
-- >>> 1 `embedded` mempty
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\NUL"
-- >>> 1 `embedded` (1 `string` "this message" <> 2 `string` " is embedded")
-- Proto3.Wire.Encode.unsafeFromLazyByteString "\n\FS\n\fthis message\DC2\f is embedded"
embedded :: FieldNumber -> MessageBuilder -> MessageBuilder
embedded = \(!num) (MessageBuilder bb) ->
    MessageBuilder (RB.withLengthOf (prefix num) bb)
  where
    prefix !num len = Prim.liftBoundedPrim $
      unMessageBoundedPrim (fieldHeader num LengthDelimited) &<>
      Prim.wordBase128LEVar (fromIntegral @Int @Word len)
    {-# INLINE prefix #-}
{-# INLINE embedded #-}
