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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module Main where

import           Control.Arrow         ( (&&&), second )
import           Control.Monad         ( guard, void )
import           Control.Monad.Trans.State ( StateT(..) )
import qualified Data.Bits             as Bits
import qualified Data.ByteString       as B
import qualified Data.ByteString.Builder.Extra as BB
import qualified Data.ByteString.Lazy  as BL
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Builder.Internal as BBI
import           Data.Either           ( isLeft )
import           Data.Foldable         ( toList )
import           Data.Functor.Identity ( Identity )
import           Data.Int
import qualified Data.IntMap.Lazy
import qualified Data.IntSet
import qualified Data.Map.Lazy
import           Data.Maybe            ( fromMaybe )
import           Data.List             ( sort )
import qualified Data.List.NonEmpty    as NE
import           Data.Proxy            ( Proxy(..) )
import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text.Lazy        as T
import qualified Data.Text.Short       as TS
import           Data.Typeable         ( Typeable, showsTypeRep, typeRep )
import qualified Data.Vector           as V
import qualified Data.Vector.Storable  as VS
import qualified Data.Vector.Unboxed   as VU
import           Data.Word             ( Word8, Word64 )
import           Foreign               ( sizeOf )
import qualified GHC.Exts

import           Proto3.Wire
import           Proto3.Wire.FoldR     ( FoldR )
import qualified Proto3.Wire.Builder   as Builder
import qualified Proto3.Wire.Reverse   as Reverse
import qualified Proto3.Wire.Encode    as Encode
import           Proto3.Wire.Encode.Repeated ( Repeated(..), ToRepeated(..), nullRepeated )
import qualified Proto3.Wire.Decode    as Decode

import qualified Test.DocTest
import           Test.QuickCheck       ( (===), Arbitrary )
import           Test.Tasty
import qualified Test.Tasty.HUnit      as HU
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = do
    Test.DocTest.doctest
      [ "-isrc"
      , "-fobject-code"
      , "-Wno-warnings-deprecations"
      , "src/Proto3/Wire/Builder.hs"
      , "src/Proto3/Wire/Reverse.hs"
      , "src/Proto3/Wire/Encode.hs"
      , "src/Proto3/Wire/Decode.hs"
      ]
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ roundTripTests
                          , buildSingleChunk
                          , buildRBufferSizes
                          , strictByteString
                          , lazyByteString
                          , decodeNonsense
                          , varIntHeavyTests
                          , packedLargeTests
                          , decodeWireRoundTrip
                          , toRepeatedTests
                          ]

data StringOrInt64 = TString T.Text | TInt64 Int64
    deriving (Show,Eq)

instance QC.Arbitrary StringOrInt64 where
    arbitrary = QC.oneof [ TString . T.pack <$> QC.arbitrary, TInt64 <$> QC.arbitrary ]

-- This just stress tests the fancy varint encodings with more randomness.
varIntHeavyTests :: TestTree
varIntHeavyTests = adjustOption (const $ QC.QuickCheckTests 10000) $
                            roundTrip "varInt uint test"
                                       (Encode.uint64 (fieldNumber 1))
                                       (one Decode.uint64 0 `at` fieldNumber 1)

roundTripTests :: TestTree
roundTripTests = testGroup "Roundtrip tests"
                           [ roundTrip "int32"
                                       (Encode.int32 (fieldNumber 1))
                                       (one Decode.int32 0 `at` fieldNumber 1)
                           , roundTrip "int64"
                                       (Encode.int64 (fieldNumber 1))
                                       (one Decode.int64 0 `at` fieldNumber 1)
                           , roundTrip "sint32"
                                       (Encode.sint32 (fieldNumber 1))
                                       (one Decode.sint32 0 `at` fieldNumber 1)
                           , roundTrip "sint64"
                                       (Encode.sint64 (fieldNumber 1))
                                       (one Decode.sint64 0 `at` fieldNumber 1)
                           , roundTrip "uint32"
                                       (Encode.uint32 (fieldNumber 1))
                                       (one Decode.uint32 0 `at` fieldNumber 1)
                           , roundTrip "uint64"
                                       (Encode.uint64 (fieldNumber 1))
                                       (one Decode.uint64 0 `at` fieldNumber 1)
                           , roundTrip "fixed32"
                                       (Encode.fixed32 (fieldNumber 1))
                                       (one Decode.fixed32 0 `at` fieldNumber 1)
                           , roundTrip "fixed64"
                                       (Encode.fixed64 (fieldNumber 1))
                                       (one Decode.fixed64 0 `at` fieldNumber 1)
                           , roundTrip "sfixed32"
                                       (Encode.sfixed32 (fieldNumber 1))
                                       (one Decode.sfixed32 0 `at` fieldNumber 1)
                           , roundTrip "sfixed64"
                                       (Encode.sfixed64 (fieldNumber 1))
                                       (one Decode.sfixed64 0 `at` fieldNumber 1)
                           , roundTrip "float"
                                       (Encode.float (fieldNumber 1))
                                       (one Decode.float 0 `at` fieldNumber 1)
                           , roundTrip "double"
                                       (Encode.double (fieldNumber 1))
                                       (one Decode.double 0 `at` fieldNumber 1)
                           , roundTrip "bool"
                                       (Encode.bool (fieldNumber 1))
                                       (one Decode.bool False `at` fieldNumber 1)
                           , roundTrip "text"
                                       (Encode.text (fieldNumber 1) . T.pack)
                                       (one (fmap T.unpack Decode.text) mempty `at`
                                            fieldNumber 1)
                           , roundTrip "shortText"
                                       (Encode.shortText (fieldNumber 1) . TS.pack)
                                       (one (fmap TS.unpack Decode.shortText) mempty `at`
                                            fieldNumber 1)
                           , roundTripFor (QC.oneof [QC.arbitrary, genManyOctets])
                                       "byteString"
                                       (Encode.byteString (fieldNumber 1) . B.pack)
                                       (one (fmap B.unpack Decode.byteString) mempty `at`
                                            fieldNumber 1)
                           , roundTripFor genLazyByteString
                                       "lazyByteString"
                                       (Encode.lazyByteString (fieldNumber 1))
                                       (one (Decode.lazyByteString) mempty `at`
                                            fieldNumber 1)
                           , roundTripFor (QC.oneof [QC.arbitrary, genManyOctets])
                                       "shortByteString"
                                       (Encode.shortByteString (fieldNumber 1) . BS.pack)
                                       (one (fmap BS.unpack Decode.shortByteString) mempty `at`
                                            fieldNumber 1)
                           , roundTrip "embedded"
                                       (Encode.embedded (fieldNumber 1) .
                                            Encode.int32 (fieldNumber 1))
                                       (fmap (fromMaybe 0)
                                             (Decode.embedded (one Decode.int32
                                                                   0 `at`
                                                                   fieldNumber 1))
                                            `at` fieldNumber 1)
                           , roundTrip "embeddedListPackedVarints"
                                       (Encode.embedded (fieldNumber 1) .
                                            Encode.packedVarints (fieldNumber 1))
                                       (fmap (fromMaybe [0,1,2,3,4])
                                             (Decode.embedded (one Decode.packedVarints []
                                                                   `at`
                                                                   fieldNumber 1))
                                            `at` fieldNumber 1)
                           , roundTrip "embeddedListPackedFixed32"
                                       (Encode.embedded (fieldNumber 1) .
                                            Encode.packedFixed32 (fieldNumber 1))
                                       (fmap (fromMaybe [0,1,2,3,4])
                                             (Decode.embedded (one Decode.packedFixed32 []
                                                                   `at`
                                                                   fieldNumber 1))
                                            `at` fieldNumber 1)
                           , roundTrip "embeddedListPackedFixed64"
                                       (Encode.embedded (fieldNumber 1) .
                                            Encode.packedFixed64 (fieldNumber 1))
                                       (fmap (fromMaybe [0,1,2,3,4])
                                             (Decode.embedded (one Decode.packedFixed64 []
                                                                   `at`
                                                                   fieldNumber 1))
                                            `at` fieldNumber 1)
                           , roundTrip "embeddedListPackedFloats"
                                       (Encode.embedded (fieldNumber 1) .
                                            Encode.packedFloats (fieldNumber 1))
                                       (fmap (fromMaybe [0,1,2,3,4])
                                             (Decode.embedded (one Decode.packedFloats []
                                                                   `at`
                                                                   fieldNumber 1))
                                            `at` fieldNumber 1)
                           , roundTrip "embeddedListPackedDoubles"
                                       (Encode.embedded (fieldNumber 1) .
                                            Encode.packedDoubles (fieldNumber 1))
                                       (fmap (fromMaybe [0,1,2,3,4])
                                             (Decode.embedded (one Decode.packedDoubles []
                                                                   `at`
                                                                   fieldNumber 1))
                                            `at` fieldNumber 1)
                           , roundTrip "embeddedListUnpacked"
                                       (Encode.embedded (fieldNumber 1) .
                                            (foldMap . Encode.int32) (fieldNumber 1))
                                       (fmap (fromMaybe [0,1,2,3,4])
                                             (Decode.embedded (repeated Decode.int32
                                                                   `at`
                                                                   fieldNumber 1))
                                            `at` fieldNumber 1)
                           , roundTrip "multiple fields"
                                       (\(a, b) -> Encode.int32 (fieldNumber 1)
                                                                a <>
                                            Encode.uint32 (fieldNumber 2) b)
                                       ((,) <$>
                                            one Decode.int32 0 `at`
                                                fieldNumber 1
                                            <*> one Decode.uint32 0 `at`
                                                fieldNumber 2)
                           , roundTrip "oneof"
                                        (\case Just (TString text) -> Encode.text (fieldNumber 3) text
                                               Just (TInt64 i)     -> Encode.int64 (fieldNumber 2) i
                                               Nothing             -> mempty
                                        )
                                        (oneof Nothing
                                               [ (fieldNumber 2, fmap TInt64  <$> Decode.optional Decode.int64)
                                               , (fieldNumber 3, fmap TString <$> Decode.optional Decode.text)
                                               ]
                                        )
                           , roundTrip "oneof-last"
                                        (\case Just (TString text) -> Encode.text (fieldNumber 3) "something" <> Encode.text (fieldNumber 3) text
                                               Just (TInt64 i)     -> Encode.int64 (fieldNumber 2) 20000000 <> Encode.int64 (fieldNumber 2) i
                                               Nothing             -> mempty
                                        )
                                        (oneof Nothing
                                               [ (fieldNumber 2, fmap TInt64  <$> Decode.optional Decode.int64)
                                               , (fieldNumber 3, fmap TString <$> Decode.optional Decode.text)
                                               ]
                                        )
                           ]

roundTrip :: (Show a, Eq a, Arbitrary a)
          => String
          -> (a -> Encode.MessageBuilder)
          -> Decode.Parser Decode.RawMessage a
          -> TestTree
roundTrip = roundTripFor QC.arbitrary

roundTripFor :: (Show a, Eq a)
             => QC.Gen a
             -> String
             -> (a -> Encode.MessageBuilder)
             -> Decode.Parser Decode.RawMessage a
             -> TestTree
roundTripFor gen name encode decode =
    QC.testProperty name $ QC.forAll gen $
        \x ->
            let bytes = Encode.toLazyByteString (encode x) in
            case Decode.parse decode (BL.toStrict bytes) of
                Left _ -> error "Could not decode encoded message"
                Right x' -> x === x'

genManyOctets :: QC.Gen [Word8]
genManyOctets =
  QC.vector =<< QC.choose (BB.smallChunkSize - 64, BB.smallChunkSize + 64)

genLazyByteString :: QC.Gen BL.ByteString
genLazyByteString = do
  octets <- genManyOctets
  let total = length octets
  splits <- QC.listOf (QC.choose (0, total))
  let go :: Int -> [Int] -> [Word8] -> [[Word8]]
      go x [] os = [take (total - x) os]
      go x (y : ys) os = let (o1, o2) = splitAt (y - x) os in o1 : go y ys o2
  pure $ BL.fromChunks $ map B.pack $ go 0 (sort splits) octets

decodeWireRoundTrip :: TestTree
decodeWireRoundTrip = QC.testProperty "decodeWire round trips" $
  \(inp :: [(FieldNumber, Int32)]) ->
    let bytes = Encode.toLazyByteString (foldMap (\(k, v) -> Encode.int32 k v) inp)
        x = map (second $ Decode.VarintField . fromIntegral) inp
    in case Decode.decodeWire (BL.toStrict bytes) of
          Left _ -> error "decodeWire failed"
          Right x' -> x === x'

buildSingleChunk :: TestTree
buildSingleChunk = HU.testCase "Legacy Builder creates a single chunk" $ do
  let chunks = length . BL.toChunks . Builder.toLazyByteString

      huge = B.replicate (BBI.maximalCopySize + 16) 1
      huge2 = Builder.byteString huge <> Builder.byteString huge

      hugeL = BL.fromChunks [huge, huge]
      hugeL2 = Builder.lazyByteString hugeL <> Builder.lazyByteString hugeL

  HU.assertBool "single chunk (strict)" $ chunks huge2 == 1
  HU.assertBool "single chunk (lazy)" $ chunks hugeL2 == 1

parseBytes :: Int64 -> StateT BL.ByteString Maybe BL.ByteString
parseBytes n = StateT $ \bl -> do
  let (prefix, suffix) = BL.splitAt n bl
  guard (BL.length prefix == n)
  pure (prefix, suffix)

-- | Parses a big-endian 64-bit unsigned integer.
parseWord64BE :: StateT BL.ByteString Maybe Word64
parseWord64BE = do
  let be n bl = maybe n (j n) (BL.uncons bl)
      j n (h, t) = be (256 * n + fromIntegral h) t
  be 0 <$> parseBytes 8

-- | Consumes and returns the longest prefix whose bytes
-- all satisfy the given predicate.  Never fails.
parseWhile :: (Word8 -> Bool) -> StateT BL.ByteString Maybe BL.ByteString
parseWhile p = StateT (Just . BL.span p)

-- | Run-length encode lazy a 'BL.ByteString'
-- for concise display in test results.
rle :: BL.ByteString -> [(Int, Word8)]
rle = map (NE.length &&& NE.head) . NE.group . BL.unpack

-- | Please adjust this expected size of the metadata header
-- to match that expected of the current implementation.
buildRMeta :: Int
buildRMeta = 2 * sizeOf (undefined :: Word) + sizeOf (undefined :: Double)

buildRSmallChunkSize :: Int
buildRSmallChunkSize = BBI.smallChunkSize - buildRMeta

buildRDefaultChunkSize :: Int
buildRDefaultChunkSize = BBI.defaultChunkSize - buildRMeta

-- | Encodes the given 64-bit unsigned integer in big-endian format.
encodeWord64BE :: Word64 -> B.ByteString
encodeWord64BE = B.pack . go 8
  where
    go n w
      | n <= 0 = []
      | otherwise = fromIntegral (Bits.shiftR w (8 * (n - 1))) : go (n - 1) w

-- | Writes the given byte into all the previously-unused
-- bytes in the current buffer.
fillUnused :: Word8 -> Reverse.BuildR
fillUnused = fillUnusedExcept 0

-- | Like 'fillUnused', but writes fewer bytes in order to leave
-- the specified number of bytes unused, unless we start with fewer,
-- in which case there is no change at all.
fillUnusedExcept :: Int -> Word8 -> Reverse.BuildR
fillUnusedExcept unusedRemaining w8 = Reverse.testWithUnused $ \u ->
  foldMap (const (Reverse.word8 w8)) [unusedRemaining + 1 .. u]
{-# NOINLINE fillUnusedExcept #-}
   -- In case rewrite rules would interfere with buffer boundaries,
   -- which may be fine normally, we forbid inlining of this probe.

buildRBufferSizes :: TestTree
buildRBufferSizes = HU.testCase "BuildR buffer sizes" $ do
  let builder1 m = Reverse.ensure (max 8 m) $ Reverse.testWithUnused $ \u ->
        Reverse.word64BE (fromIntegral u) <> fillUnusedExcept 8 7
      {-# NOINLINE builder1 #-}

  let builder3 =
        builder1 (buildRDefaultChunkSize + 1) <> builder1 0 <> builder1 0

  let encodedBytes :: BL.ByteString
      encodedBytes = Reverse.toLazyByteString builder3

  let parseBuffer :: StateT BL.ByteString Maybe Word64
      parseBuffer = do
        n <- parseWord64BE
        _ <- parseBytes (max 0 (fromIntegral n - 8))
        pure n

  let parseBuffer3 :: StateT BL.ByteString Maybe (Word64, Word64, Word64)
      parseBuffer3 = do
        x <- parseBuffer
        y <- parseBuffer
        z <- parseBuffer
        pure (x, y, z)

  let actual, expected :: Maybe ((Word64, Word64, Word64), [(Int, Word8)])
      actual = second rle <$> runStateT parseBuffer3 encodedBytes
      expected = Just ((t, s, f), [])
                   -- We build in reverse but parser forward; therefore
                   -- the initial allocation is the final component.
        where
          t = fromIntegral buildRDefaultChunkSize + 1
          s = fromIntegral buildRDefaultChunkSize
          f = fromIntegral buildRSmallChunkSize

  let msg = "run-length encoding of built bytes: " ++ show (rle encodedBytes)
  HU.assertEqual msg expected actual

strictByteString :: TestTree
strictByteString = HU.testCase "Strict ByteString BuildR" $ do
  -- Because the initial buffer has a distinctive size we can use
  -- to distinguish it from other buffers, we start with a string
  -- that does not fit in that buffer, so that we can check that
  -- the buffer is reused as-is after those strings, not reallocated.
  let builder1 = Reverse.testWithUnused $ \u -> Reverse.byteString $
        B.replicate (buildRSmallChunkSize + 1) 10 <>
        encodeWord64BE (fromIntegral u)
      {-# NOINLINE builder1 #-}

  -- Then we write strings that do fit within the initial buffer.
  let builder2 = Reverse.testWithUnused $ \u -> Reverse.byteString $
        B.replicate 3 20 <> encodeWord64BE (fromIntegral u)
      {-# NOINLINE builder2 #-}

  let builder3 = Reverse.testWithUnused $ \u -> Reverse.byteString $
        B.replicate 3 30 <> encodeWord64BE (fromIntegral u)
      {-# NOINLINE builder3 #-}

  -- Then we check the just-enough-room case, which incidentally
  -- ensures that we use enough of the initial buffer that it
  -- will not be recycled.
  let builder4 = ( Reverse.testWithUnused $ \u -> Reverse.byteString $
                     B.replicate 3 40 <> encodeWord64BE (fromIntegral u) )
                 <> fillUnusedExcept 11 (0xD0 - 4) <>
                 ( Reverse.testWithUnused $ \u -> Reverse.byteString $
                     encodeWord64BE (fromIntegral u) )
      {-# NOINLINE builder4 #-}

  -- Then the case of the almost-full-buffer with not quite enough room.
  let builder5 = ( Reverse.testWithUnused $ \u -> Reverse.byteString $
                     B.replicate 3 50 <> encodeWord64BE (fromIntegral u) )
                 <> fillUnusedExcept 10 (0xD0 - 5) <>
                 ( Reverse.testWithUnused $ \u -> Reverse.byteString $
                     encodeWord64BE (fromIntegral u) )
      {-# NOINLINE builder5 #-}

  -- Then the full-buffer case.
  let builder6 = ( Reverse.testWithUnused $ \u -> Reverse.byteString $
                     B.replicate 3 60 <> encodeWord64BE (fromIntegral u) )
                 <> fillUnused (0xD0 - 6) <>
                 ( Reverse.testWithUnused $ \u -> Reverse.byteString $
                     encodeWord64BE (fromIntegral u) )
      {-# NOINLINE builder6 #-}

  -- Check final unused.
  let builder7 = ( Reverse.testWithUnused $ \u -> Reverse.byteString $
                     B.replicate 3 70 <> encodeWord64BE (fromIntegral u) )
      {-# NOINLINE builder7 #-}

  let buildAll = builder7 <> builder6 <> builder5 <>
                 builder4 <> builder3 <> builder2 <> builder1

  let encodedBytes :: BL.ByteString
      encodedBytes = Reverse.toLazyByteString buildAll

  let parseFixed :: Int64 -> Word8 -> StateT BL.ByteString Maybe ()
      parseFixed n w = do
        bl <- parseBytes n
        guard (BL.all (w ==) bl)

  let parsePad :: Word8 -> StateT BL.ByteString Maybe ()
      parsePad = void . parseWhile . (==)

  let parseAll :: StateT BL.ByteString Maybe
                         ( Word64, (Word64, Word64), (Word64, Word64),
                           (Word64, Word64), Word64, Word64, Word64 )
      parseAll = do
        parseFixed 3 70
        u7 <- parseWord64BE

        parseFixed 3 60
        u6B <- parseWord64BE
        parsePad (0xD0 - 6)
        u6A <- parseWord64BE

        parseFixed 3 50
        u5B <- parseWord64BE
        parsePad (0xD0 - 5)
        u5A <- parseWord64BE

        parseFixed 3 40
        u4B <- parseWord64BE
        parsePad (0xD0 - 4)
        u4A <- parseWord64BE

        parseFixed 3 30
        u3 <- parseWord64BE

        parseFixed 3 20
        u2 <- parseWord64BE

        parseFixed (fromIntegral (buildRSmallChunkSize + 1)) 10
        u1 <- parseWord64BE

        pure (u7, (u6B, u6A), (u5B, u5A), (u4B, u4A), u3, u2, u1)

  let actual, expected ::
        Maybe ( ( Word64, (Word64, Word64), (Word64, Word64)
                , (Word64, Word64), Word64, Word64, Word64 )
              , [(Int, Word8)]
              )
      actual = second rle <$> runStateT parseAll encodedBytes
      expected = Just ((u7, (u6B,u6A), (u5B,u5A), (u4B, u4A), u3, u2, u1), [])
        where
          u1 = fromIntegral $ buildRSmallChunkSize  -- before we wrote anything
          u2 = fromIntegral $ buildRSmallChunkSize  -- bypassed unused buffer
          u3 = fromIntegral $ buildRSmallChunkSize - 11   -- after second write
          u4A = fromIntegral $ buildRSmallChunkSize - 22  -- after third write
          u4B = 11   -- after padding
          u5A = 0    -- buffer full from previous write
          u5B = 10   -- after padding
          u6A = fromIntegral $ buildRDefaultChunkSize
                     -- new buffer after bypassing used buffer
          u6B = 0    -- buffer completely full
          u7 = fromIntegral $ buildRDefaultChunkSize
                     -- new buffer after bypassing used buffer

  let msg = "run-length encoding of built bytes: " ++ show (rle encodedBytes)
  HU.assertEqual msg expected actual

lazyByteString :: TestTree
lazyByteString = HU.testCase "Strict ByteString BuildR" $ do
  -- Because the initial buffer has a distinctive size we can use
  -- to distinguish it from other buffers, we start with a string
  -- whose chunks do not fit in that buffer, so that we can check that
  -- the buffer is reused as-is after those strings, not reallocated.
  let builder1 = Reverse.testWithUnused $ \u -> Reverse.lazyByteString $
        BL.fromStrict ( B.replicate (buildRSmallChunkSize + 1) 12 ) <>
        BL.fromStrict ( B.replicate (buildRSmallChunkSize + 1) 11 ) <>
        BL.fromStrict ( B.replicate (buildRSmallChunkSize + 1) 10 <>
                        encodeWord64BE (fromIntegral u) )
      {-# NOINLINE builder1 #-}

  -- Then we write a string whose rightmost two chunks do fit
  -- within the initial buffer but whose leftmost chunk does
  -- not fit after the others are written.  We ensure that most
  -- of the initial buffer is consumed because otherwise it might
  -- be recycled, which would prevent us from detecting that some
  -- chunks were actually written to the buffer.
  let builder2 = Reverse.testWithUnused $ \u -> Reverse.lazyByteString $
        BL.fromStrict ( B.replicate 3 22 ) <>
        BL.fromStrict ( B.replicate (buildRSmallChunkSize + 1 - 14) 21 ) <>
        BL.fromStrict ( B.replicate 3 20 <> encodeWord64BE (fromIntegral u) )
      {-# NOINLINE builder2 #-}

  -- And a string that fits entirely within the second buffer.
  let builder3 = Reverse.testWithUnused $ \u -> Reverse.lazyByteString $
        BL.fromStrict ( B.replicate 3 32 ) <>
        BL.fromStrict ( B.replicate 3 31 ) <>
        BL.fromStrict ( B.replicate 3 30 <> encodeWord64BE (fromIntegral u) )
      {-# NOINLINE builder3 #-}

  -- Then we check the just-enough-room case.
  let builder4 =
        ( Reverse.testWithUnused $ \u -> Reverse.lazyByteString $
            BL.fromStrict (B.replicate 3 41) <>
            BL.fromStrict (B.replicate 3 40 <> encodeWord64BE (fromIntegral u))
        ) <> fillUnusedExcept 14 (0xD0 - 4) <>
        ( Reverse.testWithUnused $ \u -> Reverse.lazyByteString $
            BL.fromStrict (encodeWord64BE (fromIntegral u))
        )
      {-# NOINLINE builder4 #-}

  -- Then the case of the almost-full-buffer with not quite enough room.
  let builder5 =
        ( Reverse.testWithUnused $ \u -> Reverse.lazyByteString $
            BL.fromStrict (B.replicate 3 51) <>
            BL.fromStrict (B.replicate 3 50 <> encodeWord64BE (fromIntegral u))
        ) <> fillUnusedExcept 13 (0xD0 - 5) <>
        ( Reverse.testWithUnused $ \u -> Reverse.lazyByteString $
            BL.fromStrict (encodeWord64BE (fromIntegral u))
        )
      {-# NOINLINE builder5 #-}

  -- Then the full-buffer case.
  let builder6 =
        ( Reverse.testWithUnused $ \u -> Reverse.lazyByteString $
            BL.fromStrict (B.replicate 3 61) <>
            BL.fromStrict (B.replicate 3 60 <> encodeWord64BE (fromIntegral u))
        ) <> fillUnused (0xD0 - 6) <>
        ( Reverse.testWithUnused $ \u -> Reverse.lazyByteString $
            BL.fromStrict (encodeWord64BE (fromIntegral u))
        )
      {-# NOINLINE builder6 #-}

  -- Check final unused.
  let builder7 =
        ( Reverse.testWithUnused $ \u -> Reverse.lazyByteString $
            BL.fromStrict (B.replicate 3 70 <> encodeWord64BE (fromIntegral u))
        )
      {-# NOINLINE builder7 #-}

  let buildAll = builder7 <> builder6 <> builder5 <>
                 builder4 <> builder3 <> builder2 <> builder1

  let encodedBytes :: BL.ByteString
      encodedBytes = Reverse.toLazyByteString buildAll

  let parseFixed :: Int64 -> Word8 -> StateT BL.ByteString Maybe ()
      parseFixed n w = do
        bl <- parseBytes n
        guard (BL.all (w ==) bl)

  let parsePad :: Word8 -> StateT BL.ByteString Maybe ()
      parsePad = void . parseWhile . (==)

  let parseAll :: StateT BL.ByteString Maybe
                         ( Word64, (Word64, Word64), (Word64, Word64),
                           (Word64, Word64), Word64, Word64, Word64 )
      parseAll = do
        parseFixed 3 70
        u7 <- parseWord64BE

        parseFixed 3 61
        parseFixed 3 60
        u6B <- parseWord64BE
        parsePad (0xD0 - 6)
        u6A <- parseWord64BE

        parseFixed 3 51
        parseFixed 3 50
        u5B <- parseWord64BE
        parsePad (0xD0 - 5)
        u5A <- parseWord64BE

        parseFixed 3 41
        parseFixed 3 40
        u4B <- parseWord64BE
        parsePad (0xD0 - 4)
        u4A <- parseWord64BE

        parseFixed 3 32
        parseFixed 3 31
        parseFixed 3 30
        u3 <- parseWord64BE

        parseFixed 3 22
        parseFixed (fromIntegral (buildRSmallChunkSize + 1 - 14)) 21
        parseFixed 3 20
        u2 <- parseWord64BE

        parseFixed (fromIntegral (buildRSmallChunkSize + 1)) 12
        parseFixed (fromIntegral (buildRSmallChunkSize + 1)) 11
        parseFixed (fromIntegral (buildRSmallChunkSize + 1)) 10
        u1 <- parseWord64BE

        pure (u7, (u6B, u6A), (u5B, u5A), (u4B, u4A), u3, u2, u1)

  let actual, expected ::
        Maybe ( ( Word64, (Word64, Word64), (Word64, Word64)
                , (Word64, Word64), Word64, Word64, Word64 )
              , [(Int, Word8)]
              )
      actual = second rle <$> runStateT parseAll encodedBytes
      expected = Just ((u7, (u6B,u6A), (u5B,u5A), (u4B, u4A), u3, u2, u1), [])
        where
          u1 = fromIntegral $ buildRSmallChunkSize  -- before we wrote anything
          u2 = fromIntegral $ buildRSmallChunkSize  -- bypassed unused buffer
          u3 = fromIntegral $ buildRDefaultChunkSize -- after second write
          u4A = fromIntegral $ buildRDefaultChunkSize - 17 -- after third write
          u4B = 14   -- after padding
          u5A = 0    -- buffer full from previous write
          u5B = 13   -- after padding
          u6A = fromIntegral $ buildRDefaultChunkSize
                     -- new buffer after bypassing used buffer
          u6B = 0    -- buffer completely full
          u7 = fromIntegral $ buildRDefaultChunkSize
                     -- new buffer after bypassing used buffer

  let msg = "run-length encoding of built bytes: " ++ show (rle encodedBytes)
  HU.assertEqual msg expected actual

decodeNonsense :: TestTree
decodeNonsense = HU.testCase "Decoding a nonsensical string fails." $ do
  let decoded = Decode.parse (one Decode.fixed64 0 `at` fieldNumber 1) "test"
  HU.assertBool "decode fails" $ isLeft decoded

packedLargeTests :: TestTree
packedLargeTests = testGroup "Test packed encoders on large inputs"
  [ packedVarints_large
  , packedVarintsV_large
  , packedBoolsV_large
  , packedFixed32_large
  , packedFixed32V_large
  , packedFixed64_large
  , packedFixed64V_large
  , packedFloats_large
  , packedFloatsV_large
  , packedDoubles_large
  , packedDoublesV_large
  ]

packedVarints_large :: TestTree
packedVarints_large = HU.testCase "Large packedVarints" $ do
  let count = 40000
      encoded = Encode.toLazyByteString (Encode.packedVarints 13 [1 .. count])
      decoded = Decode.parse (one Decode.packedVarints [] `at` fieldNumber 13)
                             (BL.toStrict encoded)
  HU.assertEqual "round trip" (Right [1 .. count]) decoded

packedVarintsV_large :: TestTree
packedVarintsV_large = HU.testCase "Large packedVarintsV" $ do
  let count = 40000
      encoded = Encode.toLazyByteString
                  (Encode.packedVarintsV (1 +) 13 (V.fromList [1 .. count]))
      decoded = Decode.parse (one Decode.packedVarints [] `at` fieldNumber 13)
                             (BL.toStrict encoded)
  HU.assertEqual "round trip" (Right [2 .. count + 1]) decoded

packedBoolsV_large :: TestTree
packedBoolsV_large = HU.testCase "Large packedBoolsV" $ do
  let count = 40000 :: Int
      values = map (flip Bits.testBit 0) [1 .. count]
      encoded = Encode.toLazyByteString
                  (Encode.packedBoolsV not 13 (V.fromList values))
      decoded = Decode.parse (one Decode.packedVarints [] `at` fieldNumber 13)
                             (BL.toStrict encoded)
  HU.assertEqual "round trip" (Right (map (fromEnum . not) values)) decoded

packedFixed32_large :: TestTree
packedFixed32_large = HU.testCase "Large packedFixed32" $ do
  let count = 40000
      encoded = Encode.toLazyByteString (Encode.packedFixed32 13 [1 .. count])
      decoded = Decode.parse (one Decode.packedFixed32 [] `at` fieldNumber 13)
                             (BL.toStrict encoded)
  HU.assertEqual "round trip" (Right [1 .. count]) decoded

packedFixed32V_large :: TestTree
packedFixed32V_large = HU.testCase "Large packedFixed32V" $ do
  let count = 40000
      encoded = Encode.toLazyByteString
                  (Encode.packedFixed32V (1 +) 13 (V.fromList [1 .. count]))
      decoded = Decode.parse (one Decode.packedFixed32 [] `at` fieldNumber 13)
                             (BL.toStrict encoded)
  HU.assertEqual "round trip" (Right [2 .. count + 1]) decoded

packedFixed64_large :: TestTree
packedFixed64_large = HU.testCase "Large packedFixed64" $ do
  let count = 40000
      encoded = Encode.toLazyByteString (Encode.packedFixed64 13 [1 .. count])
      decoded = Decode.parse (one Decode.packedFixed64 [] `at` fieldNumber 13)
                             (BL.toStrict encoded)
  HU.assertEqual "round trip" (Right [1 .. count]) decoded

packedFixed64V_large :: TestTree
packedFixed64V_large = HU.testCase "Large packedFixed64V" $ do
  let count = 40000
      encoded = Encode.toLazyByteString
                  (Encode.packedFixed64V (1 +) 13 (V.fromList [1 .. count]))
      decoded = Decode.parse (one Decode.packedFixed64 [] `at` fieldNumber 13)
                             (BL.toStrict encoded)
  HU.assertEqual "round trip" (Right [2 .. count + 1]) decoded

packedFloats_large :: TestTree
packedFloats_large = HU.testCase "Large packedFloats" $ do
  let count = 40000
      encoded = Encode.toLazyByteString (Encode.packedFloats 13 [1 .. count])
      decoded = Decode.parse (one Decode.packedFloats [] `at` fieldNumber 13)
                             (BL.toStrict encoded)
  HU.assertEqual "round trip" (Right [1 .. count]) decoded

packedFloatsV_large :: TestTree
packedFloatsV_large = HU.testCase "Large packedFloatsV" $ do
  let count = 40000
      encoded = Encode.toLazyByteString
                  (Encode.packedFloatsV (1 +) 13 (V.fromList [1 .. count]))
      decoded = Decode.parse (one Decode.packedFloats [] `at` fieldNumber 13)
                             (BL.toStrict encoded)
  HU.assertEqual "round trip" (Right [2 .. count + 1]) decoded

packedDoubles_large :: TestTree
packedDoubles_large = HU.testCase "Large packedDoubles" $ do
  let count = 40000
      encoded = Encode.toLazyByteString (Encode.packedDoubles 13 [1 .. count])
      decoded = Decode.parse (one Decode.packedDoubles [] `at` fieldNumber 13)
                             (BL.toStrict encoded)
  HU.assertEqual "round trip" (Right [1 .. count]) decoded

packedDoublesV_large :: TestTree
packedDoublesV_large = HU.testCase "Large packedDoublesV" $ do
  let count = 40000
      encoded = Encode.toLazyByteString
                  (Encode.packedDoublesV (1 +) 13 (V.fromList [1 .. count]))
      decoded = Decode.parse (one Decode.packedDoubles [] `at` fieldNumber 13)
                             (BL.toStrict encoded)
  HU.assertEqual "round trip" (Right [2 .. count + 1]) decoded

data ExpectedCountPrediction c = NoCP | CorrectCP | SameCP (c -> Maybe Int)

toRepeatedTests :: TestTree
toRepeatedTests = testGroup "ToRepeated"
  [ test_nullRepeated
  , test_ToRepeated (SameCP countRepeated) genRepeated (reverse . toList . reverseRepeated)
  , test_ToRepeated CorrectCP QC.arbitrary (toList @Identity @Word8)
  , test_ToRepeated NoCP QC.arbitrary (id @[Word8])
  , test_ToRepeated NoCP ((NE.:|) <$> QC.arbitrary <*> QC.arbitrary) (toList @NE.NonEmpty @Word8)
  , test_ToRepeated CorrectCP (fmap V.fromList QC.arbitrary) (V.toList @Word8)
  , test_ToRepeated CorrectCP (fmap VS.fromList QC.arbitrary) (VS.toList @Word8)
  , test_ToRepeated CorrectCP (fmap VU.fromList QC.arbitrary) (VU.toList @Word8)
  , test_ToRepeated CorrectCP QC.arbitrary (toList @Data.Sequence.Seq @Word8)
  , test_ToRepeated CorrectCP QC.arbitrary (Data.Set.toAscList @Word8)
  , test_ToRepeated NoCP QC.arbitrary Data.IntSet.toAscList
  , test_ToRepeated CorrectCP QC.arbitrary (Data.Map.Lazy.toAscList @Int8 @Word8)
  , test_ToRepeated NoCP QC.arbitrary (Data.IntMap.Lazy.toAscList @Word8)
  ]
  where
    genRepeated :: QC.Gen (Repeated Word8)
    genRepeated = do
      predict <- QC.arbitrary
      xs <- QC.arbitrary
      pure ReverseRepeated
        { countRepeated = if predict then Just (length xs) else Nothing
        , reverseRepeated = GHC.Exts.fromList xs
        }

    test_nullRepeated :: TestTree
    test_nullRepeated =
      QC.testProperty "nullRepeated" $
        QC.forAll genRepeated $ \c ->
          nullRepeated c === null (reverseRepeated c)

    test_ToRepeated ::
      forall c e .
      ( ToRepeated c e
      , Show c
      , Typeable c
      , Eq e
      , Show e
      ) =>
      ExpectedCountPrediction c ->
      (QC.Gen c) ->
      (c -> [e]) ->
      TestTree
    test_ToRepeated expectedCP gen cToList =
      let cRep = typeRep (Proxy :: Proxy c) in
      QC.testProperty (showString "toRepeated @(" $ showsTypeRep cRep ")") $
        QC.forAll gen $ \(c :: c) ->
          let es :: [e]
              es = cToList c

              prediction :: Maybe Int
              reversed :: FoldR e
              ReverseRepeated prediction reversed = toRepeated c
          in
            QC.counterexample "correctly reversed elements" (toList reversed === reverse es)
            QC..&&.
            QC.counterexample "correct count prediction if any"
              (all @Maybe (== length es) prediction)
            QC..&&.
            case expectedCP of
              NoCP ->
                QC.counterexample "no count prediction" (prediction === Nothing)
              CorrectCP ->
                QC.counterexample "correct count prediction" (prediction === Just (length es))
              SameCP expected ->
                QC.counterexample "unchanged count prediction" (prediction === expected c)
