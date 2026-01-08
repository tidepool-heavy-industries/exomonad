-- | Tests for wire protocol encoding/decoding.
module ProtocolSpec (spec) where

import qualified Data.ByteString as BS
import Test.Hspec

import Tidepool.GHCi.Protocol (encodeLen, decodeLen)


spec :: Spec
spec = describe "Length Encoding" $ do

  it "encodes 0 correctly" $ do
    encodeLen 0 `shouldBe` BS.pack [0, 0, 0, 0]

  it "encodes 1 correctly" $ do
    encodeLen 1 `shouldBe` BS.pack [0, 0, 0, 1]

  it "encodes 255 correctly" $ do
    encodeLen 255 `shouldBe` BS.pack [0, 0, 0, 255]

  it "encodes 256 correctly" $ do
    encodeLen 256 `shouldBe` BS.pack [0, 0, 1, 0]

  it "encodes 65535 correctly" $ do
    encodeLen 65535 `shouldBe` BS.pack [0, 0, 255, 255]

  it "encodes 65536 correctly" $ do
    encodeLen 65536 `shouldBe` BS.pack [0, 1, 0, 0]

  it "encodes 16777215 correctly" $ do
    encodeLen 16777215 `shouldBe` BS.pack [0, 255, 255, 255]

  it "encodes 16777216 correctly" $ do
    encodeLen 16777216 `shouldBe` BS.pack [1, 0, 0, 0]

  it "decodes 0 correctly" $ do
    decodeLen (BS.pack [0, 0, 0, 0]) `shouldBe` 0

  it "decodes 1 correctly" $ do
    decodeLen (BS.pack [0, 0, 0, 1]) `shouldBe` 1

  it "decodes 256 correctly" $ do
    decodeLen (BS.pack [0, 0, 1, 0]) `shouldBe` 256

  it "decodes 16777216 correctly" $ do
    decodeLen (BS.pack [1, 0, 0, 0]) `shouldBe` 16777216

  it "round-trips small values" $ do
    decodeLen (encodeLen 42) `shouldBe` 42

  it "round-trips medium values" $ do
    decodeLen (encodeLen 12345) `shouldBe` 12345

  it "round-trips large values" $ do
    decodeLen (encodeLen 1000000) `shouldBe` 1000000

  it "handles invalid input (too short)" $ do
    decodeLen (BS.pack [0, 0, 0]) `shouldBe` 0

  it "handles invalid input (empty)" $ do
    decodeLen BS.empty `shouldBe` 0
