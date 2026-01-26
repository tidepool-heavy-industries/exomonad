{-# LANGUAGE OverloadedStrings #-}

-- | Tests for GraphInput JSON encoding/decoding.
--
-- These tests ensure that:
-- 1. Round-tripping (encode then decode) preserves values
-- 2. JSON structure matches protocol.ts GraphInput type
-- 3. Aeson derives correct discriminated union format
module GraphInputSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, Value(..))
import Data.Aeson.KeyMap qualified as KM

import ExoMonad.Wasm.GraphInput
import ExoMonad.Anthropic.Types (ImageSource(..))


spec :: Spec
spec = do
  textInputSpec
  photoInputSpec
  jsonStructureSpec


-- ════════════════════════════════════════════════════════════════════════════
-- TextInput Round-Tripping
-- ════════════════════════════════════════════════════════════════════════════

textInputSpec :: Spec
textInputSpec = describe "TextInput" $ do

  it "round-trips simple text" $ do
    let input = TextInput "hello"
    decode (encode input) `shouldBe` Just input

  it "round-trips empty text" $ do
    let input = TextInput ""
    decode (encode input) `shouldBe` Just input

  it "round-trips unicode text" $ do
    let input = TextInput "Hello \x1F600 world 你好"
    decode (encode input) `shouldBe` Just input

  it "round-trips multiline text" $ do
    let input = TextInput "Line 1\nLine 2\nLine 3"
    decode (encode input) `shouldBe` Just input


-- ════════════════════════════════════════════════════════════════════════════
-- PhotoInput Round-Tripping
-- ════════════════════════════════════════════════════════════════════════════

photoInputSpec :: Spec
photoInputSpec = describe "PhotoInput" $ do

  it "round-trips photo with caption" $ do
    let imageSource = Base64Image "image/jpeg" "base64data"
        input = PhotoInput (Just "A photo of a cat") imageSource
    decode (encode input) `shouldBe` Just input

  it "round-trips photo without caption" $ do
    let imageSource = Base64Image "image/png" "SGVsbG8="
        input = PhotoInput Nothing imageSource
    decode (encode input) `shouldBe` Just input

  it "round-trips photo with empty caption" $ do
    let imageSource = Base64Image "image/gif" "R0lGODlh"
        input = PhotoInput (Just "") imageSource
    decode (encode input) `shouldBe` Just input

  it "round-trips photo with unicode caption" $ do
    let imageSource = Base64Image "image/jpeg" "data123"
        input = PhotoInput (Just "Photo: 📸 景色") imageSource
    decode (encode input) `shouldBe` Just input

  it "round-trips photo with URL source" $ do
    let imageSource = UrlImage "https://example.com/image.jpg"
        input = PhotoInput (Just "Remote image") imageSource
    decode (encode input) `shouldBe` Just input


-- ════════════════════════════════════════════════════════════════════════════
-- JSON Structure Validation
-- ════════════════════════════════════════════════════════════════════════════

jsonStructureSpec :: Spec
jsonStructureSpec = describe "JSON structure" $ do

  it "encodes TextInput with correct discriminator and field names" $ do
    let input = TextInput "hello world"
        json = decode (encode input) :: Maybe Value
    case json of
      Just (Object obj) -> do
        -- Should have discriminator field "type": "text"
        KM.lookup "type" obj `shouldBe` Just (String "text")
        -- Should have field "text" (mapped from textContent)
        KM.lookup "text" obj `shouldBe` Just (String "hello world")
        -- Should only have these two fields
        KM.size obj `shouldBe` 2
      _ -> expectationFailure "Expected JSON object"

  it "encodes PhotoInput with correct discriminator and field names" $ do
    let imageSource = Base64Image "image/jpeg" "SGVsbG8="
        input = PhotoInput (Just "test caption") imageSource
        json = decode (encode input) :: Maybe Value
    case json of
      Just (Object obj) -> do
        -- Should have discriminator field "type": "photo"
        KM.lookup "type" obj `shouldBe` Just (String "photo")
        -- Should have field "caption"
        KM.lookup "caption" obj `shouldBe` Just (String "test caption")
        -- Should have field "image" as an object
        case KM.lookup "image" obj of
          Just (Object imgObj) -> do
            KM.lookup "mediaType" imgObj `shouldBe` Just (String "image/jpeg")
            KM.lookup "data" imgObj `shouldBe` Just (String "SGVsbG8=")
          _ -> expectationFailure "Expected image field to be an object"
      _ -> expectationFailure "Expected JSON object"

  it "encodes PhotoInput without caption correctly" $ do
    let imageSource = Base64Image "image/png" "data"
        input = PhotoInput Nothing imageSource
        json = decode (encode input) :: Maybe Value
    case json of
      Just (Object obj) -> do
        KM.lookup "type" obj `shouldBe` Just (String "photo")
        -- caption field should be absent (not null)
        KM.member "caption" obj `shouldBe` False
      _ -> expectationFailure "Expected JSON object"

  it "parses TypeScript-generated text input" $ do
    -- Simulate what TypeScript sends: {"type":"text","text":"hello"}
    let json = "{\"type\":\"text\",\"text\":\"hello\"}"
        expected = TextInput "hello"
    decode json `shouldBe` Just expected

  it "parses TypeScript-generated photo input" $ do
    -- Simulate what TypeScript sends
    let json = "{\"type\":\"photo\",\"caption\":\"test\",\"image\":{\"mediaType\":\"image/jpeg\",\"data\":\"base64\"}}"
        expected = PhotoInput (Just "test") (Base64Image "image/jpeg" "base64")
    decode json `shouldBe` Just expected

  it "parses TypeScript-generated photo input without caption" $ do
    let json = "{\"type\":\"photo\",\"image\":{\"mediaType\":\"image/png\",\"data\":\"SGVsbG8=\"}}"
        expected = PhotoInput Nothing (Base64Image "image/png" "SGVsbG8=")
    decode json `shouldBe` Just expected
