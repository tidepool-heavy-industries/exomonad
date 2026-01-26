{-# LANGUAGE OverloadedStrings #-}

module ToolResultOutcomeSpec (spec) where

import Test.Hspec
import Data.Aeson (toJSON, fromJSON, encode, decode, Value(..), object, (.=), Result(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T

import ExoMonad.Wasm.WireTypes (ToolResultOutcome(..))
import ExoMonad.Wasm.Conversion (toToolResultOutcome, fromToolResultOutcome)
import ExoMonad.Effect.Types (ToolResult(..))

-- ════════════════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  describe "ToolResultOutcome serialization" $ do
    describe "round-trip JSON serialization" $ do
      it "TROSuccess with simple JSON value round-trips" $ do
        let original = TROSuccess (toJSON (42 :: Int))
        let json = encode original
        decode json `shouldBe` Just original

      it "TROSuccess with complex object round-trips" $ do
        let originalObj = object ["key" .= ("value" :: String), "count" .= (10 :: Int)]
        let original = TROSuccess originalObj
        let json = encode original
        decode json `shouldBe` Just original

      it "TROBreak with error message round-trips" $ do
        let original = TROBreak "tool execution failed"
        let json = encode original
        decode json `shouldBe` Just original

      it "TROBreak with empty message round-trips" $ do
        let original = TROBreak ""
        let json = encode original
        decode json `shouldBe` Just original

      it "TROTransition with target and payload round-trips" $ do
        let original = TROTransition "nodeA" (toJSON (123 :: Int))
        let json = encode original
        decode json `shouldBe` Just original

      it "TROTransition with complex payload round-trips" $ do
        let payload = object ["status" .= ("success" :: String), "data" .= (99 :: Int)]
        let original = TROTransition "nodeB" payload
        let json = encode original
        decode json `shouldBe` Just original

    describe "JSON structure validation" $ do
      it "TROSuccess has correct tag" $ do
        let outcome = TROSuccess (toJSON (42 :: Int))
        let json = BLC.unpack (encode outcome)
        json `shouldContain` "\"tag\":\"success\""

      it "TROSuccess includes value field" $ do
        let outcome = TROSuccess (toJSON (42 :: Int))
        let json = BLC.unpack (encode outcome)
        json `shouldContain` "\"value\":42"

      it "TROBreak has correct tag" $ do
        let outcome = TROBreak "error message"
        let json = BLC.unpack (encode outcome)
        json `shouldContain` "\"tag\":\"break\""

      it "TROBreak includes reason field" $ do
        let outcome = TROBreak "error message"
        let json = BLC.unpack (encode outcome)
        json `shouldContain` "\"reason\":\"error message\""

      it "TROTransition has correct tag" $ do
        let outcome = TROTransition "target" (toJSON (123 :: Int))
        let json = BLC.unpack (encode outcome)
        json `shouldContain` "\"tag\":\"transition\""

      it "TROTransition includes target field" $ do
        let outcome = TROTransition "nodeA" (toJSON (123 :: Int))
        let json = BLC.unpack (encode outcome)
        json `shouldContain` "\"target\":\"nodeA\""

      it "TROTransition includes payload field" $ do
        let outcome = TROTransition "target" (toJSON (123 :: Int))
        let json = BLC.unpack (encode outcome)
        json `shouldContain` "\"payload\":123"

    describe "JSON parsing errors" $ do
      it "rejects unknown tag" $ do
        let json = BLC.pack "{\"tag\":\"unknown\",\"value\":42}"
        (decode json :: Maybe ToolResultOutcome) `shouldBe` Nothing

      it "rejects missing tag" $ do
        let json = BLC.pack "{\"value\":42}"
        (decode json :: Maybe ToolResultOutcome) `shouldBe` Nothing

      it "rejects mismatched fields for success" $ do
        let json = BLC.pack "{\"tag\":\"success\"}"
        (decode json :: Maybe ToolResultOutcome) `shouldBe` Nothing

      it "rejects mismatched fields for break" $ do
        let json = BLC.pack "{\"tag\":\"break\"}"
        (decode json :: Maybe ToolResultOutcome) `shouldBe` Nothing

      it "rejects mismatched fields for transition" $ do
        let json = BLC.pack "{\"tag\":\"transition\",\"target\":\"nodeA\"}"
        (decode json :: Maybe ToolResultOutcome) `shouldBe` Nothing

  describe "Conversion between ToolResult and ToolResultOutcome" $ do
    describe "toToolResultOutcome preserves data" $ do
      it "converts ToolSuccess with integer value" $ do
        let toolResult = ToolSuccess (toJSON (42 :: Int))
        let outcome = toToolResultOutcome toolResult
        outcome `shouldBe` TROSuccess (toJSON (42 :: Int))

      it "converts ToolSuccess with string value" $ do
        let toolResult = ToolSuccess (toJSON ("result" :: String))
        let outcome = toToolResultOutcome toolResult
        outcome `shouldBe` TROSuccess (toJSON ("result" :: String))

      it "converts ToolSuccess with object value" $ do
        let value = object ["key" .= ("value" :: String)]
        let toolResult = ToolSuccess value
        let outcome = toToolResultOutcome toolResult
        outcome `shouldBe` TROSuccess value

      it "converts ToolBreak with reason" $ do
        let toolResult = ToolBreak "tool failed"
        let outcome = toToolResultOutcome toolResult
        outcome `shouldBe` TROBreak "tool failed"

      it "converts ToolTransition with target and payload" $ do
        let toolResult = ToolTransition "nodeA" (toJSON (100 :: Int))
        let outcome = toToolResultOutcome toolResult
        outcome `shouldBe` TROTransition "nodeA" (toJSON (100 :: Int))

    describe "fromToolResultOutcome restores ToolResult" $ do
      it "converts TROSuccess back to ToolSuccess" $ do
        let outcome = TROSuccess (toJSON (42 :: Int))
        let toolResult = fromToolResultOutcome outcome
        case toolResult of
          ToolSuccess val -> val `shouldBe` toJSON (42 :: Int)
          _ -> expectationFailure "Expected ToolSuccess"

      it "converts TROBreak back to ToolBreak" $ do
        let outcome = TROBreak "error occurred"
        let toolResult = fromToolResultOutcome outcome
        case toolResult of
          ToolBreak reason -> reason `shouldBe` "error occurred"
          _ -> expectationFailure "Expected ToolBreak"

      it "converts TROTransition back to ToolTransition" $ do
        let outcome = TROTransition "nodeX" (toJSON ("payload" :: String))
        let toolResult = fromToolResultOutcome outcome
        case toolResult of
          ToolTransition target payload ->
            (target, payload) `shouldBe` ("nodeX", toJSON ("payload" :: String))
          _ -> expectationFailure "Expected ToolTransition"

  describe "Round-trip conversion" $ do
    it "ToolSuccess -> wire -> ToolResult preserves value" $ do
      let original = ToolSuccess (toJSON (123 :: Int))
      let wire = toToolResultOutcome original
      let restored = fromToolResultOutcome wire
      case restored of
        ToolSuccess val -> val `shouldBe` toJSON (123 :: Int)
        _ -> expectationFailure "Round-trip failed"

    it "ToolBreak -> wire -> ToolResult preserves reason" $ do
      let original = ToolBreak "original error"
      let wire = toToolResultOutcome original
      let restored = fromToolResultOutcome wire
      case restored of
        ToolBreak reason -> reason `shouldBe` "original error"
        _ -> expectationFailure "Round-trip failed"

    it "ToolTransition -> wire -> ToolResult preserves target and payload" $ do
      let original = ToolTransition "myTarget" (toJSON (456 :: Int))
      let wire = toToolResultOutcome original
      let restored = fromToolResultOutcome wire
      case restored of
        ToolTransition target payload ->
          (target, payload) `shouldBe` ("myTarget", toJSON (456 :: Int))
        _ -> expectationFailure "Round-trip failed"

    it "wire -> JSON -> wire preserves structure" $ do
      let wire = TROTransition "nodeA" (toJSON (789 :: Int))
      let json = encode wire
      let decoded = decode json :: Maybe ToolResultOutcome
      decoded `shouldBe` Just wire

  describe "Edge cases" $ do
    it "handles null value in ToolSuccess" $ do
      let outcome = TROSuccess Null
      let json = encode outcome
      decode json `shouldBe` Just outcome

    it "handles empty string in ToolBreak" $ do
      let outcome = TROBreak ""
      let json = encode outcome
      decode json `shouldBe` Just outcome

    it "handles empty target name in TROTransition" $ do
      let outcome = TROTransition "" (toJSON (42 :: Int))
      let json = encode outcome
      decode json `shouldBe` Just outcome

    it "handles unicode characters in target name" $ do
      let outcome = TROTransition "노드A" (toJSON (42 :: Int))
      let json = encode outcome
      decode json `shouldBe` Just outcome

    it "handles unicode characters in break reason" $ do
      let outcome = TROBreak "错误信息"
      let json = encode outcome
      decode json `shouldBe` Just outcome
