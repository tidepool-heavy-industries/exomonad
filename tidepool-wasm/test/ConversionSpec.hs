{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Tests for wire type conversion utilities.
module ConversionSpec (spec) where

import Test.Hspec
import Data.Aeson (toJSON, Value(..), object, (.=))
import qualified Data.Text as T

import Tidepool.Anthropic.Types
  ( ContentBlock(..)
  , ImageSource(..)
  , Message(..)
  , Role(..)
  , ToolUse(..)
  , ToolResult(..)
  , ThinkingContent(..)
  , RedactedThinking(..)
  )
import Tidepool.Wasm.WireTypes
  ( WireMessage(..)
  , WireContentBlock(..)
  )
import Tidepool.Effect.Types (TurnResult(..))
import Tidepool.Wasm.Conversion


spec :: Spec
spec = describe "Tidepool.Wasm.Conversion" $ do
  describe "contentBlockToWire" $ do
    it "converts TextBlock" $ do
      contentBlockToWire (TextBlock "hello") `shouldBe` Just (WCBText "hello")

    it "converts ImageBlock with base64" $ do
      let src = Base64Image "image/png" "abc123"
      contentBlockToWire (ImageBlock src) `shouldBe` Just (WCBImage src)

    it "converts ToolUseBlock" $ do
      let tu = ToolUse "tool_1" "search" (toJSON (123 :: Int))
      let expected = WCBToolUse "tool_1" "search" (toJSON (123 :: Int))
      contentBlockToWire (ToolUseBlock tu) `shouldBe` Just expected

    it "converts ToolResultBlock" $ do
      let tr = ToolResult "tool_1" "result text" False
      let expected = WCBToolResult "tool_1" "result text" False
      contentBlockToWire (ToolResultBlock tr) `shouldBe` Just expected

    it "filters out ThinkingBlock" $ do
      let tc = ThinkingContent "thinking..." "sig123"
      contentBlockToWire (ThinkingBlock tc) `shouldBe` Nothing

    it "filters out RedactedThinkingBlock" $ do
      let rt = RedactedThinking "encrypted"
      contentBlockToWire (RedactedThinkingBlock rt) `shouldBe` Nothing

    it "converts JsonBlock to text" $ do
      let result = contentBlockToWire (JsonBlock (toJSON (42 :: Int)))
      result `shouldSatisfy` \case
        Just (WCBText _) -> True
        _ -> False

  describe "contentBlocksToWireMessages" $ do
    it "creates system and user messages" $ do
      let blocks = [TextBlock "hello world"]
      let result = contentBlocksToWireMessages "Be helpful" blocks
      length result `shouldBe` 2
      -- First message is system
      case head result of
        WireMessage role _ -> role `shouldBe` "system"
      -- Second message is user
      case result !! 1 of
        WireMessage role _ -> role `shouldBe` "user"

    it "skips system message when empty" $ do
      let blocks = [TextBlock "hello"]
      let result = contentBlocksToWireMessages "" blocks
      length result `shouldBe` 1
      case head result of
        WireMessage role _ -> role `shouldBe` "user"

    it "filters thinking blocks from output" $ do
      let blocks =
            [ TextBlock "hello"
            , ThinkingBlock (ThinkingContent "thinking" "sig")
            , TextBlock "world"
            ]
      let result = contentBlocksToWireMessages "" blocks
      case head result of
        WireMessage _ content -> length content `shouldBe` 2

  describe "wireContentBlockToNative" $ do
    it "converts WCBText to TextBlock" $ do
      wireContentBlockToNative (WCBText "hello") `shouldBe` TextBlock "hello"

    it "converts WCBImage to ImageBlock" $ do
      let src = Base64Image "image/jpeg" "xyz"
      wireContentBlockToNative (WCBImage src) `shouldBe` ImageBlock src

    it "converts WCBToolUse to ToolUseBlock" $ do
      let wcb = WCBToolUse "tid" "tname" (toJSON ("input" :: String))
      let expected = ToolUseBlock $ ToolUse "tid" "tname" (toJSON ("input" :: String))
      wireContentBlockToNative wcb `shouldBe` expected

    it "converts WCBToolResult to ToolResultBlock" $ do
      let wcb = WCBToolResult "tid" "content" True
      let expected = ToolResultBlock $ ToolResult "tid" "content" True
      wireContentBlockToNative wcb `shouldBe` expected

  describe "messageToWire" $ do
    it "converts User message" $ do
      let msg = Message User [TextBlock "hi"]
      let result = messageToWire msg
      result.wmRole `shouldBe` "user"

    it "converts Assistant message" $ do
      let msg = Message Assistant [TextBlock "hello"]
      let result = messageToWire msg
      result.wmRole `shouldBe` "assistant"

  describe "wireMessageToNative" $ do
    it "converts user wire message" $ do
      let wire = WireMessage "user" [WCBText "hello"]
      let result = wireMessageToNative wire
      result.role `shouldBe` User

    it "converts assistant wire message" $ do
      let wire = WireMessage "assistant" [WCBText "hi"]
      let result = wireMessageToNative wire
      result.role `shouldBe` Assistant

  describe "parseWireTurnResult" $ do
    it "parses valid TurnResult JSON" $ do
      let json = object
            [ "content" .= ("output" :: String)
            , "toolsInvoked" .= ([] :: [Value])
            , "narrative" .= ("story" :: String)
            , "thinking" .= ("thought" :: String)
            ]
      case parseWireTurnResult json of
        Right (TurnResult _ _ narrative thinking) -> do
          narrative `shouldBe` "story"
          thinking `shouldBe` "thought"
        Left err -> expectationFailure $ "Parse failed: " ++ T.unpack err

    it "handles missing optional fields" $ do
      let json = object
            [ "content" .= (123 :: Int)
            ]
      case parseWireTurnResult json of
        Right (TurnResult _ tools narrative thinking) -> do
          narrative `shouldBe` ""
          thinking `shouldBe` ""
          tools `shouldBe` []
        Left err -> expectationFailure $ "Parse failed: " ++ T.unpack err

    it "returns Left for invalid JSON" $ do
      let json = toJSON ("not an object" :: String)
      case parseWireTurnResult json of
        Left _ -> pure ()
        Right _ -> expectationFailure "Should have failed"
