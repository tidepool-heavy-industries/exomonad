{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Tests for wire type conversion utilities.
module ConversionSpec (spec) where

import Test.Hspec
import Data.Aeson (toJSON, Value(..), object, (.=))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T

import ExoMonad.Anthropic.Types
  ( ContentBlock(..)
  , ImageSource(..)
  , Message(..)
  , Role(..)
  , ToolUseId(..)
  , ToolResultId(..)
  )
import ExoMonad.Wasm.WireTypes
  ( WireMessage(..)
  , WireContentBlock(..)
  )
import ExoMonad.Effect.Types (TurnResult(..))
import ExoMonad.Wasm.Conversion


spec :: Spec
spec = describe "ExoMonad.Wasm.Conversion" $ do
  describe "contentBlockToWire" $ do
    it "converts Text block" $ do
      contentBlockToWire Text { text = "hello" } `shouldBe` Just (WCBText "hello")

    it "converts Image block with base64" $ do
      let src = Base64 { mediaType = "image/png", data_ = "abc123" }
      contentBlockToWire Image { source = src } `shouldBe` Just (WCBImage src)

    it "converts ToolUse block" $ do
      let expected = WCBToolUse "tool_1" "search" (toJSON (123 :: Int))
      contentBlockToWire ToolUse { id = ToolUseId "tool_1", name = "search", input = toJSON (123 :: Int) }
        `shouldBe` Just expected

    it "converts ToolResult block" $ do
      let expected = WCBToolResult "tool_1" "result text" False
      contentBlockToWire ToolResult { toolUseId = ToolResultId "tool_1", content = "result text", isError = False }
        `shouldBe` Just expected

    it "filters out Thinking block" $ do
      contentBlockToWire Thinking { thinking = "thinking...", signature = "sig123" }
        `shouldBe` Nothing

    it "filters out RedactedThinking block" $ do
      contentBlockToWire RedactedThinking { data_ = "encrypted" }
        `shouldBe` Nothing

    it "converts Json block to text" $ do
      let result = contentBlockToWire Json { json = toJSON (42 :: Int) }
      result `shouldSatisfy` \case
        Just (WCBText _) -> True
        _ -> False

  describe "contentBlocksToWireMessages" $ do
    it "creates system and user messages" $ do
      let blocks = [Text { text = "hello world" }]
      let result = contentBlocksToWireMessages "Be helpful" blocks
      length result `shouldBe` 2
      -- First message is system
      case head result of
        WireMessage role _ -> role `shouldBe` "system"
      -- Second message is user
      case result !! 1 of
        WireMessage role _ -> role `shouldBe` "user"

    it "skips system message when empty" $ do
      let blocks = [Text { text = "hello" }]
      let result = contentBlocksToWireMessages "" blocks
      length result `shouldBe` 1
      case head result of
        WireMessage role _ -> role `shouldBe` "user"

    it "filters thinking blocks from output" $ do
      let blocks =
            [ Text { text = "hello" }
            , Thinking { thinking = "thinking", signature = "sig" }
            , Text { text = "world" }
            ]
      let result = contentBlocksToWireMessages "" blocks
      case head result of
        WireMessage _ content -> length content `shouldBe` 2

  describe "wireContentBlockToNative" $ do
    it "converts WCBText to Text block" $ do
      wireContentBlockToNative (WCBText "hello") `shouldBe` Text { text = "hello" }

    it "converts WCBImage to Image block" $ do
      let src = Base64 { mediaType = "image/jpeg", data_ = "xyz" }
      wireContentBlockToNative (WCBImage src) `shouldBe` Image { source = src }

    it "converts WCBToolUse to ToolUse block" $ do
      let wcb = WCBToolUse "tid" "tname" (toJSON ("input" :: String))
      let expected = ToolUse { id = ToolUseId "tid", name = "tname", input = toJSON ("input" :: String) }
      wireContentBlockToNative wcb `shouldBe` expected

    it "converts WCBToolResult to ToolResult block" $ do
      let wcb = WCBToolResult "tid" "content" True
      let expected = ToolResult { toolUseId = ToolResultId "tid", content = "content", isError = True }
      wireContentBlockToNative wcb `shouldBe` expected

  describe "messageToWire" $ do
    it "converts User message" $ do
      let msg = Message { role = User, content = Text { text = "hi" } :| [] }
      let result = messageToWire msg
      result.wmRole `shouldBe` "user"

    it "converts Assistant message" $ do
      let msg = Message { role = Assistant, content = Text { text = "hello" } :| [] }
      let result = messageToWire msg
      result.wmRole `shouldBe` "assistant"

  describe "wireMessageToNative" $ do
    it "converts user wire message" $ do
      let wire = WireMessage "user" [WCBText "hello"]
      case wireMessageToNative wire of
        Just result -> result.role `shouldBe` User
        Nothing -> expectationFailure "Expected Just, got Nothing"

    it "converts assistant wire message" $ do
      let wire = WireMessage "assistant" [WCBText "hi"]
      case wireMessageToNative wire of
        Just result -> result.role `shouldBe` Assistant
        Nothing -> expectationFailure "Expected Just, got Nothing"

    it "returns Nothing for empty content" $ do
      let wire = WireMessage "user" []
      wireMessageToNative wire `shouldBe` Nothing

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
