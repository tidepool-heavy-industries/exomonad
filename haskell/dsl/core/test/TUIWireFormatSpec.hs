{-# LANGUAGE OverloadedStrings #-}
-- | Golden tests for TUI wire format compatibility with Rust.
--
-- These tests verify that Haskell's JSON encoding matches the canonical
-- format expected by Rust's tui-popup binary. If these tests fail,
-- the TUI popup system will break.
--
-- Golden files are in: tests/golden/tui/
module TUIWireFormatSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import ExoMonad.Effect.TUI

-- | Path to golden files (relative to test execution directory)
-- Tests run from haskell/dsl/core/, so we go up 3 levels to repo root
goldenDir :: FilePath
goldenDir = "../../../tests/golden/tui/"

spec :: Spec
spec = do
  describe "TUI Wire Format - Golden Tests" $ do
    describe "Component encoding matches Rust expectations" $ do
      it "Text component encodes correctly" $ do
        let component = Component "msg" (Text "Hello World") Nothing
            encoded = encode component
        -- Must have flat structure with "type" and "content" at top level
        encoded `shouldSatisfy` containsSubstring "\"type\":\"text\""
        encoded `shouldSatisfy` containsSubstring "\"content\":\"Hello World\""
        -- Must NOT have nested "spec" object
        encoded `shouldSatisfy` (not . containsSubstring "\"spec\"")

      it "Slider component encodes correctly" $ do
        let component = Component "volume" (Slider "Volume" 0.0 100.0 50.0) Nothing
            encoded = encode component
        encoded `shouldSatisfy` containsSubstring "\"type\":\"slider\""
        encoded `shouldSatisfy` containsSubstring "\"label\":\"Volume\""
        encoded `shouldSatisfy` containsSubstring "\"min\":0"
        encoded `shouldSatisfy` containsSubstring "\"max\":100"
        encoded `shouldSatisfy` containsSubstring "\"default\":50"

      it "Checkbox component encodes correctly" $ do
        let component = Component "enabled" (Checkbox "Enable feature" False) Nothing
            encoded = encode component
        encoded `shouldSatisfy` containsSubstring "\"type\":\"checkbox\""
        encoded `shouldSatisfy` containsSubstring "\"label\":\"Enable feature\""
        encoded `shouldSatisfy` containsSubstring "\"default\":false"

    describe "PopupDefinition encoding matches Rust expectations" $ do
      it "Full popup encodes correctly" $ do
        let popup = PopupDefinition
              { pdTitle = "Confirm Action"
              , pdComponents =
                  [ Component "action" (Text "Action: Delete files") Nothing
                  , Component "details" (Text "This cannot be undone.") Nothing
                  ]
              }
            encoded = encode popup
        encoded `shouldSatisfy` containsSubstring "\"title\":\"Confirm Action\""
        encoded `shouldSatisfy` containsSubstring "\"components\":"
        -- Verify component structure is flat
        encoded `shouldSatisfy` containsSubstring "\"type\":\"text\""
        encoded `shouldSatisfy` (not . containsSubstring "\"spec\"")

    describe "Component roundtrip with golden files" $ do
      it "Text component roundtrips through golden format" $ do
        golden <- LBS.readFile (goldenDir <> "text_component.json")
        case eitherDecode golden of
          Left err -> expectationFailure $ "Failed to decode golden text_component.json: " <> err
          Right (component :: Component) -> do
            case component of
              Component cid (Text content) _ -> do
                cid `shouldBe` "msg"
                content `shouldBe` "Hello World"
              _ -> expectationFailure "Expected Text component"

      it "Slider component roundtrips through golden format" $ do
        golden <- LBS.readFile (goldenDir <> "slider_component.json")
        case eitherDecode golden of
          Left err -> expectationFailure $ "Failed to decode golden slider_component.json: " <> err
          Right (component :: Component) -> do
            case component of
              Component cid (Slider label minVal maxVal defVal) _ -> do
                cid `shouldBe` "volume"
                label `shouldBe` "Volume"
                minVal `shouldBe` 0.0
                maxVal `shouldBe` 100.0
                defVal `shouldBe` 50.0
              _ -> expectationFailure "Expected Slider component"

      it "Checkbox component roundtrips through golden format" $ do
        golden <- LBS.readFile (goldenDir <> "checkbox_component.json")
        case eitherDecode golden of
          Left err -> expectationFailure $ "Failed to decode golden checkbox_component.json: " <> err
          Right (component :: Component) -> do
            case component of
              Component cid (Checkbox label defVal) _ -> do
                cid `shouldBe` "enabled"
                label `shouldBe` "Enable feature"
                defVal `shouldBe` False
              _ -> expectationFailure "Expected Checkbox component"

      it "PopupDefinition roundtrips through golden format" $ do
        golden <- LBS.readFile (goldenDir <> "popup_definition.json")
        case eitherDecode golden of
          Left err -> expectationFailure $ "Failed to decode golden popup_definition.json: " <> err
          Right (popup :: PopupDefinition) -> do
            popup.pdTitle `shouldBe` "Confirm Action"
            length popup.pdComponents `shouldBe` 2

      it "PopupResult roundtrips through golden format" $ do
        golden <- LBS.readFile (goldenDir <> "popup_result.json")
        case eitherDecode golden of
          Left err -> expectationFailure $ "Failed to decode golden popup_result.json: " <> err
          Right (result :: PopupResult) -> do
            result.prButton `shouldBe` "submit"

-- | Check if a ByteString contains a substring (using naive search)
containsSubstring :: LBS.ByteString -> LBS.ByteString -> Bool
containsSubstring needle haystack =
  let needleLen = LBS.length needle
      haystackLen = LBS.length haystack
  in any (\i -> LBS.take needleLen (LBS.drop i haystack) == needle) [0..haystackLen - needleLen]
