{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TypedToolSpec (spec) where

import Test.Hspec
import Data.Aeson (object, (.=), Value(..))
import Data.Text (Text)
import qualified Data.Text as T

import ExoMonad.Wasm.TypedTool
import ExoMonad.Wasm.WireTypes (WireToolCall(..))


spec :: Spec
spec = do
  describe "AskUserInput" $ do
    describe "parseToolInput" $ do
      it "parses ask_user with question and options" $ do
        let tc = WireToolCall
              { wtcId = "test-id"
              , wtcName = "ask_user"
              , wtcInput = object
                  [ "question" .= ("Which meds?" :: String)
                  , "options" .= (["Morning meds", "HRT Shot"] :: [String])
                  ]
              }
        case parseToolInput @AskUserInput tc of
          Left err -> expectationFailure $ T.unpack err
          Right input -> do
            input.auiQuestion `shouldBe` ("Which meds?" :: Text)
            input.auiOptions `shouldBe` Just (["Morning meds", "HRT Shot"] :: [Text])

      it "parses ask_user with question only (no options)" $ do
        let tc = WireToolCall
              { wtcId = "test-id"
              , wtcName = "ask_user"
              , wtcInput = object
                  [ "question" .= ("What would you like to do?" :: String)
                  ]
              }
        case parseToolInput @AskUserInput tc of
          Left err -> expectationFailure $ T.unpack err
          Right input -> do
            input.auiQuestion `shouldBe` ("What would you like to do?" :: Text)
            input.auiOptions `shouldBe` (Nothing :: Maybe [Text])

      it "fails for missing question field" $ do
        let tc = WireToolCall
              { wtcId = "test-id"
              , wtcName = "ask_user"
              , wtcInput = object
                  [ "options" .= (["A", "B"] :: [String])
                  ]
              }
        case parseToolInput @AskUserInput tc of
          Left err -> err `shouldSatisfy` T.isInfixOf "question"
          Right _ -> expectationFailure "Should have failed"

  describe "requireSingleTool" $ do
    it "returns tool when exactly one matches" $ do
      let tc = WireToolCall "id1" "ask_user" Null
      requireSingleTool "ask_user" [tc] `shouldBe` Right tc

    it "fails when tool name doesn't match" $ do
      let tc = WireToolCall "id1" "other_tool" Null
      case requireSingleTool "ask_user" [tc] of
        Left err -> do
          err `shouldSatisfy` T.isInfixOf "ask_user"
          err `shouldSatisfy` T.isInfixOf "other_tool"
        Right _ -> expectationFailure "Should have failed"

    it "fails when no tools provided" $ do
      requireSingleTool "ask_user" [] `shouldBe` Left "No tool calls received"

    it "fails when multiple tools provided" $ do
      let tc1 = WireToolCall "id1" "ask_user" Null
          tc2 = WireToolCall "id2" "ask_user" Null
      case requireSingleTool "ask_user" [tc1, tc2] of
        Left err -> err `shouldSatisfy` T.isInfixOf "2"
        Right _ -> expectationFailure "Should have failed"
