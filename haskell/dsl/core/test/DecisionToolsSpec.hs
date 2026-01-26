{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for ToDecisionTools typeclass.
--
-- Verifies:
-- * Tool generation from sum types
-- * Tool name format (decision::snake_case)
-- * Input schema matches constructor fields
-- * parseToolCall roundtrip
module DecisionToolsSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as V
import Data.Text (Text)
import GHC.Generics (Generic)

import ExoMonad.StructuredOutput ()  -- Bring instances into scope
import ExoMonad.StructuredOutput.DecisionTools


-- ════════════════════════════════════════════════════════════════════════════
-- TEST FIXTURES
-- ════════════════════════════════════════════════════════════════════════════

-- | Simple two-branch sum type for testing.
data ReviewDecision
  = Approve { approveNotes :: Text }
  | Reject { rejectReason :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToDecisionTools)

-- | Three-branch sum type with multiple fields.
data TaskAction
  = Complete { completeNotes :: Text, completeScore :: Int }
  | Defer { deferReason :: Text, deferDays :: Int }
  | Cancel { cancelReason :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToDecisionTools)

-- | Sum type with PascalCase constructor names.
data HttpResponse
  = OkResponse { okBody :: Text }
  | NotFoundError { notFoundPath :: Text }
  | InternalServerError { iseMessage :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToDecisionTools)


-- ════════════════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  describe "toDecisionTools" $ do
    it "generates one tool per constructor" $ do
      let tools = toDecisionTools @ReviewDecision
      length tools `shouldBe` 2

    it "generates correct tool names (snake_case)" $ do
      let tools = toDecisionTools @ReviewDecision
      let names = map (.dtName) tools
      names `shouldContain` ["decision::approve"]
      names `shouldContain` ["decision::reject"]

    it "handles PascalCase constructor names" $ do
      let tools = toDecisionTools @HttpResponse
      let names = map (.dtName) tools
      names `shouldContain` ["decision::ok_response"]
      names `shouldContain` ["decision::not_found_error"]
      names `shouldContain` ["decision::internal_server_error"]

    it "generates input schemas with correct properties" $ do
      let tools = toDecisionTools @TaskAction
      let completeTool = head $ filter (\t -> t.dtName == "decision::complete") tools
      case completeTool.dtInputSchema of
        Object obj -> do
          KeyMap.member "properties" obj `shouldBe` True
          case KeyMap.lookup "properties" obj of
            Just (Object props) -> do
              -- Field prefix "complete" should be stripped
              KeyMap.member "notes" props `shouldBe` True
              KeyMap.member "score" props `shouldBe` True
            _ -> expectationFailure "Expected properties object"
        _ -> expectationFailure "Expected object schema"

    it "sets required fields correctly" $ do
      let tools = toDecisionTools @ReviewDecision
      let approveTool = head $ filter (\t -> t.dtName == "decision::approve") tools
      case approveTool.dtInputSchema of
        Object obj -> do
          case KeyMap.lookup "required" obj of
            Just (Array arr) -> do
              V.toList arr `shouldContain` [String "notes"]
            _ -> expectationFailure "Expected required array"
        _ -> expectationFailure "Expected object schema"

  describe "parseToolCall" $ do
    it "parses matching tool call correctly" $ do
      let toolCall = ToolCall
            { tcName = "decision::approve"
            , tcInput = object ["notes" .= ("LGTM" :: Text)]
            }
      parseToolCall @ReviewDecision toolCall `shouldBe` Right (Approve "LGTM")

    it "parses second variant correctly" $ do
      let toolCall = ToolCall
            { tcName = "decision::reject"
            , tcInput = object ["reason" .= ("Needs work" :: Text)]
            }
      parseToolCall @ReviewDecision toolCall `shouldBe` Right (Reject "Needs work")

    it "parses multi-field constructor" $ do
      let toolCall = ToolCall
            { tcName = "decision::complete"
            , tcInput = object ["notes" .= ("Done!" :: Text), "score" .= (5 :: Int)]
            }
      parseToolCall @TaskAction toolCall `shouldBe` Right (Complete "Done!" 5)

    it "returns error for unknown tool name" $ do
      let toolCall = ToolCall
            { tcName = "decision::unknown"
            , tcInput = object []
            }
      case parseToolCall @ReviewDecision toolCall of
        Left err -> err `shouldContain` "mismatch"
        Right _ -> expectationFailure "Should have failed"

    it "returns error for missing required field" $ do
      let toolCall = ToolCall
            { tcName = "decision::approve"
            , tcInput = object []  -- Missing "notes"
            }
      case parseToolCall @ReviewDecision toolCall of
        Left _ -> pure ()  -- Expected to fail
        Right _ -> expectationFailure "Should have failed"

  describe "roundtrip" $ do
    it "toDecisionTools + parseToolCall roundtrips" $ do
      let original = Approve "Test notes"
      let tools = toDecisionTools @ReviewDecision
      let approveTool = head $ filter (\t -> t.dtName == "decision::approve") tools
      -- Manually construct a tool call that matches
      let toolCall = ToolCall
            { tcName = approveTool.dtName
            , tcInput = object ["notes" .= ("Test notes" :: Text)]
            }
      parseToolCall @ReviewDecision toolCall `shouldBe` Right original

  describe "decisionServerName" $ do
    it "is 'decision'" $ do
      decisionServerName `shouldBe` "decision"
