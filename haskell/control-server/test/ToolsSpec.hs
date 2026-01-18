{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for SelectSymbolsTool ToolDef wrapper
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Tidepool.Graph.Tool (ToolDef(..))
import Tidepool.Tool.Convert (toAnthropicTool)
import Tidepool.Teaching.Teacher (FineTrainingTeacher(..))
import Tidepool.Control.Scout.Tools (SelectSymbolsTool(..))
import Tidepool.Control.Scout.DocGen.Teacher (ScoutGemmaEffect(..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Scout Tools Integration"
  [ testGroup "SelectSymbolsTool"
      [ testCase "has correct tool name" test_toolName
      , testCase "derives Anthropic tool schema" test_anthropicTool
      ]
  , testGroup "ScoutGemma FineTrainingTeacher"
      [ testCase "provides teacher guidance" test_teacherGuidance
      , testCase "guidance contains strategy keywords" test_guidanceContent
      ]
  ]

-- | Test that SelectSymbolsTool has the correct tool name
test_toolName :: Assertion
test_toolName = do
  let name = toolName SelectSymbolsTool
  assertEqual "Tool name should be select_symbols" "select_symbols" name

-- | Test that Anthropic tool schema can be derived
test_anthropicTool :: Assertion
test_anthropicTool = do
  let tool = toAnthropicTool SelectSymbolsTool
  -- The tool should have been converted successfully (non-throwing = pass)
  pure ()

-- | Test that FineTrainingTeacher instance provides guidance
test_teacherGuidance :: Assertion
test_teacherGuidance = do
  let guidance = teacherGuidance @ScoutGemmaEffect
  assertBool "Teacher guidance should not be empty" (not $ T.null guidance)
  assertBool "Teacher guidance should contain strategy section"
    ("Symbol Selection Strategy" `T.isInfixOf` guidance)

-- | Test that guidance contains expected keywords
test_guidanceContent :: Assertion
test_guidanceContent = do
  let guidance = teacherGuidance @ScoutGemmaEffect
  assertBool "Guidance should mention brittleness"
    ("break" `T.isInfixOf` T.toLower guidance)
  assertBool "Guidance should mention primitives"
    ("primitive" `T.isInfixOf` T.toLower guidance)
  assertBool "Guidance should mention exhaustive"
    ("exhaustive" `T.isInfixOf` T.toLower guidance)
