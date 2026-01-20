{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Aeson (toJSON, fromJSON, Result(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Tidepool.Control.FeedbackTools
import Tidepool.Graph.MCPReify (reifyMCPTools, MCPToolInfo(..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "FeedbackTools"
  [ testCase "register_feedback is discoverable" test_discovery_rf
  , testCase "register_feedback result serialization" test_serialization_rf
  , testCase "register_feedback args serialization" test_args_serialization
  ]

test_discovery_rf :: Assertion
test_discovery_rf = do
  -- This checks that the MCPExport annotation is correctly handled
  let rfTools = reifyMCPTools (Proxy @RegisterFeedbackGraph)
  assertEqual "Should find one tool" 1 (length rfTools)
  let tool = head rfTools
  assertEqual "Name should be register_feedback" "register_feedback" tool.mtdName

test_serialization_rf :: Assertion
test_serialization_rf = do
  let res = RegisterFeedbackResult True "/path/to/feedback.json" Nothing
  let json = toJSON res
  case fromJSON @RegisterFeedbackResult json of
    Success res' -> assertEqual "Should roundtrip" res res'
    Error err -> assertFailure $ "Failed to parse: " ++ err

test_args_serialization :: Assertion
test_args_serialization = do
  let args = RegisterFeedbackArgs
        { rfaBeadId = "tidepool-123"
        , rfaSuggestions = ["Make it faster"]
        , rfaIdeas = ["New tool"]
        , rfaNits = ["Typo"]
        , rfaTokenCategories = [TokenCategoryEstimate "coding" "high"]
        , rfaOverallExperience = "smooth"
        , rfaNotes = Just "Good job"
        }
  let json = toJSON args
  case fromJSON @RegisterFeedbackArgs json of
    Success args' -> assertEqual "Should roundtrip" args args'
    Error err -> assertFailure $ "Failed to parse: " ++ err
