{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad.Freer (runM)
import Data.Aeson (toJSON, fromJSON, Result(..))
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import Test.Tasty
import Test.Tasty.HUnit
import Tidepool.Control.FeedbackTools
import Tidepool.Graph.Goto (unwrapSingleChoice)
import Tidepool.Graph.MCPReify (reifyMCPTools, MCPToolInfo(..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "FeedbackTools"
  [ testCase "register_feedback is discoverable" test_discovery_rf
  , testCase "register_feedback result serialization" test_serialization_rf
  , testCase "register_feedback args serialization" test_args_serialization
  , testCase "integration: write feedback file" test_integration_write
  , testCase "integration: handle write error" test_integration_error
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
        { rfaIssueId = "gh-123"
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

test_integration_write :: Assertion
test_integration_write = do
  let issueId = "test-issue-integration"
  let filePath = ".tidepool/feedback/" <> T.unpack issueId <> ".json"
  
  -- Ensure clean state
  exists <- doesFileExist filePath
  if exists then removeFile filePath else pure ()
  
  let args = RegisterFeedbackArgs
        { rfaIssueId = issueId
        , rfaSuggestions = []
        , rfaIdeas = []
        , rfaNits = []
        , rfaTokenCategories = []
        , rfaOverallExperience = "test"
        , rfaNotes = Nothing
        }
  
  -- Run the logic
  result <- runM $ fmap unwrapSingleChoice (registerFeedbackLogic args)
  
  -- Verify result
  assertBool "Should succeed" (rfrSuccess result)
  assertEqual "Path should match" (T.pack filePath) (rfrPath result)
  assertEqual "Error should be Nothing" Nothing (rfrError result)
  
  -- Verify file exists
  fileExists <- doesFileExist filePath
  assertBool "File should exist" fileExists
  
  -- Cleanup
  removeFile filePath

test_integration_error :: Assertion
test_integration_error = do
  -- To trigger an error, we can try to write to a path where a directory is a file.
  -- We'll use an issue ID that requires a directory structure that conflicts with an existing file.
  -- e.g. .tidepool/feedback/conflict/issue.json, but .tidepool/feedback/conflict is a file.
  
  let conflictDir = ".tidepool/feedback/conflict"
  let issueId = "conflict/issue"
  
  -- Setup: create conflict file
  createDirectoryIfMissing True ".tidepool/feedback"
  writeFile conflictDir "I am a file"
  
  let args = RegisterFeedbackArgs
        { rfaIssueId = issueId
        , rfaSuggestions = []
        , rfaIdeas = []
        , rfaNits = []
        , rfaTokenCategories = []
        , rfaOverallExperience = "test"
        , rfaNotes = Nothing
        }
        
  -- Run logic
  result <- runM $ fmap unwrapSingleChoice (registerFeedbackLogic args)
  
  -- Verify failure
  assertBool "Should fail" (not $ rfrSuccess result)
  assertBool "Should have error" (rfrError result /= Nothing)
  
  -- Cleanup
  removeFile conflictDir