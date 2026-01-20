{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Aeson (toJSON, fromJSON, Result(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Time.Clock (getCurrentTime)
import Test.Tasty
import Test.Tasty.HUnit
import Tidepool.Control.ExoTools
import Tidepool.Effects.GitHub (ReviewComment(..), ReviewState(..))
import Tidepool.Graph.MCPReify (reifyMCPTools, MCPToolInfo(..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ExoTools"
  [ testCase "pre_commit_check is discoverable" test_discovery_pcc
  , testCase "pre_commit_check result serialization" test_serialization_pcc
  , testCase "file_pr is discoverable" test_discovery_fpr
  , testCase "file_pr result serialization" test_serialization_fpr
  , testCase "bead_to_pr is discoverable" test_discovery_btp
  , testCase "bead_to_pr result serialization" test_serialization_btp
  , testCase "pr_to_bead is discoverable" test_discovery_ptb
  , testCase "pr_to_bead result serialization" test_serialization_ptb
  , testCase "pr_review_status is discoverable" test_pr_discovery
  , testCase "pr_review_status result serialization" test_pr_serialization
  ]

test_discovery_pcc :: Assertion
test_discovery_pcc = do
  -- This checks that the MCPExport annotation is correctly handled
  let pccTools = reifyMCPTools (Proxy @PreCommitCheckGraph)
  assertEqual "Should find one tool" 1 (length pccTools)
  let tool = head pccTools
  assertEqual "Name should be pre_commit_check" "pre_commit_check" tool.mtdName

test_serialization_pcc :: Assertion
test_serialization_pcc = do
  let res = PreCommitCheckResult True "all good" ""
  let json = toJSON res
  case fromJSON @PreCommitCheckResult json of
    Success res' -> assertEqual "Should roundtrip" res res'
    Error err -> assertFailure $ "Failed to parse: " ++ err

test_discovery_fpr :: Assertion
test_discovery_fpr = do
  -- This checks that the MCPExport annotation is correctly handled for FilePRGraph
  let fprTools = reifyMCPTools (Proxy @FilePRGraph)
  assertEqual "Should find one tool" 1 (length fprTools)
  let tool = head fprTools
  assertEqual "Name should be file_pr" "file_pr" tool.mtdName

test_serialization_fpr :: Assertion
test_serialization_fpr = do
  -- Test success case
  let resSuccess = FilePRResult (Just "http://pr.url") Nothing
  let jsonSuccess = toJSON resSuccess
  case fromJSON @FilePRResult jsonSuccess of
    Success res' -> assertEqual "Should roundtrip success" resSuccess res'
    Error err -> assertFailure $ "Failed to parse success: " ++ err

  -- Test error case
  let resError = FilePRResult Nothing (Just "error message")
  let jsonError = toJSON resError
  case fromJSON @FilePRResult jsonError of
    Success res' -> assertEqual "Should roundtrip error" resError res'
    Error err -> assertFailure $ "Failed to parse error: " ++ err

test_discovery_btp :: Assertion
test_discovery_btp = do
  let btpTools = reifyMCPTools (Proxy @BeadToPrGraph)
  assertEqual "Should find one tool" 1 (length btpTools)
  let tool = head btpTools
  assertEqual "Name should be bead_to_pr" "bead_to_pr" tool.mtdName

test_serialization_btp :: Assertion
test_serialization_btp = do
  let info = PRInfo 123 "http://pr.url" "OPEN" "Fix bug"
  let res = BeadToPrResult (Just info)
  let json = toJSON res
  case fromJSON @BeadToPrResult json of
    Success res' -> assertEqual "Should roundtrip" res res'
    Error err -> assertFailure $ "Failed to parse: " ++ err

test_discovery_ptb :: Assertion
test_discovery_ptb = do
  let ptbTools = reifyMCPTools (Proxy @PrToBeadGraph)
  assertEqual "Should find one tool" 1 (length ptbTools)
  let tool = head ptbTools
  assertEqual "Name should be pr_to_bead" "pr_to_bead" tool.mtdName

test_serialization_ptb :: Assertion
test_serialization_ptb = do
  let res = PrToBeadResult (Just "tidepool-123")
  let json = toJSON res
  case fromJSON @PrToBeadResult json of
    Success res' -> assertEqual "Should roundtrip" res res'
    Error err -> assertFailure $ "Failed to parse: " ++ err

test_pr_discovery :: Assertion
test_pr_discovery = do
  let prTools = reifyMCPTools (Proxy @PrReviewStatusGraph)
  assertEqual "Should find one tool" 1 (length prTools)
  let tool = head prTools
  assertEqual "Name should be pr_review_status" "pr_review_status" tool.mtdName

test_pr_serialization :: Assertion
test_pr_serialization = do
  now <- getCurrentTime
  let comment = ReviewComment "Copilot" "Looks good" (Just "Main.hs") (Just 10) ReviewCommented now
  let res = PrReviewStatusResult (Map.fromList [("Copilot", [comment])])
  let json = toJSON res
  -- Verify the structure is flat (Map) not wrapped
  case fromJSON @(Map.Map Text [ReviewComment]) json of
    Success _ -> pure ()
    Error err -> assertFailure $ "JSON should be a direct map: " ++ err
  
  case fromJSON @PrReviewStatusResult json of
    Success res' -> assertEqual "Should roundtrip" res res'
    Error err -> assertFailure $ "Failed to parse: " ++ err
