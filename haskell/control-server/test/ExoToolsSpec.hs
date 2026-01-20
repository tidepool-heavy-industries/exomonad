{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Aeson (toJSON, fromJSON, Result(..))
import Data.Proxy (Proxy(..))
import Test.Tasty
import Test.Tasty.HUnit
import Tidepool.Control.ExoTools
import Tidepool.Graph.MCPReify (reifyMCPTools, MCPToolInfo(..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ExoTools"
  [ testCase "pre_commit_check is discoverable" test_discovery_pcc
  , testCase "pre_commit_check result serialization" test_serialization_pcc
  , testCase "file_pr is discoverable" test_discovery_fpr
  , testCase "file_pr result serialization" test_serialization_fpr
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