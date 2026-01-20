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
  [ testCase "pre_commit_check is discoverable" test_discovery
  , testCase "pre_commit_check result serialization" test_serialization
  ]

test_discovery :: Assertion
test_discovery = do
  -- This checks that the MCPExport annotation is correctly handled
  let pccTools = reifyMCPTools (Proxy @PreCommitCheckGraph)
  assertEqual "Should find one tool" 1 (length pccTools)
  let tool = head pccTools
  assertEqual "Name should be pre_commit_check" "pre_commit_check" tool.mtdName

test_serialization :: Assertion
test_serialization = do
  let res = PreCommitCheckResult True "all good" ""
  let json = toJSON res
  case fromJSON @PreCommitCheckResult json of
    Success res' -> assertEqual "Should roundtrip" res res'
    Error err -> assertFailure $ "Failed to parse: " ++ err