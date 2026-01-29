{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Aeson (toJSON, fromJSON, Result(..))
import Data.Proxy (Proxy(..))
import Data.Time.Clock (getCurrentTime)
import Test.Tasty
import Test.Tasty.HUnit
import ExoMonad.Control.ExoTools
import ExoMonad.Control.ExoTools.SpawnAgents (SpawnAgentsGraph(..), CleanupAgentsGraph(..))
import ExoMonad.Control.ExoTools.SpawnAgents.Types (CleanupAgentsResult(..), SpawnAgentsResult(..))
import ExoMonad.Control.ExoTools.PrReviewStatus (PrReviewStatusGraph(..))
import ExoMonad.Control.ExoTools.PrReviewStatus.Types (PrReviewStatusResult(..), AuthorFeedback(..), FeedbackSummary(..))
import ExoMonad.Effects.GitHub (ReviewComment(..), ReviewState(..))
import ExoMonad.Graph.MCPReify (reifyMCPTools, MCPToolInfo(..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ExoTools"
  [ testCase "pr_review_status is discoverable" test_pr_discovery
  , testCase "spawn_agents is discoverable" test_discovery_spawn
  , testCase "spawn_agents result serialization" test_serialization_spawn
  , testCase "cleanup_agents is discoverable" test_discovery_cleanup
  , testCase "cleanup_agents result serialization" test_serialization_cleanup
  ]

test_discovery_cleanup :: Assertion
test_discovery_cleanup = do
  let cleanupTools = reifyMCPTools (Proxy @CleanupAgentsGraph)
  case cleanupTools of
    [tool] -> assertEqual "Name should be cleanup_agents" "cleanup_agents" tool.mtdName
    _ -> assertFailure $ "Expected exactly one tool, but found " ++ show (length cleanupTools)

test_serialization_cleanup :: Assertion
test_serialization_cleanup = do
  let res = CleanupAgentsResult
        { cleaned = ["123", "456"]
        , failed = [("789", "Worktree not found")]
        }
  let json = toJSON res
  case fromJSON @CleanupAgentsResult json of
    Success res' -> assertEqual "Should roundtrip" res res'
    Error err -> assertFailure $ "Failed to parse: " ++ err

test_discovery_spawn :: Assertion
test_discovery_spawn = do
  let spawnTools = reifyMCPTools (Proxy @SpawnAgentsGraph)
  case spawnTools of
    [tool] -> assertEqual "Name should be spawn_agents" "spawn_agents" tool.mtdName
    _ -> assertFailure $ "Expected exactly one tool, but found " ++ show (length spawnTools)

test_serialization_spawn :: Assertion
test_serialization_spawn = do
  let res = SpawnAgentsResult
        { worktrees = [("wzi", "/path/to/wt")]
        , tabs = [("wzi", "tab-123")]
        , failed = []
        }
  let json = toJSON res
  case fromJSON @SpawnAgentsResult json of
    Success res' -> assertEqual "Should roundtrip" res res'
    Error err -> assertFailure $ "Failed to parse: " ++ err

test_pr_discovery :: Assertion
test_pr_discovery = do
  let prTools = reifyMCPTools (Proxy @PrReviewStatusGraph)
  case prTools of
    [tool] -> assertEqual "Name should be pr_review_status" "pr_review_status" tool.mtdName
    _ -> assertFailure $ "Expected exactly one tool, but found " ++ show (length prTools)
