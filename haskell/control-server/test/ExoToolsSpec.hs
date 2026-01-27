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
import ExoMonad.Effects.GitHub (ReviewComment(..), ReviewState(..))
import ExoMonad.Graph.MCPReify (reifyMCPTools, MCPToolInfo(..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ExoTools"
  [ testCase "file_pr is discoverable" test_discovery_fpr
  , testCase "file_pr result serialization" test_serialization_fpr
  , testCase "pr_review_status is discoverable" test_pr_discovery
  , testCase "pr_review_status result serialization" test_pr_serialization
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
        { carCleaned = ["123", "456"]
        , carFailed = [("789", "Worktree not found")]
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
        { sarWorktrees = [("wzi", "/path/to/wt")]
        , sarTabs = [("wzi", "tab-123")]
        , sarFailed = []
        }
  let json = toJSON res
  case fromJSON @SpawnAgentsResult json of
    Success res' -> assertEqual "Should roundtrip" res res'
    Error err -> assertFailure $ "Failed to parse: " ++ err

test_discovery_fpr :: Assertion
test_discovery_fpr = do
  -- This checks that the MCPExport annotation is correctly handled for FilePRGraph
  let fprTools = reifyMCPTools (Proxy @FilePRGraph)
  case fprTools of
    [tool] -> assertEqual "Name should be file_pr" "file_pr" tool.mtdName
    _ -> assertFailure $ "Expected exactly one tool, but found " ++ show (length fprTools)

test_serialization_fpr :: Assertion
test_serialization_fpr = do
  -- Test created success case
  let info = PRInfo 42 "http://pr.url" "OPEN" "[exomonad-xyz] Fix bug"
  let resCreated = FilePRResult (Just info) True Nothing
  let jsonCreated = toJSON resCreated
  case fromJSON @FilePRResult jsonCreated of
    Success res' -> assertEqual "Should roundtrip created" resCreated res'
    Error err -> assertFailure $ "Failed to parse created: " ++ err

  -- Test existing PR case (idempotent)
  let resExisting = FilePRResult (Just info) False Nothing
  let jsonExisting = toJSON resExisting
  case fromJSON @FilePRResult jsonExisting of
    Success res' -> assertEqual "Should roundtrip existing" resExisting res'
    Error err -> assertFailure $ "Failed to parse existing: " ++ err

  -- Test error case
  let resError = FilePRResult Nothing False (Just "error message")
  let jsonError = toJSON resError
  case fromJSON @FilePRResult jsonError of
    Success res' -> assertEqual "Should roundtrip error" resError res'
    Error err -> assertFailure $ "Failed to parse error: " ++ err

test_pr_discovery :: Assertion
test_pr_discovery = do
  let prTools = reifyMCPTools (Proxy @PrReviewStatusGraph)
  case prTools of
    [tool] -> assertEqual "Name should be pr_review_status" "pr_review_status" tool.mtdName
    _ -> assertFailure $ "Expected exactly one tool, but found " ++ show (length prTools)

test_pr_serialization :: Assertion
test_pr_serialization = do
  now <- getCurrentTime
  let copilotComment = ReviewComment "Copilot" "Looks good" (Just "Main.hs") (Just 10) ReviewCommented now False
  let humanComment = ReviewComment "reviewer" "LGTM" Nothing Nothing ReviewApproved now True
  let copilotFeedback = AuthorFeedback
        { afPending = [copilotComment]
        , afResolved = []
        }
  let humanFeedback = AuthorFeedback
        { afPending = []
        , afResolved = [humanComment]
        }
  let summary = FeedbackSummary
        { fsCopilotPending = 1
        , fsCopilotResolved = 0
        , fsHumanPending = 0
        , fsHumanResolved = 1
        }
  let res = PrReviewStatusResult
        { prsrPrNumber = 123
        , prsrCopilot = copilotFeedback
        , prsrHumans = humanFeedback
        , prsrSummary = summary
        }
  let json = toJSON res
  case fromJSON @PrReviewStatusResult json of
    Success res' -> assertEqual "Should roundtrip" res res'
    Error err -> assertFailure $ "Failed to parse: " ++ err
