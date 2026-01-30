{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the unified graph registry.
module RegistrySpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Wasm.Registry
  ( getGraphInfo,
    getGraphState,
    getRegistry,
    initialize,
    resetSession,
    step,
  )
import ExoMonad.Wasm.Registry.Default (setupDefaultRegistry)
import Test.Hspec

spec :: Spec
spec = do
  -- Setup default registry before all tests
  beforeAll_ setupDefaultRegistry $ do
    describe "Unified Registry" $ do
      describe "getRegistry" $ do
        it "contains expected graph IDs" $ do
          registry <- getRegistry
          let ids = Map.keys registry
          ids `shouldContain` ["test"]
          ids `shouldContain` ["example"]

    describe "initialize" $ do
      it "returns error for unknown graph" $ do
        resetSession
        result <- initialize "nonexistent" "{}"
        result `shouldSatisfy` T.isInfixOf "Unknown graph: nonexistent"
        result `shouldSatisfy` T.isInfixOf "Valid graphs:"

      it "returns error for invalid JSON input" $ do
        resetSession
        result <- initialize "test" "not valid json"
        result `shouldSatisfy` T.isInfixOf "JSON parse error"

      it "initializes test graph and yields Log effect" $ do
        resetSession
        result <- initialize "test" "5"
        -- Should yield a Log effect
        result `shouldSatisfy` T.isInfixOf "LogInfo"
        result `shouldSatisfy` T.isInfixOf "Computing: 5"

      it "initializes example graph with GraphInput" $ do
        resetSession
        result <- initialize "example" "{\"type\":\"text\",\"text\":\"hello\"}"
        -- Should yield a Log effect for classification
        result `shouldSatisfy` T.isInfixOf "LogInfo"

    describe "step" $ do
      it "returns error when no session active" $ do
        resetSession
        result <- step "test" "{\"type\": \"success\"}"
        result `shouldSatisfy` T.isInfixOf "No active session"

      it "returns error for graph mismatch" $ do
        resetSession
        -- Initialize test graph
        _ <- initialize "test" "5"
        -- Try to step with wrong graph ID
        result <- step "example" "{\"type\": \"success\"}"
        result `shouldSatisfy` T.isInfixOf "Graph mismatch"
        result `shouldSatisfy` T.isInfixOf "test"
        result `shouldSatisfy` T.isInfixOf "example"

      it "completes test graph after step" $ do
        resetSession
        -- Initialize
        _ <- initialize "test" "5"
        -- Step with success
        result <- step "test" "{\"type\": \"success\"}"
        -- Should be done with result 6
        result `shouldSatisfy` T.isInfixOf "\"done\":true"
        result `shouldSatisfy` T.isInfixOf "6"

    describe "getGraphInfo" $ do
      it "returns info for valid graph" $ do
        result <- getGraphInfo "test"
        -- Graph ID is single source of truth for name
        result `shouldSatisfy` T.isInfixOf "\"name\":\"test\""
        result `shouldSatisfy` T.isInfixOf "compute"

      it "returns error for unknown graph" $ do
        result <- getGraphInfo "nonexistent"
        result `shouldSatisfy` T.isInfixOf "Unknown graph"

    describe "getGraphState" $ do
      it "returns idle state when no session" $ do
        resetSession
        result <- getGraphState "test"
        result `shouldSatisfy` T.isInfixOf "idle"

      it "returns in_node state during execution" $ do
        resetSession
        _ <- initialize "test" "5"
        result <- getGraphState "test"
        result `shouldSatisfy` T.isInfixOf "in_node"

    describe "session lifecycle" $ do
      it "clears session on completion" $ do
        resetSession
        -- Initialize and complete
        _ <- initialize "test" "5"
        _ <- step "test" "{\"type\": \"success\"}"
        -- Should be idle now
        result <- getGraphState "test"
        result `shouldSatisfy` T.isInfixOf "idle"

      it "clears previous session on new initialize" $ do
        resetSession
        -- Start first session
        _ <- initialize "test" "5"
        -- Start new session (should clear previous)
        _ <- initialize "example" "{\"type\":\"text\",\"text\":\"hello\"}"
        -- Stepping old graph should fail
        result <- step "test" "{\"type\": \"success\"}"
        result `shouldSatisfy` T.isInfixOf "Graph mismatch"
