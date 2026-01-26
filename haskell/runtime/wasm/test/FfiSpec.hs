{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for unified FFI entry points.
--
-- These tests verify the full end-to-end flow using the unified Registry API:
-- 1. initialize(graphId, JSON) parses input and runs until first effect yield
-- 2. step(graphId, JSON) resumes with effect result and runs until next yield or completion
-- 3. getGraphInfo(graphId) returns static graph structure
-- 4. getGraphState(graphId) reflects current execution progress
--
-- All tests use resetSession between runs to ensure isolation.
module FfiSpec (spec) where

import Test.Hspec
import Data.Aeson (decode, Value(..))
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import ExoMonad.Wasm.Registry
  ( initialize
  , step
  , getGraphInfo
  , getGraphState
  , resetSession
  )
import ExoMonad.Wasm.Registry.Default (setupDefaultRegistry)


spec :: Spec
spec = beforeAll_ setupDefaultRegistry $ do
  describe "FFI Integration" $ do
    initializeSpec
    stepSpec
    fullCycleSpec
    getGraphInfoSpec
    getGraphStateSpec
    errorCasesSpec


-- | Helper to decode JSON from Text output
decodeOutput :: T.Text -> Maybe Value
decodeOutput = decode . LBS.fromStrict . encodeUtf8


-- ════════════════════════════════════════════════════════════════════════════
-- initialize
-- ════════════════════════════════════════════════════════════════════════════

initializeSpec :: Spec
initializeSpec = describe "initialize" $ do

  it "returns StepOutput with effect for valid input" $ do
    resetSession
    result <- initialize "test" "5"
    let Just output = decodeOutput result
    case output of
      Object o -> do
        -- Should have effect field with EffLogInfo
        case KM.lookup "effect" o of
          Just (Object eff) -> KM.lookup "type" eff `shouldBe` Just (String "LogInfo")
          _ -> expectationFailure "Expected effect object"
        KM.lookup "done" o `shouldBe` Just (Bool False)
      _ -> expectationFailure "Expected JSON object"

  it "includes message with input value in effect" $ do
    resetSession
    result <- initialize "test" "42"
    let Just output = decodeOutput result
    case output of
      Object o -> case KM.lookup "effect" o of
        Just (Object eff) ->
          KM.lookup "eff_message" eff `shouldBe` Just (String "Computing: 42")
        _ -> expectationFailure "Expected effect object"
      _ -> expectationFailure "Expected JSON object"

  it "includes graphState in output" $ do
    resetSession
    result <- initialize "test" "10"
    let Just output = decodeOutput result
    case output of
      Object o -> case KM.lookup "graphState" o of
        Just (Object gs) -> do
          case KM.lookup "phase" gs of
            Just (Object phase) -> KM.lookup "type" phase `shouldBe` Just (String "in_node")
            _ -> expectationFailure "Expected phase object"
          KM.lookup "completedNodes" gs `shouldBe` Just (Array V.empty)
        _ -> expectationFailure "Expected graphState object"
      _ -> expectationFailure "Expected JSON object"


-- ════════════════════════════════════════════════════════════════════════════
-- step
-- ════════════════════════════════════════════════════════════════════════════

stepSpec :: Spec
stepSpec = describe "step" $ do

  it "completes graph with success result" $ do
    resetSession
    _ <- initialize "test" "5"
    result <- step "test" "{\"type\": \"success\", \"value\": null}"
    let Just output = decodeOutput result
    case output of
      Object o -> do
        KM.lookup "done" o `shouldBe` Just (Bool True)
        KM.lookup "stepResult" o `shouldBe` Just (Number 6)
        KM.lookup "effect" o `shouldBe` Just Null
      _ -> expectationFailure "Expected JSON object"

  -- Note: Log effects are fire-and-forget - they ignore errors and continue.
  -- This test documents that behavior: even with ResError, the computation completes.
  it "Log effects ignore errors and continue (fire-and-forget)" $ do
    resetSession
    _ <- initialize "test" "5"
    result <- step "test" "{\"type\": \"error\", \"message\": \"test failure\"}"
    let Just output = decodeOutput result
    case output of
      Object o -> do
        -- Computation completes successfully despite the error
        KM.lookup "done" o `shouldBe` Just (Bool True)
        -- Result is n+1 = 6
        KM.lookup "stepResult" o `shouldBe` Just (Number 6)
        case KM.lookup "graphState" o of
          Just (Object gs) -> case KM.lookup "phase" gs of
            Just (Object phase) -> do
              -- Phase shows completed, not failed
              KM.lookup "type" phase `shouldBe` Just (String "completed")
            _ -> expectationFailure "Expected phase object"
          _ -> expectationFailure "Expected graphState object"
      _ -> expectationFailure "Expected JSON object"


-- ════════════════════════════════════════════════════════════════════════════
-- Full Cycle
-- ════════════════════════════════════════════════════════════════════════════

fullCycleSpec :: Spec
fullCycleSpec = describe "Full cycle (initialize -> step -> done)" $ do

  it "produces n+1 for input n" $ do
    resetSession
    -- Step 1: Initialize with 10
    initResult <- initialize "test" "10"
    let Just initOutput = decodeOutput initResult

    -- Verify we got the Log effect
    case initOutput of
      Object o -> case KM.lookup "effect" o of
        Just (Object eff) -> do
          KM.lookup "type" eff `shouldBe` Just (String "LogInfo")
          KM.lookup "eff_message" eff `shouldBe` Just (String "Computing: 10")
        _ -> expectationFailure "Expected effect"
      _ -> expectationFailure "Expected object"

    -- Step 2: Send success, expect completion with 11
    stepResult <- step "test" "{\"type\": \"success\"}"
    let Just stepOutput = decodeOutput stepResult

    case stepOutput of
      Object o -> do
        KM.lookup "done" o `shouldBe` Just (Bool True)
        KM.lookup "stepResult" o `shouldBe` Just (Number 11)
      _ -> expectationFailure "Expected object"

  it "works with zero" $ do
    resetSession
    _ <- initialize "test" "0"
    result <- step "test" "{\"type\": \"success\"}"
    let Just output = decodeOutput result
    case output of
      Object o -> KM.lookup "stepResult" o `shouldBe` Just (Number 1)
      _ -> expectationFailure "Expected object"

  it "works with negative numbers" $ do
    resetSession
    _ <- initialize "test" "-5"
    result <- step "test" "{\"type\": \"success\"}"
    let Just output = decodeOutput result
    case output of
      Object o -> KM.lookup "stepResult" o `shouldBe` Just (Number (-4))
      _ -> expectationFailure "Expected object"


-- ════════════════════════════════════════════════════════════════════════════
-- getGraphInfo
-- ════════════════════════════════════════════════════════════════════════════

getGraphInfoSpec :: Spec
getGraphInfoSpec = describe "getGraphInfo" $ do

  it "returns static graph structure" $ do
    result <- getGraphInfo "test"
    let Just output = decodeOutput result
    case output of
      Object o -> do
        -- Graph ID is single source of truth for both id and name
        KM.lookup "name" o `shouldBe` Just (String "test")
        case KM.lookup "nodes" o of
          Just (Array nodes) -> length nodes `shouldBe` 3
          _ -> expectationFailure "Expected nodes array"
      _ -> expectationFailure "Expected JSON object"

  it "includes edge information" $ do
    result <- getGraphInfo "test"
    let Just output = decodeOutput result
    case output of
      Object o -> case KM.lookup "edges" o of
        Just (Array edges) -> do
          length edges `shouldBe` 2
          -- Edges from reification use type names: Entry -> compute -> Exit
          let edgeList = V.toList edges
          case edgeList of
            [Object e1, Object e2] -> do
              KM.lookup "from" e1 `shouldBe` Just (String "Entry")
              KM.lookup "to" e1 `shouldBe` Just (String "compute")
              KM.lookup "from" e2 `shouldBe` Just (String "compute")
              KM.lookup "to" e2 `shouldBe` Just (String "Exit")
            _ -> expectationFailure "Expected 2 edge objects"
        _ -> expectationFailure "Expected edges array"
      _ -> expectationFailure "Expected JSON object"


-- ════════════════════════════════════════════════════════════════════════════
-- getGraphState
-- ════════════════════════════════════════════════════════════════════════════

getGraphStateSpec :: Spec
getGraphStateSpec = describe "getGraphState" $ do

  it "returns idle when not initialized" $ do
    resetSession
    result <- getGraphState "test"
    let Just output = decodeOutput result
    case output of
      Object o -> case KM.lookup "phase" o of
        Just (Object phase) -> KM.lookup "type" phase `shouldBe` Just (String "idle")
        _ -> expectationFailure "Expected phase object"
      _ -> expectationFailure "Expected JSON object"

  it "returns in_node after initialize" $ do
    resetSession
    _ <- initialize "test" "5"
    result <- getGraphState "test"
    let Just output = decodeOutput result
    case output of
      Object o -> case KM.lookup "phase" o of
        Just (Object phase) -> do
          KM.lookup "type" phase `shouldBe` Just (String "in_node")
          -- Generic session state uses "running" as phase name
          KM.lookup "nodeName" phase `shouldBe` Just (String "running")
        _ -> expectationFailure "Expected phase object"
      _ -> expectationFailure "Expected JSON object"

  it "returns idle after step completes (state cleared)" $ do
    resetSession
    _ <- initialize "test" "5"
    _ <- step "test" "{\"type\": \"success\"}"
    result <- getGraphState "test"
    let Just output = decodeOutput result
    case output of
      Object o -> case KM.lookup "phase" o of
        Just (Object phase) ->
          -- State is cleared after completion, so getGraphState returns idle
          KM.lookup "type" phase `shouldBe` Just (String "idle")
        _ -> expectationFailure "Expected phase object"
      _ -> expectationFailure "Expected JSON object"


-- ════════════════════════════════════════════════════════════════════════════
-- Error Cases
-- ════════════════════════════════════════════════════════════════════════════

errorCasesSpec :: Spec
errorCasesSpec = describe "Error cases" $ do

  it "step before initialize returns error" $ do
    resetSession
    result <- step "test" "{\"type\": \"success\"}"
    let Just output = decodeOutput result
    case output of
      Object o -> do
        KM.lookup "done" o `shouldBe` Just (Bool True)
        case KM.lookup "error" o of
          Just (String err) -> T.unpack err `shouldContain` "No active session"
          _ -> expectationFailure "Expected error message"
      _ -> expectationFailure "Expected JSON object"

  it "initialize with invalid JSON returns error" $ do
    resetSession
    result <- initialize "test" "not valid json"
    let Just output = decodeOutput result
    case output of
      Object o -> do
        KM.lookup "done" o `shouldBe` Just (Bool True)
        case KM.lookup "error" o of
          Just (String err) -> T.unpack err `shouldContain` "JSON parse error"
          _ -> expectationFailure "Expected error message"
      _ -> expectationFailure "Expected JSON object"

  it "step with invalid JSON returns error" $ do
    resetSession
    _ <- initialize "test" "5"
    result <- step "test" "not valid json"
    let Just output = decodeOutput result
    case output of
      Object o -> do
        KM.lookup "done" o `shouldBe` Just (Bool True)
        case KM.lookup "error" o of
          Just (String err) -> T.unpack err `shouldContain` "JSON parse error"
          _ -> expectationFailure "Expected error message"
      _ -> expectationFailure "Expected JSON object"

  it "double initialize resets state" $ do
    resetSession
    _ <- initialize "test" "5"
    _ <- initialize "test" "10"  -- Reset with new value
    result <- step "test" "{\"type\": \"success\"}"
    let Just output = decodeOutput result
    case output of
      Object o -> KM.lookup "stepResult" o `shouldBe` Just (Number 11)  -- 10+1, not 5+1
      _ -> expectationFailure "Expected object"

  it "step with wrong graphId returns mismatch error" $ do
    resetSession
    _ <- initialize "test" "5"
    result <- step "example" "{\"type\": \"success\"}"
    let Just output = decodeOutput result
    case output of
      Object o -> do
        KM.lookup "done" o `shouldBe` Just (Bool True)
        case KM.lookup "error" o of
          Just (String err) -> T.unpack err `shouldContain` "Graph mismatch"
          _ -> expectationFailure "Expected error message"
      _ -> expectationFailure "Expected JSON object"
