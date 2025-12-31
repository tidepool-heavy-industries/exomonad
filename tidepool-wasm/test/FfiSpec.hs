{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for FFI entry points.
--
-- These tests verify the full end-to-end flow:
-- 1. initialize(JSON) parses input and runs until first effect yield
-- 2. step(JSON) resumes with effect result and runs until next yield or completion
-- 3. getGraphInfo returns static graph structure
-- 4. getGraphState reflects current execution progress
--
-- All tests use resetState between runs to ensure isolation.
module FfiSpec (spec) where

import Test.Hspec
import Data.Aeson (decode, Value(..))
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Tidepool.Wasm.Ffi (initialize, step, getGraphInfo, getGraphState, resetState)


spec :: Spec
spec = do
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
    resetState
    result <- initialize "5"
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
    resetState
    result <- initialize "42"
    let Just output = decodeOutput result
    case output of
      Object o -> case KM.lookup "effect" o of
        Just (Object eff) ->
          KM.lookup "eff_message" eff `shouldBe` Just (String "Computing: 42")
        _ -> expectationFailure "Expected effect object"
      _ -> expectationFailure "Expected JSON object"

  it "includes graphState in output" $ do
    resetState
    result <- initialize "10"
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
    resetState
    _ <- initialize "5"
    result <- step "{\"type\": \"success\", \"value\": null}"
    let Just output = decodeOutput result
    case output of
      Object o -> do
        KM.lookup "done" o `shouldBe` Just (Bool True)
        KM.lookup "stepResult" o `shouldBe` Just (Number 6)
        KM.lookup "effect" o `shouldBe` Just Null
      _ -> expectationFailure "Expected JSON object"

  it "yields error when effect fails" $ do
    resetState
    _ <- initialize "5"
    result <- step "{\"type\": \"error\", \"message\": \"test failure\"}"
    let Just output = decodeOutput result
    case output of
      Object o -> do
        KM.lookup "done" o `shouldBe` Just (Bool True)
        case KM.lookup "graphState" o of
          Just (Object gs) -> case KM.lookup "phase" gs of
            Just (Object phase) -> do
              KM.lookup "type" phase `shouldBe` Just (String "failed")
              case KM.lookup "error" phase of
                Just (String err) -> T.unpack err `shouldContain` "Log effect failed"
                _ -> expectationFailure "Expected error message"
            _ -> expectationFailure "Expected phase object"
          _ -> expectationFailure "Expected graphState object"
      _ -> expectationFailure "Expected JSON object"


-- ════════════════════════════════════════════════════════════════════════════
-- Full Cycle
-- ════════════════════════════════════════════════════════════════════════════

fullCycleSpec :: Spec
fullCycleSpec = describe "Full cycle (initialize -> step -> done)" $ do

  it "produces n+1 for input n" $ do
    resetState
    -- Step 1: Initialize with 10
    initResult <- initialize "10"
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
    stepResult <- step "{\"type\": \"success\"}"
    let Just stepOutput = decodeOutput stepResult

    case stepOutput of
      Object o -> do
        KM.lookup "done" o `shouldBe` Just (Bool True)
        KM.lookup "stepResult" o `shouldBe` Just (Number 11)
      _ -> expectationFailure "Expected object"

  it "works with zero" $ do
    resetState
    _ <- initialize "0"
    result <- step "{\"type\": \"success\"}"
    let Just output = decodeOutput result
    case output of
      Object o -> KM.lookup "stepResult" o `shouldBe` Just (Number 1)
      _ -> expectationFailure "Expected object"

  it "works with negative numbers" $ do
    resetState
    _ <- initialize "-5"
    result <- step "{\"type\": \"success\"}"
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
    result <- getGraphInfo
    let Just output = decodeOutput result
    case output of
      Object o -> do
        KM.lookup "name" o `shouldBe` Just (String "TestGraph")
        case KM.lookup "nodes" o of
          Just (Array nodes) -> length nodes `shouldBe` 3
          _ -> expectationFailure "Expected nodes array"
      _ -> expectationFailure "Expected JSON object"

  it "includes edge information" $ do
    result <- getGraphInfo
    let Just output = decodeOutput result
    case output of
      Object o -> case KM.lookup "edges" o of
        Just (Object edges) -> do
          KM.lookup "entry" edges `shouldBe` Just (String "compute")
          KM.lookup "compute" edges `shouldBe` Just (String "exit")
        _ -> expectationFailure "Expected edges object"
      _ -> expectationFailure "Expected JSON object"


-- ════════════════════════════════════════════════════════════════════════════
-- getGraphState
-- ════════════════════════════════════════════════════════════════════════════

getGraphStateSpec :: Spec
getGraphStateSpec = describe "getGraphState" $ do

  it "returns idle when not initialized" $ do
    resetState
    result <- getGraphState
    let Just output = decodeOutput result
    case output of
      Object o -> case KM.lookup "phase" o of
        Just (Object phase) -> KM.lookup "type" phase `shouldBe` Just (String "idle")
        _ -> expectationFailure "Expected phase object"
      _ -> expectationFailure "Expected JSON object"

  it "returns in_node after initialize" $ do
    resetState
    _ <- initialize "5"
    result <- getGraphState
    let Just output = decodeOutput result
    case output of
      Object o -> case KM.lookup "phase" o of
        Just (Object phase) -> do
          KM.lookup "type" phase `shouldBe` Just (String "in_node")
          KM.lookup "nodeName" phase `shouldBe` Just (String "compute")
        _ -> expectationFailure "Expected phase object"
      _ -> expectationFailure "Expected JSON object"

  it "returns idle after step completes (state cleared)" $ do
    resetState
    _ <- initialize "5"
    _ <- step "{\"type\": \"success\"}"
    result <- getGraphState
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
    resetState
    result <- step "{\"type\": \"success\"}"
    let Just output = decodeOutput result
    case output of
      Object o -> do
        KM.lookup "done" o `shouldBe` Just (Bool True)
        case KM.lookup "graphState" o of
          Just (Object gs) -> case KM.lookup "phase" gs of
            Just (Object phase) -> do
              KM.lookup "type" phase `shouldBe` Just (String "failed")
              case KM.lookup "error" phase of
                Just (String err) -> T.unpack err `shouldContain` "not initialized"
                _ -> expectationFailure "Expected error message"
            _ -> expectationFailure "Expected phase"
          _ -> expectationFailure "Expected graphState"
      _ -> expectationFailure "Expected JSON object"

  it "initialize with invalid JSON returns error" $ do
    resetState
    result <- initialize "not valid json"
    let Just output = decodeOutput result
    case output of
      Object o -> do
        KM.lookup "done" o `shouldBe` Just (Bool True)
        case KM.lookup "graphState" o of
          Just (Object gs) -> case KM.lookup "phase" gs of
            Just (Object phase) -> do
              KM.lookup "type" phase `shouldBe` Just (String "failed")
              case KM.lookup "error" phase of
                Just (String err) -> T.unpack err `shouldContain` "JSON parse error"
                _ -> expectationFailure "Expected error message"
            _ -> expectationFailure "Expected phase"
          _ -> expectationFailure "Expected graphState"
      _ -> expectationFailure "Expected JSON object"

  it "step with invalid JSON returns error" $ do
    resetState
    _ <- initialize "5"
    result <- step "not valid json"
    let Just output = decodeOutput result
    case output of
      Object o -> do
        KM.lookup "done" o `shouldBe` Just (Bool True)
        case KM.lookup "graphState" o of
          Just (Object gs) -> case KM.lookup "phase" gs of
            Just (Object phase) ->
              KM.lookup "type" phase `shouldBe` Just (String "failed")
            _ -> expectationFailure "Expected phase"
          _ -> expectationFailure "Expected graphState"
      _ -> expectationFailure "Expected JSON object"

  it "double initialize resets state" $ do
    resetState
    _ <- initialize "5"
    _ <- initialize "10"  -- Reset with new value
    result <- step "{\"type\": \"success\"}"
    let Just output = decodeOutput result
    case output of
      Object o -> KM.lookup "stepResult" o `shouldBe` Just (Number 11)  -- 10+1, not 5+1
      _ -> expectationFailure "Expected object"
