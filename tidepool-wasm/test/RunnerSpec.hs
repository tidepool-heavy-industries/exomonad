{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Graph Runner (explicit continuation encoding).
--
-- These tests verify:
-- 1. initializeGraph yields the expected Log effect
-- 2. stepGraph resumes correctly with success/error results
-- 3. Full yield/resume cycle produces expected output
module RunnerSpec (spec) where

import Test.Hspec
import Data.Aeson (decode, encode, Value(..))
import qualified Data.Text as T

import Tidepool.Wasm.Runner
import Tidepool.Wasm.WireTypes


spec :: Spec
spec = do
  initializeGraphSpec
  stepGraphSpec
  fullCycleSpec
  runnerStateSpec


-- ════════════════════════════════════════════════════════════════════════════
-- initializeGraph
-- ════════════════════════════════════════════════════════════════════════════

initializeGraphSpec :: Spec
initializeGraphSpec = describe "initializeGraph" $ do

  it "yields Log effect with input value in message" $ do
    let yield = initializeGraph 5
    case yield of
      YieldEffect (EffLogInfo msg) _ ->
        msg `shouldBe` "Computing: 5"
      _ -> expectationFailure "Expected YieldEffect with EffLogInfo"

  it "returns continuation with input value" $ do
    let yield = initializeGraph 42
    case yield of
      YieldEffect _ (ContAfterLog n) ->
        n `shouldBe` 42
      _ -> expectationFailure "Expected YieldEffect with ContAfterLog"

  it "works with zero" $ do
    let yield = initializeGraph 0
    case yield of
      YieldEffect (EffLogInfo msg) (ContAfterLog n) -> do
        msg `shouldBe` "Computing: 0"
        n `shouldBe` 0
      _ -> expectationFailure "Expected YieldEffect"

  it "works with negative numbers" $ do
    let yield = initializeGraph (-10)
    case yield of
      YieldEffect (EffLogInfo msg) (ContAfterLog n) -> do
        msg `shouldBe` "Computing: -10"
        n `shouldBe` (-10)
      _ -> expectationFailure "Expected YieldEffect"


-- ════════════════════════════════════════════════════════════════════════════
-- stepGraph
-- ════════════════════════════════════════════════════════════════════════════

stepGraphSpec :: Spec
stepGraphSpec = describe "stepGraph" $ do

  describe "with ResSuccess" $ do
    it "completes with n+1 after Log success" $ do
      let yield = stepGraph (ContAfterLog 5) (ResSuccess Nothing)
      case yield of
        YieldComplete result ->
          decode (encode result) `shouldBe` Just (Number 6)
        _ -> expectationFailure "Expected YieldComplete"

    it "completes correctly for edge cases" $ do
      let yield = stepGraph (ContAfterLog 0) (ResSuccess Nothing)
      case yield of
        YieldComplete result ->
          decode (encode result) `shouldBe` Just (Number 1)
        _ -> expectationFailure "Expected YieldComplete"

    it "ignores success value (for Log effects)" $ do
      -- Log success value is ignored, only matters that it succeeded
      let yield = stepGraph (ContAfterLog 7) (ResSuccess (Just (String "ignored")))
      case yield of
        YieldComplete result ->
          decode (encode result) `shouldBe` Just (Number 8)
        _ -> expectationFailure "Expected YieldComplete"

  describe "with ResError" $ do
    it "yields error on Log failure" $ do
      let yield = stepGraph (ContAfterLog 5) (ResError "network down")
      case yield of
        YieldError msg -> do
          T.unpack msg `shouldContain` "Log effect failed"
          T.unpack msg `shouldContain` "network down"
        _ -> expectationFailure "Expected YieldError"


-- ════════════════════════════════════════════════════════════════════════════
-- Full Cycle (integration)
-- ════════════════════════════════════════════════════════════════════════════

fullCycleSpec :: Spec
fullCycleSpec = describe "Full yield/resume cycle" $ do

  it "initialize then step produces n+1" $ do
    -- Simulate full TypeScript ↔ WASM interaction
    let input = 10

    -- Step 1: Initialize
    let yield1 = initializeGraph input
    cont <- case yield1 of
      YieldEffect (EffLogInfo msg) c -> do
        T.unpack msg `shouldContain` "Computing: 10"
        pure c
      _ -> expectationFailure "Expected initial YieldEffect" >> error "unreachable"

    -- Step 2: TypeScript executes log, sends success
    let yield2 = stepGraph cont (ResSuccess Nothing)
    case yield2 of
      YieldComplete result ->
        decode (encode result) `shouldBe` Just (Number 11)
      _ -> expectationFailure "Expected final YieldComplete"

  it "handles multiple independent runs" $ do
    -- Verify state doesn't leak between runs (pure functions)
    let yield1 = initializeGraph 100
        yield2 = initializeGraph 200

    case (yield1, yield2) of
      (YieldEffect _ (ContAfterLog n1), YieldEffect _ (ContAfterLog n2)) -> do
        n1 `shouldBe` 100
        n2 `shouldBe` 200
      _ -> expectationFailure "Expected both to yield"


-- ════════════════════════════════════════════════════════════════════════════
-- RunnerState (JSON serialization for debugging)
-- ════════════════════════════════════════════════════════════════════════════

runnerStateSpec :: Spec
runnerStateSpec = describe "RunnerState" $ do

  it "round-trips through JSON" $ do
    let state = RunnerState (ContAfterLog 42) (PhaseInNode "compute")
    decode (encode state) `shouldBe` Just state

  it "preserves continuation value" $ do
    let state = RunnerState (ContAfterLog 999) PhaseIdle
    case decode (encode state) of
      Just (RunnerState (ContAfterLog n) _) ->
        n `shouldBe` 999
      _ -> expectationFailure "Expected RunnerState with ContAfterLog"
