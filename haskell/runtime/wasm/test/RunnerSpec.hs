{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Graph Runner (freer-simple yield/resume pattern).
--
-- These tests verify:
-- 1. initializeWasm yields the expected Log effect
-- 2. WasmResult continuation resumes correctly
-- 3. Full yield/resume cycle produces expected output
module RunnerSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..))
import qualified Data.Text as T

import ExoMonad.Wasm.Runner (initializeWasm, WasmResult(..))
import ExoMonad.Wasm.TestGraph (computeHandlerWasm)
import ExoMonad.Wasm.WireTypes (SerializableEffect(..), EffectResult(..))
import ExoMonad.Graph.Goto (GotoChoice, OneOf)
import ExoMonad.Graph.Goto.Internal (GotoChoice(..), OneOf(..))  -- For test assertions


spec :: Spec
spec = do
  initializeWasmSpec
  resumeCycleSpec
  fullCycleSpec


-- ════════════════════════════════════════════════════════════════════════════
-- initializeWasm
-- ════════════════════════════════════════════════════════════════════════════

initializeWasmSpec :: Spec
initializeWasmSpec = describe "initializeWasm" $ do

  it "yields Log effect with input value in message" $ do
    let result = initializeWasm (computeHandlerWasm 5)
    case result of
      WasmYield (EffLogInfo msg _) _ ->
        msg `shouldBe` "Computing: 5"
      _ -> expectationFailure "Expected WasmYield with EffLogInfo"

  it "yields Log effect for zero" $ do
    let result = initializeWasm (computeHandlerWasm 0)
    case result of
      WasmYield (EffLogInfo msg _) _ ->
        msg `shouldBe` "Computing: 0"
      _ -> expectationFailure "Expected WasmYield with EffLogInfo"

  it "yields Log effect for negative numbers" $ do
    let result = initializeWasm (computeHandlerWasm (-10))
    case result of
      WasmYield (EffLogInfo msg _) _ ->
        msg `shouldBe` "Computing: -10"
      _ -> expectationFailure "Expected WasmYield with EffLogInfo"


-- ════════════════════════════════════════════════════════════════════════════
-- Resume Cycle
-- ════════════════════════════════════════════════════════════════════════════

resumeCycleSpec :: Spec
resumeCycleSpec = describe "resuming with EffectResult" $ do

  it "completes with n+1 after Log success" $ do
    let result = initializeWasm (computeHandlerWasm 5)
    case result of
      WasmYield _ resume ->
        case resume (ResSuccess Nothing) of
          WasmComplete (GotoChoice (Here n)) ->
            n `shouldBe` 6
          _ -> expectationFailure "Expected WasmComplete with result"
      _ -> expectationFailure "Expected initial WasmYield"

  it "completes correctly for zero" $ do
    let result = initializeWasm (computeHandlerWasm 0)
    case result of
      WasmYield _ resume ->
        case resume (ResSuccess Nothing) of
          WasmComplete (GotoChoice (Here n)) ->
            n `shouldBe` 1
          _ -> expectationFailure "Expected WasmComplete with result"
      _ -> expectationFailure "Expected initial WasmYield"

  it "ignores success value (for Log effects)" $ do
    -- Log success value is ignored, only matters that it succeeded
    let result = initializeWasm (computeHandlerWasm 7)
    case result of
      WasmYield _ resume ->
        case resume (ResSuccess (Just (String "ignored"))) of
          WasmComplete (GotoChoice (Here n)) ->
            n `shouldBe` 8
          _ -> expectationFailure "Expected WasmComplete with result"
      _ -> expectationFailure "Expected initial WasmYield"

  -- Note: ResError handling in current implementation ignores errors for Log effects.
  -- The handler doesn't distinguish success/error for effects that don't return values.
  -- This test documents current behavior - Log effects succeed regardless of ResError.
  it "Log effects continue on ResError (current behavior)" $ do
    let result = initializeWasm (computeHandlerWasm 5)
    case result of
      WasmYield _ resume ->
        -- Even with ResError, the computation continues because logInfo ignores the result
        case resume (ResError "simulated failure") of
          WasmComplete (GotoChoice (Here n)) ->
            n `shouldBe` 6
          _ -> expectationFailure "Expected WasmComplete despite error"
      _ -> expectationFailure "Expected initial WasmYield"


-- ════════════════════════════════════════════════════════════════════════════
-- Full Cycle (integration)
-- ════════════════════════════════════════════════════════════════════════════

fullCycleSpec :: Spec
fullCycleSpec = describe "Full yield/resume cycle" $ do

  it "initialize then resume produces n+1" $ do
    -- Simulate full TypeScript ↔ WASM interaction
    let input = 10

    -- Step 1: Initialize - yields Log effect
    case initializeWasm (computeHandlerWasm input) of
      WasmYield (EffLogInfo msg _) resume -> do
        T.unpack msg `shouldContain` "Computing: 10"

        -- Step 2: TypeScript executes log, sends success
        case resume (ResSuccess Nothing) of
          WasmComplete (GotoChoice (Here n)) ->
            n `shouldBe` 11
          _ -> expectationFailure "Expected final WasmComplete"

      _ -> expectationFailure "Expected initial WasmYield"

  it "handles multiple independent runs" $ do
    -- Verify state doesn't leak between runs (pure functions)
    let result1 = initializeWasm (computeHandlerWasm 100)
        result2 = initializeWasm (computeHandlerWasm 200)

    case (result1, result2) of
      (WasmYield (EffLogInfo msg1 _) _, WasmYield (EffLogInfo msg2 _) _) -> do
        msg1 `shouldBe` "Computing: 100"
        msg2 `shouldBe` "Computing: 200"
      _ -> expectationFailure "Expected both to yield"

  it "large values work correctly" $ do
    let result = initializeWasm (computeHandlerWasm 1000000)
    case result of
      WasmYield _ resume ->
        case resume (ResSuccess Nothing) of
          WasmComplete (GotoChoice (Here n)) ->
            n `shouldBe` 1000001
          _ -> expectationFailure "Expected WasmComplete"
      _ -> expectationFailure "Expected WasmYield"
