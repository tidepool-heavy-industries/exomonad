{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end tests for multi-yield WASM execution.
--
-- These tests verify the full yield/resume cycle when a graph yields
-- multiple effects before completing. The existing FfiSpec tests single-yield;
-- this module tests multi-yield scenarios.
--
-- Test architecture:
-- - Multi-yield tests use Runner directly (pure, no global state)
-- - JSON boundary verification uses the same wire types as FFI
--
-- Note: We test the Runner layer rather than FFI because:
-- 1. FFI is hardcoded to computeHandlerWasm (single yield)
-- 2. Runner tests prove the continuation chain works
-- 3. FFI is just JSON encoding on top of Runner
module E2ESpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, Value(..))
import Data.Aeson.KeyMap qualified as KM

import ExoMonad.Wasm.Runner (initializeWasm, WasmResult(..))
import ExoMonad.Wasm.TestGraph (computeMultiEffectWasm)
import ExoMonad.Wasm.WireTypes
  ( SerializableEffect(..)
  , EffectResult(..)
  , StepOutput(..)
  , GraphState(..)
  , ExecutionPhase(..)
  )
import ExoMonad.Graph.Goto (GotoChoice, OneOf, To)
import ExoMonad.Graph.Goto.Internal (GotoChoice(..), OneOf(..))  -- For test assertions
import ExoMonad.Graph.Types (Exit)


spec :: Spec
spec = do
  describe "E2E Multi-Yield" $ do
    multiYieldRunnerSpec
    multiYieldEffectSequenceSpec
    multiYieldJsonBoundarySpec
    multiYieldEdgeCasesSpec


-- ════════════════════════════════════════════════════════════════════════════
-- Multi-Yield Runner Tests
-- ════════════════════════════════════════════════════════════════════════════

multiYieldRunnerSpec :: Spec
multiYieldRunnerSpec = describe "Multi-yield through Runner" $ do

  it "yields 3 effects before completing" $ do
    -- Initialize with multi-effect handler
    let result1 = initializeWasm (computeMultiEffectWasm 5)

    -- First yield: "Step 1: received 5"
    case result1 of
      WasmYield (EffLogInfo msg1 _) resume1 -> do
        msg1 `shouldBe` "Step 1: received 5"

        -- Second yield: "Step 2: computing"
        case resume1 (ResSuccess Nothing) of
          WasmYield (EffLogInfo msg2 _) resume2 -> do
            msg2 `shouldBe` "Step 2: computing"

            -- Third yield: "Step 3: returning 6"
            case resume2 (ResSuccess Nothing) of
              WasmYield (EffLogInfo msg3 _) resume3 -> do
                msg3 `shouldBe` "Step 3: returning 6"

                -- Completion with result
                case resume3 (ResSuccess Nothing) of
                  WasmComplete (GotoChoice (Here n)) ->
                    n `shouldBe` 6
                  _ -> expectationFailure "Expected WasmComplete after third effect"

              _ -> expectationFailure "Expected third WasmYield"
          _ -> expectationFailure "Expected second WasmYield"
      _ -> expectationFailure "Expected first WasmYield"

  it "correctly computes n+1 for various inputs" $ do
    -- Test helper that runs full multi-yield cycle
    let runMultiYield :: Int -> Maybe Int
        runMultiYield n = runFull (initializeWasm (computeMultiEffectWasm n))

        runFull :: WasmResult (GotoChoice '[To Exit Int]) -> Maybe Int
        runFull (WasmYield _ k) = runFull (k (ResSuccess Nothing))
        runFull (WasmComplete (GotoChoice (Here result))) = Just result
        runFull (WasmComplete (GotoChoice (There _))) = Nothing  -- impossible, but pattern match
        runFull (WasmError _) = Nothing

    runMultiYield 0 `shouldBe` Just 1
    runMultiYield 10 `shouldBe` Just 11
    runMultiYield (-5) `shouldBe` Just (-4)
    runMultiYield 1000000 `shouldBe` Just 1000001


-- ════════════════════════════════════════════════════════════════════════════
-- Effect Sequence Verification
-- ════════════════════════════════════════════════════════════════════════════

multiYieldEffectSequenceSpec :: Spec
multiYieldEffectSequenceSpec = describe "Effect sequence verification" $ do

  it "effects are yielded in correct order" $ do
    let collectEffects :: Int -> WasmResult a -> [SerializableEffect]
        collectEffects maxSteps result
          | maxSteps <= 0 = []
          | otherwise = case result of
              WasmYield eff k -> eff : collectEffects (maxSteps - 1) (k (ResSuccess Nothing))
              _ -> []

        effects = collectEffects 10 (initializeWasm (computeMultiEffectWasm 42))

    length effects `shouldBe` 3

    -- Verify each effect message
    case effects of
      [EffLogInfo m1 _, EffLogInfo m2 _, EffLogInfo m3 _] -> do
        m1 `shouldBe` "Step 1: received 42"
        m2 `shouldBe` "Step 2: computing"
        m3 `shouldBe` "Step 3: returning 43"
      _ -> expectationFailure $ "Unexpected effects: " ++ show effects

  it "all effects are LogInfo type" $ do
    let isLogInfo (EffLogInfo _ _) = True
        isLogInfo _ = False

        collectEffects :: WasmResult a -> [SerializableEffect]
        collectEffects (WasmYield eff k) = eff : collectEffects (k (ResSuccess Nothing))
        collectEffects _ = []

        effects = collectEffects (initializeWasm (computeMultiEffectWasm 7))

    all isLogInfo effects `shouldBe` True


-- ════════════════════════════════════════════════════════════════════════════
-- JSON Boundary Tests
-- ════════════════════════════════════════════════════════════════════════════

multiYieldJsonBoundarySpec :: Spec
multiYieldJsonBoundarySpec = describe "JSON boundary (wire types)" $ do

  it "StepOutput encodes multi-yield correctly" $ do
    -- Simulate what FFI would produce for each step
    let state = GraphState (PhaseInNode "compute") []
        step1Output = StepYield (EffLogInfo "Step 1: received 5" Nothing) state

    case decode (encode step1Output) :: Maybe Value of
      Nothing -> expectationFailure "Failed to decode StepOutput JSON"
      Just json -> case json of
        Object obj -> do
          KM.lookup "done" obj `shouldBe` Just (Bool False)
          case KM.lookup "effect" obj of
            Just (Object eff) -> do
              KM.lookup "type" eff `shouldBe` Just (String "LogInfo")
              KM.lookup "eff_message" eff `shouldBe` Just (String "Step 1: received 5")
            _ -> expectationFailure "Expected effect object"
        _ -> expectationFailure "Expected JSON object"

  it "StepOutput encodes completion correctly after multi-yield" $ do
    let state = GraphState (PhaseCompleted (Number 6)) ["compute"]
        doneOutput = StepDone (Number 6) state

    case decode (encode doneOutput) :: Maybe Value of
      Nothing -> expectationFailure "Failed to decode StepOutput JSON"
      Just json -> case json of
        Object obj -> do
          KM.lookup "done" obj `shouldBe` Just (Bool True)
          KM.lookup "stepResult" obj `shouldBe` Just (Number 6)
          KM.lookup "effect" obj `shouldBe` Just Null
        _ -> expectationFailure "Expected JSON object"

  it "EffectResult round-trips for multi-step acknowledgments" $ do
    -- Each step between yields receives a ResSuccess Nothing
    let ack = ResSuccess Nothing
    decode (encode ack) `shouldBe` Just ack

    -- Error mid-sequence would look like this (but Log ignores it)
    let err = ResError "mid-sequence failure"
    decode (encode err) `shouldBe` Just err


-- ════════════════════════════════════════════════════════════════════════════
-- Edge Cases
-- ════════════════════════════════════════════════════════════════════════════

multiYieldEdgeCasesSpec :: Spec
multiYieldEdgeCasesSpec = describe "Edge cases" $ do

  it "Log effects ignore errors and continue (fire-and-forget)" $ do
    -- Even with ResError on each step, Log effects continue
    let result = initializeWasm (computeMultiEffectWasm 5)

    case result of
      WasmYield _ resume1 ->
        -- Send error instead of success - Log ignores it
        case resume1 (ResError "error 1") of
          WasmYield _ resume2 ->
            case resume2 (ResError "error 2") of
              WasmYield _ resume3 ->
                case resume3 (ResError "error 3") of
                  WasmComplete (GotoChoice (Here n)) ->
                    -- Still completes successfully!
                    n `shouldBe` 6
                  _ -> expectationFailure "Expected completion despite errors"
              _ -> expectationFailure "Expected yield 3"
          _ -> expectationFailure "Expected yield 2"
      _ -> expectationFailure "Expected yield 1"

  it "ResSuccess with value is ignored for Log effects" $ do
    -- Log effects ignore the value field entirely
    let result = initializeWasm (computeMultiEffectWasm 10)

    case result of
      WasmYield _ resume1 ->
        case resume1 (ResSuccess (Just (String "ignored value"))) of
          WasmYield _ resume2 ->
            case resume2 (ResSuccess (Just (Number 999))) of
              WasmYield _ resume3 ->
                case resume3 (ResSuccess (Just (Bool True))) of
                  WasmComplete (GotoChoice (Here n)) ->
                    n `shouldBe` 11
                  _ -> expectationFailure "Expected completion"
              _ -> expectationFailure "Expected yield 3"
          _ -> expectationFailure "Expected yield 2"
      _ -> expectationFailure "Expected yield 1"

  it "state is independent between separate initializations" $ do
    -- Two separate runs don't interfere (pure functions, no global state in Runner)
    let run1 = initializeWasm (computeMultiEffectWasm 100)
        run2 = initializeWasm (computeMultiEffectWasm 200)

    case (run1, run2) of
      (WasmYield (EffLogInfo m1 _) _, WasmYield (EffLogInfo m2 _) _) -> do
        m1 `shouldBe` "Step 1: received 100"
        m2 `shouldBe` "Step 1: received 200"
      _ -> expectationFailure "Expected both to yield"
