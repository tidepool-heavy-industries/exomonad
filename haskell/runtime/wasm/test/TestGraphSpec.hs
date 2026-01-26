{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for TestGraph structure and WasmM handler behavior.
--
-- These tests verify that:
-- 1. TestGraph compiles (passes graph validation constraints)
-- 2. The computeHandlerWasm handler works with WasmM effects
-- 3. The handler correctly uses the Log effect and returns n+1
module TestGraphSpec (spec) where

import Test.Hspec
import qualified Data.Text as T

import ExoMonad.Graph.Goto (GotoChoice, OneOf)
import ExoMonad.Graph.Goto.Internal (GotoChoice(..), OneOf(..))  -- For test assertions
import ExoMonad.Wasm.TestGraph (computeHandlerWasm)
import ExoMonad.Wasm.Runner (initializeWasm, WasmResult(..))
import ExoMonad.Wasm.WireTypes (SerializableEffect(..), EffectResult(..))


spec :: Spec
spec = do
  computeHandlerSpec
  graphStructureSpec


-- ════════════════════════════════════════════════════════════════════════════
-- Compute Handler
-- ════════════════════════════════════════════════════════════════════════════

computeHandlerSpec :: Spec
computeHandlerSpec = describe "computeHandlerWasm" $ do

  it "returns n+1 for input 0" $ do
    runComputeHandler 0 `shouldBe` 1

  it "returns n+1 for input 5" $ do
    runComputeHandler 5 `shouldBe` 6

  it "returns n+1 for negative input" $ do
    runComputeHandler (-3) `shouldBe` (-2)

  it "returns n+1 for large input" $ do
    runComputeHandler 1000000 `shouldBe` 1000001


-- | Helper to run the compute handler through full yield/resume cycle.
--
-- The handler logs a message (yield), we resume with success, and extract the result.
runComputeHandler :: Int -> Int
runComputeHandler n =
  case initializeWasm (computeHandlerWasm n) of
    WasmYield _ resume ->
      case resume (ResSuccess Nothing) of
        WasmComplete (GotoChoice (Here result)) -> result
        WasmError msg -> error $ "runComputeHandler: WasmError after resume: " <> T.unpack msg
        _ -> error "runComputeHandler: expected WasmComplete after resume"
    WasmComplete (GotoChoice (Here result)) ->
      -- Handler completed without yielding (shouldn't happen for current impl)
      error $ "runComputeHandler: handler completed without yielding, got: " <> show result
    WasmError msg ->
      error $ "runComputeHandler: WasmError: " <> T.unpack msg
    _ -> error "runComputeHandler: unexpected result"


-- ════════════════════════════════════════════════════════════════════════════
-- Graph Structure
-- ════════════════════════════════════════════════════════════════════════════

graphStructureSpec :: Spec
graphStructureSpec = describe "TestGraph structure" $ do

  it "handler yields Log effect with correct message format" $ do
    case initializeWasm (computeHandlerWasm 42) of
      WasmYield (EffLogInfo msg _) _ -> do
        T.unpack msg `shouldContain` "Computing:"
        T.unpack msg `shouldContain` "42"
      _ -> expectationFailure "Expected Log effect yield"

  it "handler uses EffLogInfo (not EffLogError)" $ do
    case initializeWasm (computeHandlerWasm 7) of
      WasmYield (EffLogInfo _ _) _ -> pure ()
      WasmYield (EffLogError _ _) _ ->
        expectationFailure "Expected EffLogInfo, got EffLogError"
      _ -> expectationFailure "Expected yield"
