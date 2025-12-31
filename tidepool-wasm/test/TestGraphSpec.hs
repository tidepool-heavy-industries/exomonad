{-# LANGUAGE TypeApplications #-}

-- | Tests for TestGraph structure and handler behavior.
--
-- These tests verify that:
-- 1. TestGraph compiles (passes graph validation constraints)
-- 2. The compute handler returns n+1 as expected
-- 3. The DispatchGoto typeclass correctly dispatches to Exit
module TestGraphSpec (spec) where

import Test.Hspec
import Effectful (runPureEff)

import Tidepool.Graph.Goto (GotoChoice(..), OneOf(..))
import Tidepool.Graph.Execute (DispatchGoto(..))
import Tidepool.Wasm.TestGraph (TestGraph(..), testHandlers)


spec :: Spec
spec = do
  computeHandlerSpec
  dispatchGotoSpec


-- ════════════════════════════════════════════════════════════════════════════
-- Compute Handler
-- ════════════════════════════════════════════════════════════════════════════

computeHandlerSpec :: Spec
computeHandlerSpec = describe "compute handler" $ do

  it "returns n+1 for input 0" $ do
    runComputeHandler 0 `shouldBe` 1

  it "returns n+1 for input 5" $ do
    runComputeHandler 5 `shouldBe` 6

  it "returns n+1 for negative input" $ do
    runComputeHandler (-3) `shouldBe` (-2)

  it "returns n+1 for large input" $ do
    runComputeHandler 1000000 `shouldBe` 1000001


-- | Helper to run the compute handler and extract the exit payload.
--
-- TestGraph's compute handler returns GotoChoice '[To Exit Int], which is
-- a newtype over OneOf '[Int]. Pattern matching on Here gives us the Int directly.
runComputeHandler :: Int -> Int
runComputeHandler n =
  let GotoChoice oneOf = runPureEff (testHandlers.compute n)
  in case oneOf of
       Here result -> result
       -- Note: There case is impossible since OneOf '[Int] only has Here,
       -- but we handle it explicitly to avoid incomplete pattern warnings.
       There _     -> error "runComputeHandler: impossible - OneOf '[Int] has no There case"


-- ════════════════════════════════════════════════════════════════════════════
-- DispatchGoto
-- ════════════════════════════════════════════════════════════════════════════

dispatchGotoSpec :: Spec
dispatchGotoSpec = describe "dispatchGoto" $ do

  it "dispatches Exit target and returns payload" $ do
    runDispatchTest 5 `shouldBe` 6

  it "dispatches Exit target for input 0" $ do
    runDispatchTest 0 `shouldBe` 1

  it "dispatches Exit target for negative input" $ do
    runDispatchTest (-10) `shouldBe` (-9)

  it "dispatches Exit target for large input" $ do
    runDispatchTest 999999 `shouldBe` 1000000


-- | Helper to run the full dispatch through TestGraph.
--
-- This tests the DispatchGoto typeclass by:
-- 1. Calling the compute handler to get a GotoChoice
-- 2. Using dispatchGoto to dispatch on the choice
-- 3. Since compute returns GotoChoice '[To Exit Int], dispatch returns the Int
runDispatchTest :: Int -> Int
runDispatchTest n = runPureEff $ do
  choice <- testHandlers.compute n
  dispatchGoto testHandlers choice
