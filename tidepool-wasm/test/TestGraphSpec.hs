{-# LANGUAGE TypeApplications #-}

-- | Tests for TestGraph structure and handler behavior.
--
-- These tests verify that:
-- 1. TestGraph compiles (passes graph validation constraints)
-- 2. The compute handler returns n+1 as expected
module TestGraphSpec (spec) where

import Test.Hspec
import Data.Dynamic (fromDynamic)
import Effectful (runPureEff)

import Tidepool.Graph.Goto (GotoResult(..), gotoChoiceToResult)
import Tidepool.Wasm.TestGraph (TestGraph(..), testHandlers)


spec :: Spec
spec = do
  computeHandlerSpec


-- ════════════════════════════════════════════════════════════════════════════
-- Compute Handler
-- ════════════════════════════════════════════════════════════════════════════

computeHandlerSpec :: Spec
computeHandlerSpec = describe "compute handler" $ do

  it "returns GotoExit with n+1 for input 0" $ do
    let result = runComputeHandler 0
    extractExitPayload result `shouldBe` Just 1

  it "returns GotoExit with n+1 for input 5" $ do
    let result = runComputeHandler 5
    extractExitPayload result `shouldBe` Just 6

  it "returns GotoExit with n+1 for negative input" $ do
    let result = runComputeHandler (-3)
    extractExitPayload result `shouldBe` Just (-2)

  it "returns GotoExit with n+1 for large input" $ do
    let result = runComputeHandler 1000000
    extractExitPayload result `shouldBe` Just 1000001

  it "produces a GotoExit result (not GotoNode)" $ do
    let result = runComputeHandler 0
    case result of
      GotoExit _ -> pure ()
      GotoNode _ _ -> expectationFailure "Expected GotoExit, got GotoNode"
      GotoSelf _ -> expectationFailure "Expected GotoExit, got GotoSelf"


-- | Helper to run the compute handler and get the GotoResult.
runComputeHandler :: Int -> GotoResult
runComputeHandler n =
  let choice = runPureEff (testHandlers.compute n)
  in gotoChoiceToResult choice


-- | Extract the Int payload from a GotoExit result.
extractExitPayload :: GotoResult -> Maybe Int
extractExitPayload (GotoExit dyn) = fromDynamic dyn
extractExitPayload _ = Nothing
