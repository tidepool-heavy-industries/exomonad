{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for InjectTarget typeclass.
--
-- InjectTarget is like Inject but matches on full (To name payload) markers
-- instead of just payload types. This correctly handles cases where multiple
-- targets have the same payload type but different names.
module InjectTargetSpec (spec) where

import ExoMonad.Graph.Goto (InjectTarget (..), OneOf, Payloads, To)
import ExoMonad.Graph.Goto.Internal (OneOf (..)) -- For test assertions
import ExoMonad.Graph.Types (Exit)
import Test.Hspec

-- | Test targets with different payload types.
type SimpleTargets = '[To "nodeA" Int, To "nodeB" String, To Exit Bool]

-- | Test targets with DUPLICATE payload types (same Int at different names).
-- This is the key case that InjectTarget handles correctly.
type DuplicatePayloadTargets = '[To "alpha" Int, To "beta" Int, To Exit String]

-- | All same payload type.
type AllSamePayload = '[To "a" Int, To "b" Int, To "c" Int]

spec :: Spec
spec = do
  -- ════════════════════════════════════════════════════════════════════════════
  -- SIMPLE TARGETS (different payload types)
  -- ════════════════════════════════════════════════════════════════════════════
  describe "InjectTarget with different payload types" $ do
    it "injects first target at position 0" $ do
      let x :: OneOf (Payloads SimpleTargets) = injectTarget @(To "nodeA" Int) @SimpleTargets 42
      case x of
        Here n -> n `shouldBe` 42
        There _ -> expectationFailure "Expected position 0"

    it "injects second target at position 1" $ do
      let x :: OneOf (Payloads SimpleTargets) = injectTarget @(To "nodeB" String) @SimpleTargets "hello"
      case x of
        There (Here s) -> s `shouldBe` "hello"
        _ -> expectationFailure "Expected position 1"

    it "injects Exit target at position 2" $ do
      let x :: OneOf (Payloads SimpleTargets) = injectTarget @(To Exit Bool) @SimpleTargets True
      case x of
        There (There (Here b)) -> b `shouldBe` True
        _ -> expectationFailure "Expected position 2"

  -- ════════════════════════════════════════════════════════════════════════════
  -- DUPLICATE PAYLOAD TYPES (the critical case)
  -- ════════════════════════════════════════════════════════════════════════════
  describe "InjectTarget with duplicate payload types" $ do
    it "To 'alpha' Int injects at position 0 (not position 1)" $ do
      let x :: OneOf (Payloads DuplicatePayloadTargets) =
            injectTarget @(To "alpha" Int) @DuplicatePayloadTargets 100
      case x of
        Here n -> n `shouldBe` 100
        There (Here _) -> expectationFailure "Wrong position: got 1, expected 0"
        There (There _) -> expectationFailure "Wrong position: got 2, expected 0"

    it "To 'beta' Int injects at position 1 (not position 0)" $ do
      let x :: OneOf (Payloads DuplicatePayloadTargets) =
            injectTarget @(To "beta" Int) @DuplicatePayloadTargets 200
      case x of
        Here _ -> expectationFailure "Wrong position: got 0, expected 1"
        There (Here n) -> n `shouldBe` 200
        There (There _) -> expectationFailure "Wrong position: got 2, expected 1"

    it "To Exit String injects at position 2" $ do
      let x :: OneOf (Payloads DuplicatePayloadTargets) =
            injectTarget @(To Exit String) @DuplicatePayloadTargets "done"
      case x of
        There (There (Here s)) -> s `shouldBe` "done"
        _ -> expectationFailure "Expected position 2"

    it "different names with same payload go to different positions" $ do
      -- This is THE key property: same payload type, different target names
      let alpha = injectTarget @(To "alpha" Int) @DuplicatePayloadTargets 1
          beta = injectTarget @(To "beta" Int) @DuplicatePayloadTargets 2

      -- alpha should be at position 0, beta at position 1
      case alpha of
        Here n -> n `shouldBe` 1
        _ -> expectationFailure "alpha should be at position 0"

      case beta of
        There (Here n) -> n `shouldBe` 2
        _ -> expectationFailure "beta should be at position 1"

  -- ════════════════════════════════════════════════════════════════════════════
  -- ALL SAME PAYLOAD TYPE
  -- ════════════════════════════════════════════════════════════════════════════
  describe "InjectTarget with all same payload type" $ do
    it "distinguishes three Int targets by name" $ do
      let a = injectTarget @(To "a" Int) @AllSamePayload 10
          b = injectTarget @(To "b" Int) @AllSamePayload 20
          c = injectTarget @(To "c" Int) @AllSamePayload 30

      case a of Here n -> n `shouldBe` 10; _ -> expectationFailure "a wrong"
      case b of There (Here n) -> n `shouldBe` 20; _ -> expectationFailure "b wrong"
      case c of There (There (Here n)) -> n `shouldBe` 30; _ -> expectationFailure "c wrong"

  -- ════════════════════════════════════════════════════════════════════════════
  -- PAYLOADS TYPE FAMILY
  -- ════════════════════════════════════════════════════════════════════════════
  describe "Payloads type family" $ do
    it "extracts payload types from To markers" $ do
      -- Payloads '[To "a" Int, To "b" String] = '[Int, String]
      -- We verify this by constructing OneOf values
      let intVal :: OneOf (Payloads '[To "a" Int, To "b" String]) = Here 42
          strVal :: OneOf (Payloads '[To "a" Int, To "b" String]) = There (Here "hi")

      case intVal of Here n -> n `shouldBe` 42; _ -> expectationFailure "wrong"
      case strVal of There (Here s) -> s `shouldBe` "hi"; _ -> expectationFailure "wrong"
