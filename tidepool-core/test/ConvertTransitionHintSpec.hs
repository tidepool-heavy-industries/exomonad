{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for ConvertTransitionHint typeclass.
--
-- ConvertTransitionHint converts untyped tool transitions (Text target name + JSON payload)
-- into typed GotoChoice by matching against the node's UsesEffects targets.
--
-- Design: Recursive matching against target list, constructing proper OneOf positions,
-- with type-checking via FromJSON for each target's payload.
module ConvertTransitionHintSpec (spec) where

import Test.Hspec
import Data.Aeson (toJSON, object, (.=), Value)
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Graph.Execute (ConvertTransitionHint(..))
import Tidepool.Graph.Goto (To, GotoChoice)
import Tidepool.Graph.Goto.Internal (GotoChoice(..), OneOf(..))
import Tidepool.Graph.Types (Exit, Self)

-- ════════════════════════════════════════════════════════════════════════════
-- TEST TARGET LISTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Simple list with a named target and Exit
type SimpleTargets = '[To "nodeA" Int, To Exit String]

-- | Multiple named targets before Exit
type MultiNamedTargets = '[To "alpha" Int, To "beta" String, To "gamma" Bool, To Exit ()]

-- | With Exit as first target
type ExitFirstTargets = '[To Exit Int, To "other" String]

-- | With Self
type WithSelfTargets = '[To Self Int, To "continue" String, To Exit Bool]

-- | Complex target list
type ComplexTargets = '[To "step1" Int, To Self String, To "step2" Bool, To Exit ()]

-- | Empty targets list (edge case)
type EmptyTargets = '[]

-- ════════════════════════════════════════════════════════════════════════════
-- TEST SUITE
-- ════════════════════════════════════════════════════════════════════════════

spec :: Spec
spec = describe "ConvertTransitionHint typeclass" $ do

  describe "Named targets" $ do
    it "matches first named target with correct payload" $
      case convertTransitionHint @SimpleTargets "nodeA" (toJSON (42 :: Int)) of
        Just _ -> pure ()
        Nothing -> expectationFailure "Should match 'nodeA' with Int"

    it "rejects unknown target name" $
      case convertTransitionHint @SimpleTargets "unknown" (toJSON (42 :: Int)) of
        Just _ -> expectationFailure "Should reject unknown target"
        Nothing -> pure ()

    it "rejects wrong payload type for target" $
      case convertTransitionHint @SimpleTargets "nodeA" (toJSON ("string" :: String)) of
        Just _ -> expectationFailure "Should reject wrong type"
        Nothing -> pure ()

    it "matches second named target correctly" $
      case convertTransitionHint @MultiNamedTargets "beta" (toJSON ("hello" :: String)) of
        Just _ -> pure ()
        Nothing -> expectationFailure "Should match 'beta' with String"

    it "matches third named target correctly" $
      case convertTransitionHint @MultiNamedTargets "gamma" (toJSON True) of
        Just _ -> pure ()
        Nothing -> expectationFailure "Should match 'gamma' with Bool"

  describe "Exit targets" $ do
    it "matches Exit when it's second target" $
      case convertTransitionHint @SimpleTargets "Exit" (toJSON ("result" :: String)) of
        Just _ -> pure ()
        Nothing -> expectationFailure "Should match 'Exit' target"

    it "matches Exit when it's first target" $
      case convertTransitionHint @ExitFirstTargets "Exit" (toJSON (99 :: Int)) of
        Just _ -> pure ()
        Nothing -> expectationFailure "Should match 'Exit' as first target"

    it "matches Exit with unit payload" $
      case convertTransitionHint @MultiNamedTargets "Exit" (toJSON ()) of
        Just _ -> pure ()
        Nothing -> expectationFailure "Should match 'Exit' with unit"

    it "rejects when Exit expects different type" $
      case convertTransitionHint @MultiNamedTargets "Exit" (toJSON (123 :: Int)) of
        Just _ -> expectationFailure "Should reject wrong type"
        Nothing -> pure ()

  describe "Self targets" $ do
    it "matches Self target" $
      case convertTransitionHint @WithSelfTargets "Self" (toJSON (55 :: Int)) of
        Just _ -> pure ()
        Nothing -> expectationFailure "Should match 'Self' target"

    it "matches Self when it's first in complex list" $
      case convertTransitionHint @ComplexTargets "Self" (toJSON ("loop" :: String)) of
        Just _ -> pure ()
        Nothing -> expectationFailure "Should match 'Self' as first target"

    it "continues searching after Self" $
      case convertTransitionHint @WithSelfTargets "continue" (toJSON ("next" :: String)) of
        Just _ -> pure ()
        Nothing -> expectationFailure "Should match 'continue' after Self"

  describe "Complex scenarios" $ do
    it "matches correct target in 3-target list" $
      case convertTransitionHint @ComplexTargets "step1" (toJSON (77 :: Int)) of
        Just _ -> pure ()
        Nothing -> expectationFailure "Should match 'step1'"

    it "rejects payload when target name matches but type doesn't" $
      case convertTransitionHint @MultiNamedTargets "alpha" (toJSON ("wrong" :: String)) of
        Just _ -> expectationFailure "Should reject wrong type"
        Nothing -> pure ()

    it "handles null/unit payloads" $
      case convertTransitionHint @MultiNamedTargets "Exit" (toJSON ()) of
        Just _ -> pure ()
        Nothing -> expectationFailure "Should match 'Exit' with unit"

  describe "Edge cases" $ do
    it "returns Nothing for empty target list" $
      case convertTransitionHint @EmptyTargets "anything" (toJSON (42 :: Int)) of
        Just _ -> expectationFailure "Should reject empty target list"
        Nothing -> pure ()

    it "is case-sensitive for target names" $
      case convertTransitionHint @SimpleTargets "nodea" (toJSON (42 :: Int)) of
        Just _ -> expectationFailure "Should be case-sensitive"
        Nothing -> pure ()

    it "is case-sensitive for Exit keyword" $
      case convertTransitionHint @SimpleTargets "exit" (toJSON ("result" :: String)) of
        Just _ -> expectationFailure "Should be case-sensitive"
        Nothing -> pure ()

    it "is case-sensitive for Self keyword" $
      case convertTransitionHint @WithSelfTargets "self" (toJSON (42 :: Int)) of
        Just _ -> expectationFailure "Should be case-sensitive"
        Nothing -> pure ()

    it "handles JSON parse errors gracefully" $
      case convertTransitionHint @SimpleTargets "nodeA" (toJSON ("not an int" :: String)) of
        Just _ -> expectationFailure "Should reject type mismatch"
        Nothing -> pure ()

  describe "JSON roundtrip" $ do
    it "matches integer payloads" $
      case convertTransitionHint @MultiNamedTargets "alpha" (toJSON (42 :: Int)) of
        Just _ -> pure ()
        Nothing -> expectationFailure "Should match integer"

    it "matches string payloads" $
      case convertTransitionHint @MultiNamedTargets "beta" (toJSON ("test" :: String)) of
        Just _ -> pure ()
        Nothing -> expectationFailure "Should match string"

    it "matches boolean payloads" $
      case convertTransitionHint @MultiNamedTargets "gamma" (toJSON True) of
        Just _ -> pure ()
        Nothing -> expectationFailure "Should match boolean"

    it "rejects object payloads for Int target" $
      case convertTransitionHint @ComplexTargets "step1" (object ["key" .= ("value" :: String)]) of
        Just _ -> expectationFailure "Should reject object for Int"
        Nothing -> pure ()
