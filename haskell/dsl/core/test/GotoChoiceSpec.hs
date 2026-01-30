{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for GotoChoice and its smart constructors.
--
-- GotoChoice is the return type for Logic node handlers. It wraps OneOf
-- with target metadata, guaranteeing a transition is always selected.
module GotoChoiceSpec (spec) where

import ExoMonad.Graph.Goto
  ( GotoChoice,
    To,
    gotoChoice,
    gotoExit,
    gotoSelf,
  )
import ExoMonad.Graph.Goto.Internal (GotoChoice (..), OneOf (..)) -- For test assertions
import ExoMonad.Graph.Types (Exit, Self)
import Test.Hspec

-- | Simple target list for basic tests.
type BasicTargets = '[To "nodeA" Int, To "nodeB" String, To Exit Bool]

-- | Targets including Self for self-loop tests.
type SelfLoopTargets = '[To Self Int, To "other" String, To Exit Bool]

-- | Multiple named targets.
type MultiNodeTargets = '[To "first" Int, To "second" Int, To "third" String, To Exit ()]

spec :: Spec
spec = do
  -- ════════════════════════════════════════════════════════════════════════════
  -- gotoChoice SMART CONSTRUCTOR
  -- ════════════════════════════════════════════════════════════════════════════
  describe "gotoChoice" $ do
    it "constructs choice for first named target" $ do
      let choice :: GotoChoice BasicTargets = gotoChoice @"nodeA" (42 :: Int)
      case choice of
        GotoChoice (Here n) -> n `shouldBe` (42 :: Int)
        GotoChoice (There _) -> expectationFailure "Expected position 0"

    it "constructs choice for second named target" $ do
      let choice :: GotoChoice BasicTargets = gotoChoice @"nodeB" ("hello" :: String)
      case choice of
        GotoChoice (There (Here s)) -> s `shouldBe` "hello"
        _ -> expectationFailure "Expected position 1"

    it "constructs choice with same payload type different names" $ do
      -- Both "first" and "second" have Int payload
      let first :: GotoChoice MultiNodeTargets = gotoChoice @"first" (100 :: Int)
          second :: GotoChoice MultiNodeTargets = gotoChoice @"second" (200 :: Int)

      case first of
        GotoChoice (Here n) -> n `shouldBe` (100 :: Int)
        _ -> expectationFailure "first should be at position 0"

      case second of
        GotoChoice (There (Here n)) -> n `shouldBe` (200 :: Int)
        _ -> expectationFailure "second should be at position 1"

  -- ════════════════════════════════════════════════════════════════════════════
  -- gotoExit SMART CONSTRUCTOR
  -- ════════════════════════════════════════════════════════════════════════════
  describe "gotoExit" $ do
    it "constructs Exit choice at correct position" $ do
      let choice :: GotoChoice BasicTargets = gotoExit True
      case choice of
        GotoChoice (There (There (Here b))) -> b `shouldBe` True
        _ -> expectationFailure "Exit should be at position 2"

    it "constructs Exit choice when Exit is first" $ do
      let choice :: GotoChoice '[To Exit Int] = gotoExit (42 :: Int)
      case choice of
        GotoChoice (Here n) -> n `shouldBe` (42 :: Int)
        GotoChoice (There _) -> expectationFailure "Unexpected There"

    it "constructs Exit choice with unit payload" $ do
      let choice :: GotoChoice MultiNodeTargets = gotoExit ()
      case choice of
        GotoChoice (There (There (There (Here ())))) -> pure ()
        _ -> expectationFailure "Exit should be at position 3"

  -- ════════════════════════════════════════════════════════════════════════════
  -- gotoSelf SMART CONSTRUCTOR
  -- ════════════════════════════════════════════════════════════════════════════
  describe "gotoSelf" $ do
    it "constructs Self choice at correct position" $ do
      let choice :: GotoChoice SelfLoopTargets = gotoSelf (42 :: Int)
      case choice of
        GotoChoice (Here n) -> n `shouldBe` (42 :: Int)
        GotoChoice (There _) -> expectationFailure "Self should be at position 0"

    it "Self can coexist with other targets" $ do
      let selfChoice :: GotoChoice SelfLoopTargets = gotoSelf (10 :: Int)
          otherChoice :: GotoChoice SelfLoopTargets = gotoChoice @"other" ("hi" :: String)
          exitChoice :: GotoChoice SelfLoopTargets = gotoExit False

      case selfChoice of
        GotoChoice (Here n) -> n `shouldBe` (10 :: Int)
        _ -> expectationFailure "Self wrong position"

      case otherChoice of
        GotoChoice (There (Here s)) -> s `shouldBe` "hi"
        _ -> expectationFailure "other wrong position"

      case exitChoice of
        GotoChoice (There (There (Here b))) -> b `shouldBe` False
        _ -> expectationFailure "Exit wrong position"

  -- ════════════════════════════════════════════════════════════════════════════
  -- PATTERN MATCHING ON GotoChoice
  -- ════════════════════════════════════════════════════════════════════════════
  describe "GotoChoice pattern matching" $ do
    it "can dispatch based on GotoChoice value" $ do
      let dispatch :: GotoChoice BasicTargets -> String
          dispatch (GotoChoice (Here n)) = "nodeA: " ++ show n
          dispatch (GotoChoice (There (Here s))) = "nodeB: " ++ s
          dispatch (GotoChoice (There (There (Here b)))) = "Exit: " ++ show b

      dispatch (gotoChoice @"nodeA" (42 :: Int)) `shouldBe` "nodeA: 42"
      dispatch (gotoChoice @"nodeB" ("test" :: String)) `shouldBe` "nodeB: test"
      dispatch (gotoExit True) `shouldBe` "Exit: True"

    it "GotoChoice wraps OneOf with Payloads" $ do
      -- Pattern match to extract raw OneOf
      let choice :: GotoChoice '[To "a" Int, To Exit String] = gotoChoice @"a" (42 :: Int)
      case choice of
        GotoChoice (Here n) -> n `shouldBe` (42 :: Int)
        GotoChoice (There _) -> expectationFailure "wrong"

  -- ════════════════════════════════════════════════════════════════════════════
  -- EDGE CASES
  -- ════════════════════════════════════════════════════════════════════════════
  describe "Edge cases" $ do
    it "single Exit target" $ do
      let choice :: GotoChoice '[To Exit Int] = gotoExit (999 :: Int)
      case choice of
        GotoChoice (Here n) -> n `shouldBe` (999 :: Int)
        GotoChoice (There _) -> expectationFailure "Unexpected There"

    it "Exit with complex payload" $ do
      let payload :: (Int, String, Bool) = (1, "two", True)
          choice :: GotoChoice '[To Exit (Int, String, Bool)] = gotoExit payload
      case choice of
        GotoChoice (Here (a, b, c)) -> do
          a `shouldBe` (1 :: Int)
          b `shouldBe` "two"
          c `shouldBe` True
        GotoChoice (There _) -> expectationFailure "Unexpected There"
