{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for CallHandler typeclass.
--
-- CallHandler abstracts over how different handler types are invoked:
-- * Logic handlers: plain functions (payload -> Eff es (GotoChoice targets))
-- * LLM handlers: LLMHandler GADT (requires template context, LLM call, etc.)
--
-- These tests focus on Logic handler invocation. LLM handler tests would
-- require mocking the LLM effect.
module CallHandlerSpec (spec) where

import Test.Hspec
import Effectful (Eff, runPureEff)

import Tidepool.Graph.Goto
  ( OneOf(..)
  , To
  , GotoChoice(..)
  , gotoChoice
  , gotoExit
  )
import Tidepool.Graph.Execute (CallHandler(..))
import Tidepool.Graph.Types (Exit)

-- | Simple target list.
type SimpleTargets = '[To "next" String, To Exit Int]

-- | Branching targets.
type BranchTargets = '[To "pathA" Int, To "pathB" Int, To Exit String]

spec :: Spec
spec = do
  -- ════════════════════════════════════════════════════════════════════════════
  -- LOGIC HANDLER INVOCATION
  -- ════════════════════════════════════════════════════════════════════════════
  describe "CallHandler for Logic handlers" $ do

    it "invokes handler that returns gotoExit" $
      let handler :: Int -> Eff '[] (GotoChoice '[To Exit Int])
          handler n = pure $ gotoExit (n * 2)
          result = runPureEff $ callHandler handler (21 :: Int)
      in case result of
        GotoChoice (Here n) -> n `shouldBe` (42 :: Int)
        GotoChoice (There _) -> expectationFailure "Unexpected There"

    it "invokes handler that returns gotoChoice" $
      let handler :: Int -> Eff '[] (GotoChoice SimpleTargets)
          handler n = pure $ gotoChoice @"next" (("got: " ++ show n) :: String)
          result = runPureEff $ callHandler handler (42 :: Int)
      in case result of
        GotoChoice (Here s) -> s `shouldBe` "got: 42"
        GotoChoice (There _) -> expectationFailure "Expected position 0"

    it "invokes handler with branching logic" $ do
      -- Test negative -> pathA
      let handler :: Int -> Eff '[] (GotoChoice BranchTargets)
          handler n
            | n < 0     = pure $ gotoChoice @"pathA" (abs n)
            | n > 100   = pure $ gotoChoice @"pathB" (n - 100)
            | otherwise = pure $ gotoExit (("normal: " ++ show n) :: String)
          resultA = runPureEff $ callHandler handler ((-5) :: Int)
      case resultA of
        GotoChoice (Here n) -> n `shouldBe` (5 :: Int)
        _ -> expectationFailure "Expected pathA"

      -- Test > 100 -> pathB
      let resultB = runPureEff $ callHandler handler (150 :: Int)
      case resultB of
        GotoChoice (There (Here n)) -> n `shouldBe` (50 :: Int)
        _ -> expectationFailure "Expected pathB"

      -- Test normal -> Exit
      let resultExit = runPureEff $ callHandler handler (42 :: Int)
      case resultExit of
        GotoChoice (There (There (Here s))) -> s `shouldBe` "normal: 42"
        _ -> expectationFailure "Expected Exit"

    it "invokes handler with multiple parameters (via tuple)" $
      let handler :: (Int, String) -> Eff '[] (GotoChoice '[To Exit String])
          handler (n, s) = pure $ gotoExit ((s ++ ": " ++ show n) :: String)
          result = runPureEff $ callHandler handler ((42, "answer") :: (Int, String))
      in case result of
        GotoChoice (Here s) -> s `shouldBe` "answer: 42"
        GotoChoice (There _) -> expectationFailure "Unexpected There"

    it "invokes handler with unit input" $
      let handler :: () -> Eff '[] (GotoChoice '[To Exit String])
          handler () = pure $ gotoExit ("started" :: String)
          result = runPureEff $ callHandler handler ()
      in case result of
        GotoChoice (Here s) -> s `shouldBe` "started"
        GotoChoice (There _) -> expectationFailure "Unexpected There"

  -- ════════════════════════════════════════════════════════════════════════════
  -- HANDLER TYPE INFERENCE
  -- ════════════════════════════════════════════════════════════════════════════
  describe "Handler type inference" $ do

    it "handler type determines payload and targets" $ do
      -- The CallHandler instance infers payload type from handler signature
      let intHandler :: Int -> Eff '[] (GotoChoice '[To Exit Bool])
          intHandler n = pure $ gotoExit (n > 0)
          intResult = runPureEff $ callHandler intHandler (42 :: Int)

      case intResult of
        GotoChoice (Here b) -> b `shouldBe` True
        GotoChoice (There _) -> expectationFailure "Unexpected There"

      let strHandler :: String -> Eff '[] (GotoChoice '[To Exit Int])
          strHandler s = pure $ gotoExit (length s)
          strResult = runPureEff $ callHandler strHandler ("hello" :: String)

      case strResult of
        GotoChoice (Here n) -> n `shouldBe` (5 :: Int)
        GotoChoice (There _) -> expectationFailure "Unexpected There"

  -- ════════════════════════════════════════════════════════════════════════════
  -- EDGE CASES
  -- ════════════════════════════════════════════════════════════════════════════
  describe "Edge cases" $ do

    it "handler with complex return type" $
      let handler :: Int -> Eff '[] (GotoChoice '[To Exit (Int, Int, Int)])
          handler n = pure $ gotoExit (n, n * 2, n * 3)
          result = runPureEff $ callHandler handler (10 :: Int)
      in case result of
        GotoChoice (Here (a, b, c)) -> do
          a `shouldBe` (10 :: Int)
          b `shouldBe` (20 :: Int)
          c `shouldBe` (30 :: Int)
        GotoChoice (There _) -> expectationFailure "Unexpected There"

    it "handler identity (just wraps input)" $
      let handler :: Int -> Eff '[] (GotoChoice '[To Exit Int])
          handler = pure . gotoExit
          result = runPureEff $ callHandler handler (123 :: Int)
      in case result of
        GotoChoice (Here n) -> n `shouldBe` (123 :: Int)
        GotoChoice (There _) -> expectationFailure "Unexpected There"
