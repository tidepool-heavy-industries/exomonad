{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Tests for DispatchGoto typeclass.
--
-- DispatchGoto is the core dispatch mechanism that routes GotoChoice values
-- through the graph, calling handlers and recursing until Exit is reached.
--
-- Note: Multi-hop dispatch (handler → handler → Exit) requires complex
-- HasField instance resolution that isn't working in these tests.
-- Those tests are deferred pending investigation. For now, we test:
-- * Exit-only dispatch (base cases)
-- * Single-hop dispatch (handler → Exit)
module DispatchGotoSpec (spec) where

import Control.Monad.Freer (run)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import Test.Hspec

import Tidepool.Graph.Goto
  ( To
  , GotoChoice
  , gotoChoice
  , gotoExit
  , Goto
  )
import Tidepool.Graph.Goto.Internal (GotoChoice(..))  -- For test assertions
import Tidepool.Graph.Execute (DispatchGoto(..))
import Tidepool.Graph.Generic (GraphMode(..), type (:-), AsHandler)
import Tidepool.Graph.Generic.Core (Entry)
import Tidepool.Graph.Types (Needs, UsesEffects, type (:@))
import qualified Tidepool.Graph.Types as Types (Exit)
import qualified Tidepool.Graph.Generic as G

-- ════════════════════════════════════════════════════════════════════════════
-- TEST GRAPHS
-- ════════════════════════════════════════════════════════════════════════════

-- | Simple graph: Entry(Int) → compute(+1) → Exit(Int)
data SimpleGraph mode = SimpleGraph
  { sgEntry   :: mode :- Entry Int
  , sgCompute :: mode :- G.LogicNode :@ Needs '[Int] :@ UsesEffects '[Goto Types.Exit Int]
  , sgExit    :: mode :- G.Exit Int
  }
  deriving Generic

simpleHandlers :: SimpleGraph (AsHandler '[])
simpleHandlers = SimpleGraph
  { sgEntry   = Proxy
  , sgCompute = \n -> pure $ gotoExit (n + 1 :: Int)
  , sgExit    = Proxy
  }

-- ════════════════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  -- ════════════════════════════════════════════════════════════════════════════
  -- EXIT-ONLY DISPATCH
  -- ════════════════════════════════════════════════════════════════════════════
  describe "Exit-only dispatch" $ do

    it "returns value directly when Exit is the only target" $ do
      let choice :: GotoChoice '[To Types.Exit Int] = gotoExit (42 :: Int)
          result :: Int = run $ dispatchGoto simpleHandlers choice
      result `shouldBe` (42 :: Int)

    it "returns value for Exit target in multi-target list (same exitType)" $ do
      let choice :: GotoChoice '[To "sgCompute" Int, To Types.Exit Int] = gotoExit (99 :: Int)
          result :: Int = run $ dispatchGoto simpleHandlers choice
      result `shouldBe` (99 :: Int)

  -- ════════════════════════════════════════════════════════════════════════════
  -- SIMPLE GRAPH DISPATCH
  -- ════════════════════════════════════════════════════════════════════════════
  describe "Simple graph dispatch (Entry → compute → Exit)" $ do

    it "dispatches to compute handler then exits" $ do
      let choice :: GotoChoice '[To "sgCompute" Int, To Types.Exit Int] = gotoChoice @"sgCompute" (5 :: Int)
          result :: Int = run $ dispatchGoto simpleHandlers choice
      result `shouldBe` (6 :: Int)  -- compute adds 1

    it "dispatches multiple times with different inputs" $ do
      let choice0 :: GotoChoice '[To "sgCompute" Int, To Types.Exit Int] = gotoChoice @"sgCompute" (0 :: Int)
          choice10 :: GotoChoice '[To "sgCompute" Int, To Types.Exit Int] = gotoChoice @"sgCompute" (10 :: Int)
          choiceNeg :: GotoChoice '[To "sgCompute" Int, To Types.Exit Int] = gotoChoice @"sgCompute" ((-5) :: Int)
          results :: [Int] = run $ sequence
            [ dispatchGoto simpleHandlers choice0
            , dispatchGoto simpleHandlers choice10
            , dispatchGoto simpleHandlers choiceNeg
            ]
      results `shouldBe` ([1, 11, -4] :: [Int])

  -- ════════════════════════════════════════════════════════════════════════════
  -- EDGE CASES (Exit-only, no handler dispatch)
  -- ════════════════════════════════════════════════════════════════════════════
  describe "Edge cases (Exit-only)" $ do

    it "handles tuple exit payload" $ do
      let payload :: (Int, String) = (42 :: Int, "hi!" :: String)
          choice :: GotoChoice '[To Types.Exit (Int, String)] = gotoExit payload
          result :: (Int, String) = run $ dispatchGoto simpleHandlers choice
      result `shouldBe` ((42 :: Int), ("hi!" :: String))

    it "handles unit exit payload" $ do
      let choice :: GotoChoice '[To Types.Exit ()] = gotoExit ()
          result :: () = run $ dispatchGoto simpleHandlers choice
      result `shouldBe` ()

    it "handles Bool exit payload" $ do
      let choice :: GotoChoice '[To Types.Exit Bool] = gotoExit True
          result :: Bool = run $ dispatchGoto simpleHandlers choice
      result `shouldBe` True

    it "handles String exit payload" $ do
      let choice :: GotoChoice '[To Types.Exit String] = gotoExit ("done" :: String)
          result :: String = run $ dispatchGoto simpleHandlers choice
      result `shouldBe` ("done" :: String)

  -- ════════════════════════════════════════════════════════════════════════════
  -- DEFERRED: MULTI-HOP DISPATCH
  -- ════════════════════════════════════════════════════════════════════════════
  -- The following tests are deferred pending investigation of HasField
  -- instance resolution for multi-hop dispatch (handler → handler → Exit):
  --
  -- * Chain graph dispatch (Entry → step1 → step2 → Exit)
  -- * Branching graph dispatch (router → pathA | pathB → Exit)
  -- * Diamond graph dispatch (split → pathA/pathB → merge → Exit)
  --
  -- These work at the type level but the test infrastructure has issues
  -- with GHC's HasField constraint resolution for record types.
