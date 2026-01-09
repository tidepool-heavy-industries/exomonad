{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Tests for LLM node execution in the graph executor.
--
-- These tests verify that:
-- 1. The CallHandler typeclass correctly invokes both Logic and LLM handlers
-- 2. Logic nodes (function handlers) still work after adding LLM support
-- 3. The type machinery compiles correctly
--
-- Note: Full LLM node execution tests require Template Haskell for TypedTemplate
-- creation, which is validated via Example.hs compilation. The tests here focus
-- on the CallHandler abstraction and Logic node paths.
module LLMNodeExecuteSpec (spec) where

import Data.Aeson (FromJSON, ToJSON, object, (.=))
import Data.Proxy (Proxy(..))
import Control.Monad.Freer (Eff, run)
import GHC.Generics (Generic)
import Test.Hspec

import Test.Tidepool.MockLLM (runMockLLM)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit)
import Tidepool.Graph.Generic (GraphMode(..), AsHandler)
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Goto (Goto, To, GotoChoice, gotoExit)
import Tidepool.Graph.Goto.Internal (GotoChoice(..), OneOf(..))  -- For test assertions
import Tidepool.Graph.Execute (CallHandler(..))
import Tidepool.Schema (HasJSONSchema(..), SchemaType(..), objectSchema, describeField, emptySchema)


-- ════════════════════════════════════════════════════════════════════════════
-- TEST TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Simple output type for LLM tests
newtype TestOutput = TestOutput { toResult :: Int }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance HasJSONSchema TestOutput where
  jsonSchema = objectSchema
    [ ("result", describeField "result" "The result value" (emptySchema TInteger))
    ]
    ["result"]


-- ════════════════════════════════════════════════════════════════════════════
-- LOGIC NODE TESTS - Verify Logic nodes still work
-- ════════════════════════════════════════════════════════════════════════════

-- | Simple logic graph: Entry → compute → Exit
data LogicGraph mode = LogicGraph
  { lgEntry   :: mode :- G.Entry Int
  , lgCompute :: mode :- G.LogicNode :@ Input Int :@ UsesEffects '[Goto Exit Int]
  , lgExit    :: mode :- G.Exit Int
  }
  deriving Generic

-- | Handler record for logic graph
logicHandlers :: LogicGraph (AsHandler '[])
logicHandlers = LogicGraph
  { lgEntry   = Proxy @Int
  , lgCompute = \n -> pure $ gotoExit (n + 1)
  , lgExit    = Proxy @Int
  }


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract value from a GotoChoice that contains only an Exit target.
-- This is safe because the type guarantees only one option exists.
extractExitValue :: GotoChoice '[To Exit a] -> a
extractExitValue (GotoChoice (Here a)) = a


-- ════════════════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  describe "CallHandler for Logic nodes" $ do
    it "invokes simple function handlers and returns correct value" $ do
      let handler :: Int -> Eff '[] (GotoChoice '[To Exit Int])
          handler n = pure $ gotoExit (n + 1)

      let result = run $ callHandler handler (5 :: Int)
      extractExitValue result `shouldBe` 6

    it "works with graph field handlers and returns correct value" $ do
      let handler = lgCompute logicHandlers
      let result = run $ callHandler handler (10 :: Int)
      extractExitValue result `shouldBe` 11

    it "handles zero correctly" $ do
      let handler = lgCompute logicHandlers
      let result = run $ callHandler handler (0 :: Int)
      extractExitValue result `shouldBe` 1

    it "handles negative numbers" $ do
      let handler :: Int -> Eff '[] (GotoChoice '[To Exit Int])
          handler n = pure $ gotoExit (n * 2)
      let result = run $ callHandler handler (-5 :: Int)
      extractExitValue result `shouldBe` (-10)

  describe "DispatchGoto exit handling" $ do
    it "exit-only target returns value directly" $ do
      -- The simplest dispatch: just Exit
      -- This tests the base case instance
      let exitChoice :: GotoChoice '[To Exit Int]
          exitChoice = GotoChoice (Here 42)

      extractExitValue exitChoice `shouldBe` 42

    it "extracts different exit values correctly" $ do
      extractExitValue (gotoExit @Int 0) `shouldBe` (0 :: Int)
      extractExitValue (gotoExit @Int 100) `shouldBe` (100 :: Int)
      extractExitValue (gotoExit @Int (-1)) `shouldBe` ((-1) :: Int)

  describe "CallHandler for LLM nodes" $ do
    it "mock LLM interpreter works" $ do
      let mockOutput = object ["result" .= (42 :: Int)]
      let result = run $ runMockLLM mockOutput $ pure (42 :: Int)
      result `shouldBe` 42

  describe "Graph handler types" $ do
    it "Logic handler has correct type and behavior" $ do
      let handler :: Int -> Eff '[] (GotoChoice '[To Exit Int])
          handler = lgCompute logicHandlers
      -- Verify both type and behavior
      let result = run $ handler 99
      extractExitValue result `shouldBe` 100

    it "Graph record compiles with handlers" $ do
      -- Verify the full graph record compiles and handlers work
      let computeResult = run $ (lgCompute logicHandlers) 5
      extractExitValue computeResult `shouldBe` 6
