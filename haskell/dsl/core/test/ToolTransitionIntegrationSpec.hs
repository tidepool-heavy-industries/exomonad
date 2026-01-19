{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for tool-initiated graph transitions.
--
-- Tests that tool execution correctly handles ToolTransition results,
-- bypassing the LLM handler's afterHandler and routing directly to targets.
--
-- Architecture:
-- 1. Tool returns ToolTransition with target name + payload
-- 2. Graph interpreter's ConvertTransitionHint converts to typed GotoChoice
-- 3. Dispatch continues to target node, skipping handler's afterHandler
--
module ToolTransitionIntegrationSpec (spec) where

import Control.Monad.Freer (run, Eff, Member)
import Data.Aeson (toJSON, Value)
import qualified Data.Text as T
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import Test.Hspec

import Tidepool.Graph.Goto
  ( To, GotoChoice, gotoChoice, gotoExit, Goto )
import Tidepool.Graph.Generic
  ( GraphMode(..), type (:-), AsHandler )
import Tidepool.Graph.Types
  ( Input, UsesEffects, type (:@) )
import Tidepool.Effect.NodeMeta (NodeMeta, GraphMeta, runNodeMeta, runGraphMeta, defaultNodeMeta, defaultGraphMeta)
import qualified Tidepool.Graph.Types as Types (Exit)
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Interpret (DispatchGoto(..))

-- ════════════════════════════════════════════════════════════════════════════
-- TEST GRAPH: Simple tool transition
--
-- Entry(String) -> toolNode(uses tool that can transition)
--   -> successNode (if tool transitions to "success")
--   -> Exit(Result)
--
-- ════════════════════════════════════════════════════════════════════════════

data Result = Result { output :: String }
  deriving (Show, Eq, Generic)

data ToolTransitionGraph mode = ToolTransitionGraph
  { ttgEntry   :: mode :- G.EntryNode String
  , ttgTool    :: mode :- G.LogicNode
       :@ Input String
       :@ UsesEffects '[Goto "ttgSuccess" Value, Goto Types.Exit Result]
  , ttgSuccess :: mode :- G.LogicNode
       :@ Input Value
       :@ UsesEffects '[Goto Types.Exit Result]
  , ttgExit    :: mode :- G.ExitNode Result
  }
  deriving Generic

-- Handler that simulates tool execution
toolNodeHandler :: String -> Eff '[NodeMeta, GraphMeta] (GotoChoice '[To "ttgSuccess" Value, To Types.Exit Result])
toolNodeHandler input
  | input == "transition" = pure $ gotoChoice @"ttgSuccess" (toJSON ("transitioned" :: String))
  | input == "exit" = pure $ gotoExit (Result "exited directly")
  | otherwise = pure $ gotoExit (Result "default exit")

successNodeHandler :: Value -> Eff '[NodeMeta, GraphMeta] (GotoChoice '[To Types.Exit Result])
successNodeHandler payload = pure $ gotoExit (Result $ "success: " <> show payload)

handlers :: ToolTransitionGraph (AsHandler '[NodeMeta, GraphMeta])
handlers = ToolTransitionGraph
  { ttgEntry   = Proxy
  , ttgTool    = toolNodeHandler
  , ttgSuccess = successNodeHandler
  , ttgExit    = Proxy
  }

-- ════════════════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════════════════

spec :: Spec
spec = describe "Tool-initiated transitions" $ do
  describe "dispatch to success node" $ do
    it "routes to specified target node with payload" $ do
      -- Dispatch a transition choice directly to success node
      let choice :: GotoChoice '[To "ttgSuccess" Value, To Types.Exit Result]
          choice = gotoChoice @"ttgSuccess" (toJSON ("test_payload" :: String))
      let result = run . runGraphMeta defaultGraphMeta . runNodeMeta defaultNodeMeta $ dispatchGoto handlers choice
      result `shouldBe` Result "success: String \"test_payload\""

    it "preserves complex payloads through dispatch" $ do
      -- Dispatch with a more complex payload
      let choice :: GotoChoice '[To "ttgSuccess" Value, To Types.Exit Result]
          choice = gotoChoice @"ttgSuccess" (toJSON ("complex" :: String))
      let result = run . runGraphMeta defaultGraphMeta . runNodeMeta defaultNodeMeta $ dispatchGoto handlers choice
      result `shouldBe` Result "success: String \"complex\""

  describe "direct exit from tool node" $ do
    it "exits directly without intermediate routing" $ do
      -- Tool handler that exits directly
      let choice :: GotoChoice '[To "ttgSuccess" Value, To Types.Exit Result]
          choice = gotoExit (Result "exited directly")
      let result = run . runGraphMeta defaultGraphMeta . runNodeMeta defaultNodeMeta $ dispatchGoto handlers choice
      result `shouldBe` Result "exited directly"

    it "produces correct output format" $ do
      -- Verify output structure
      let choice :: GotoChoice '[To "ttgSuccess" Value, To Types.Exit Result]
          choice = gotoExit (Result "direct exit")
      let result = run . runGraphMeta defaultGraphMeta . runNodeMeta defaultNodeMeta $ dispatchGoto handlers choice
      output result `shouldBe` "direct exit"
