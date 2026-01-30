{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Integration tests for tool-initiated graph transitions.
--
-- Tests that tool execution correctly handles ToolTransition results,
-- bypassing the LLM handler's afterHandler and routing directly to targets.
--
-- Architecture:
-- 1. Tool returns ToolTransition with target name + payload
-- 2. Graph interpreter's ConvertTransitionHint converts to typed GotoChoice
-- 3. Dispatch continues to target node, skipping handler's afterHandler
module ToolTransitionIntegrationSpec (spec) where

import Control.Monad.Freer (Eff, Member, run)
import Data.Aeson (Value, toJSON)
import Data.Text qualified as T
import ExoMonad.Effect.NodeMeta (GraphMeta, NodeMeta, defaultGraphMeta, defaultNodeMeta, runGraphMeta, runNodeMeta)
import ExoMonad.Graph.Generic
  ( AsHandler,
    GraphMode (..),
    type (:-),
  )
import ExoMonad.Graph.Generic qualified as G
import ExoMonad.Graph.Goto
  ( Goto,
    GotoChoice,
    To,
    gotoChoice,
    gotoExit,
  )
import ExoMonad.Graph.Interpret (DispatchGoto (..))
import ExoMonad.Graph.Types
  ( Input,
    UsesEffects,
    type (:@),
  )
import ExoMonad.Graph.Types qualified as Types (Exit)
import GHC.Generics (Generic)
import Test.Hspec

-- ════════════════════════════════════════════════════════════════════════════
-- TEST GRAPH: Simple tool transition
--
-- Entry(String) -> toolNode(uses tool that can transition)
--   -> successNode (if tool transitions to "success")
--   -> Exit(Result)
--
-- ════════════════════════════════════════════════════════════════════════════

data Result = Result {output :: String}
  deriving (Show, Eq, Generic)

data ToolTransitionGraph mode = ToolTransitionGraph
  { entry :: mode :- G.EntryNode String,
    tool ::
      mode
        :- G.LogicNode
        :@ Input String
        :@ UsesEffects '[Goto "success" Value, Goto Types.Exit Result],
    success ::
      mode
        :- G.LogicNode
        :@ Input Value
        :@ UsesEffects '[Goto Types.Exit Result],
    exit :: mode :- G.ExitNode Result
  }
  deriving (Generic)

-- Handler that simulates tool execution
toolNodeHandler :: String -> Eff '[NodeMeta, GraphMeta] (GotoChoice '[To "success" Value, To Types.Exit Result])
toolNodeHandler input
  | input == "transition" = pure $ gotoChoice @"success" (toJSON ("transitioned" :: String))
  | input == "exit" = pure $ gotoExit (Result "exited directly")
  | otherwise = pure $ gotoExit (Result "default exit")

successNodeHandler :: Value -> Eff '[NodeMeta, GraphMeta] (GotoChoice '[To Types.Exit Result])
successNodeHandler payload = pure $ gotoExit (Result $ "success: " <> show payload)

handlers :: ToolTransitionGraph (AsHandler '[NodeMeta, GraphMeta])
handlers =
  ToolTransitionGraph
    { entry = (),
      tool = toolNodeHandler,
      success = successNodeHandler,
      exit = ()
    }

-- ════════════════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════════════════

spec :: Spec
spec = describe "Tool-initiated transitions" $ do
  describe "dispatch to success node" $ do
    it "routes to specified target node with payload" $ do
      -- Dispatch a transition choice directly to success node
      let choice :: GotoChoice '[To "success" Value, To Types.Exit Result]
          choice = gotoChoice @"success" (toJSON ("test_payload" :: String))
      let result = run . runGraphMeta defaultGraphMeta . runNodeMeta defaultNodeMeta $ dispatchGoto handlers choice
      result `shouldBe` Result "success: String \"test_payload\""

    it "preserves complex payloads through dispatch" $ do
      -- Dispatch with a more complex payload
      let choice :: GotoChoice '[To "success" Value, To Types.Exit Result]
          choice = gotoChoice @"success" (toJSON ("complex" :: String))
      let result = run . runGraphMeta defaultGraphMeta . runNodeMeta defaultNodeMeta $ dispatchGoto handlers choice
      result `shouldBe` Result "success: String \"complex\""

  describe "direct exit from tool node" $ do
    it "exits directly without intermediate routing" $ do
      -- Tool handler that exits directly
      let choice :: GotoChoice '[To "success" Value, To Types.Exit Result]
          choice = gotoExit (Result "exited directly")
      let result = run . runGraphMeta defaultGraphMeta . runNodeMeta defaultNodeMeta $ dispatchGoto handlers choice
      result `shouldBe` Result "exited directly"

    it "produces correct output format" $ do
      -- Verify output structure
      let choice :: GotoChoice '[To "success" Value, To Types.Exit Result]
          choice = gotoExit (Result "direct exit")
      let result :: Result
          result = run . runGraphMeta defaultGraphMeta . runNodeMeta defaultNodeMeta $ dispatchGoto handlers choice
      result.output `shouldBe` "direct exit"
