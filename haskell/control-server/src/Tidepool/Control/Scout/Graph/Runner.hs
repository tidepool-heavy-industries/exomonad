{-# LANGUAGE FlexibleContexts #-}

-- | Graph runner for DocGen exploration.
--
-- This module provides the entry point for running the DocGen graph,
-- replacing the hand-rolled BFS loop with graph-based execution.
--
-- = Usage
--
-- @
-- -- In Handler/MCP.hs or similar:
-- result <- runDocGenGraph query lspSession
-- @
--
-- = Effect Stack
--
-- The runner composes:
-- - Memory ExploreState (graph-level state)
-- - NodeMeta / GraphMeta (for dispatch)
-- - LSP (provided by caller)
-- - Log (provided by caller)
-- - LLM (for dgSelect node - provided by caller)
module Tidepool.Control.Scout.Graph.Runner
  ( -- * Main Entry Point
    runDocGenGraph
  , runDocGenGraphEff

    -- * Re-exports
  , module Tidepool.Control.Scout.Graph
  , module Tidepool.Control.Scout.Graph.Types
  ) where

import Control.Monad.Freer (Eff, Member)

import Tidepool.Effect (LLM)
import Tidepool.Effect.NodeMeta (NodeMeta, GraphMeta, runNodeMeta, runGraphMeta, defaultNodeMeta, GraphMetadata(..))
import Tidepool.Effect.Types (Log, logDebug)
import Tidepool.Effect.LSP (LSP)
import Tidepool.Graph.Interpret (runGraph)
import Tidepool.Graph.Memory (runMemory)
import Tidepool.Platform (NativeOnly)

import Tidepool.Control.Scout.Graph
import Tidepool.Control.Scout.Graph.Types
import Tidepool.Control.Scout.Graph.Handlers (docGenHandlers)


-- ════════════════════════════════════════════════════════════════════════════
-- MAIN ENTRY POINT
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the DocGen graph and return a TeachingDoc.
--
-- This is the main entry point for graph-based exploration. It:
-- 1. Initializes the Memory effect with empty ExploreState
-- 2. Sets up GraphMeta and NodeMeta for dispatch
-- 3. Runs the graph from entry to exit
-- 4. Returns the final TeachingDoc
--
-- = Effect Requirements
--
-- The caller must provide these effects in their stack:
-- - 'LSP' - for symbol lookup (workspace/symbol, hover)
-- - 'Log' - for debug logging
-- - 'LLM' - for the dgSelect node's LLM call
--
-- = Example
--
-- @
-- import Tidepool.Control.Scout.Graph.Runner
--
-- handleScoutTeach :: TeachQuery -> Eff '[LSP, Log, LLM, IO] TeachingDoc
-- handleScoutTeach query = runDocGenGraphEff query
-- @
runDocGenGraphEff
  :: ( Member LSP es
     , Member Log es
     , Member LLM es
     , Member NodeMeta es
     , Member GraphMeta es
     , NativeOnly
     )
  => TeachQuery
  -> Eff es TeachingDoc
runDocGenGraphEff query = do
  logDebug $ "[DocGenGraph] Starting exploration for topic: " <> tqTopic query

  -- Initialize with empty state (handlers will populate via Memory)
  let initialState = emptyExploreState

  -- Run the graph with Memory effect
  (result, _finalState) <- runMemory initialState $
    runGraph docGenHandlers query

  logDebug "[DocGenGraph] Graph execution complete"
  pure result


-- | Run the DocGen graph with full effect stack setup.
--
-- This convenience function sets up NodeMeta and GraphMeta effects,
-- then delegates to 'runDocGenGraphEff'.
--
-- Use this when you need to run the graph but don't have NodeMeta/GraphMeta
-- in your effect stack.
runDocGenGraph
  :: ( Member LSP es
     , Member Log es
     , Member LLM es
     , NativeOnly
     )
  => TeachQuery
  -> Eff es TeachingDoc
runDocGenGraph query =
  runGraphMeta (GraphMetadata "DocGenGraph") $
    runNodeMeta defaultNodeMeta $
      runDocGenGraphEff query


-- ════════════════════════════════════════════════════════════════════════════
-- INTERNAL HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Create an empty initial ExploreState.
--
-- The graph's dgInit handler will properly initialize this
-- based on the TeachQuery.
emptyExploreState :: ExploreState
emptyExploreState = initialExploreState "" [] 0 0
