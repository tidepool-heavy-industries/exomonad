{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- | Node and graph metadata effects for LLM node context.
--
-- These effects provide pure Reader-like context injection for node-level
-- and graph-level metadata. Used by the teaching infrastructure to know
-- which node/graph is currently executing.
--
-- = Architecture
--
-- @
-- DispatchGoto
--     │ (wrap with runNodeMeta)
-- runNodeMeta (NodeMetadata "gClassify" "MyGraph") $
--     │
-- CallHandler → executeLLMHandler → runTurn
--     │                               │
--     │                          getNodeMeta (reads pure context)
--     │                               │
--     │                          send (RunTurnOp meta ...)
--     │
-- runLLMWithTeaching (uses meta for recording)
-- @
--
-- = Design
--
-- These effects are pure Reader patterns - no IORef, no mutable state.
-- Each handler invocation gets its own scoped context via interpreter.
--
-- * 'GraphMeta' - Set once at 'runGraph' level (graph name)
-- * 'NodeMeta' - Set per-handler by 'DispatchNamedNode' (node name)
-- * Production interpreters ignore metadata
-- * Teaching interpreters use metadata for recording
module Tidepool.Effect.NodeMeta
  ( -- * Metadata Types
    NodeMetadata(..)
  , GraphMetadata(..)
  , defaultNodeMeta
  , defaultGraphMeta

    -- * Node Metadata Effect
  , NodeMeta(..)
  , getNodeMeta

    -- * Graph Metadata Effect
  , GraphMeta(..)
  , getGraphMeta

    -- * Interpreters
  , runNodeMeta
  , runGraphMeta
    -- * Interposition (Local Override)
  , withNodeMeta
  , withGraphMeta
  ) where

import Control.Monad.Freer (Eff, Member, interpret, send, interpose)
import Data.Text (Text)

-- ════════════════════════════════════════════════════════════════════════════
-- METADATA TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Metadata about the currently executing node.
--
-- This is set by 'DispatchNamedNode' when invoking each handler,
-- extracted from the type-level Symbol field name.
data NodeMetadata = NodeMetadata
  { nmNodeName :: Text
    -- ^ Name of the node (field name from graph record, e.g., "gClassify")
  , nmGraphName :: Text
    -- ^ Name of the containing graph (from GraphMeta effect)
  }
  deriving (Show, Eq)

-- | Metadata about the currently executing graph.
--
-- This is set once at the 'runGraph' entry point.
data GraphMetadata = GraphMetadata
  { gmGraphName :: Text
    -- ^ Name of the graph type (e.g., "SupportGraph")
  }
  deriving (Show, Eq)

-- | Default node metadata for contexts where metadata is unknown.
--
-- Used when:
-- * Running outside of graph dispatch (e.g., tests)
-- * Running with production interpreter that doesn't care about metadata
defaultNodeMeta :: NodeMetadata
defaultNodeMeta = NodeMetadata
  { nmNodeName = "unknown"
  , nmGraphName = "unknown"
  }

-- | Default graph metadata for unknown contexts.
defaultGraphMeta :: GraphMetadata
defaultGraphMeta = GraphMetadata
  { gmGraphName = "unknown"
  }

-- ════════════════════════════════════════════════════════════════════════════
-- NODE METADATA EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect for reading node-level metadata.
--
-- This is a pure Reader pattern - the metadata is scoped to the current
-- handler invocation and cannot be modified.
data NodeMeta r where
  GetNodeMeta :: NodeMeta NodeMetadata

-- | Read the current node metadata.
--
-- Returns 'NodeMetadata' containing the node name and graph name.
-- In teaching mode, this is used to annotate recorded LLM turns.
getNodeMeta :: Member NodeMeta effs => Eff effs NodeMetadata
getNodeMeta = send GetNodeMeta

-- | Interpret 'NodeMeta' effect with provided metadata.
--
-- Used by 'DispatchNamedNode' to scope metadata to each handler call:
--
-- @
-- dispatchNamedNode graph payload = do
--   let nodeName = symbolVal (Proxy @name)
--       meta = NodeMetadata nodeName graphName
--   runNodeMeta meta $ callHandler handler payload
-- @
runNodeMeta :: NodeMetadata -> Eff (NodeMeta ': effs) a -> Eff effs a
runNodeMeta meta = interpret $ \case
  GetNodeMeta -> pure meta

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH METADATA EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect for reading graph-level metadata.
--
-- This is set once at graph entry and available throughout execution.
data GraphMeta r where
  GetGraphMeta :: GraphMeta GraphMetadata

-- | Read the current graph metadata.
getGraphMeta :: Member GraphMeta effs => Eff effs GraphMetadata
getGraphMeta = send GetGraphMeta

-- | Interpret 'GraphMeta' effect with provided metadata.
--
-- Used at 'runGraph' entry point:
--
-- @
-- runGraph handlers input = do
--   let meta = GraphMetadata "MyGraph"
--   runGraphMeta meta $ dispatchEntry handlers input
-- @
runGraphMeta :: GraphMetadata -> Eff (GraphMeta ': effs) a -> Eff effs a
runGraphMeta meta = interpret $ \case
  GetGraphMeta -> pure meta


-- ════════════════════════════════════════════════════════════════════════════
-- INTERPOSITION (LOCAL OVERRIDE)
-- ════════════════════════════════════════════════════════════════════════════

-- | Locally override the node metadata for a computation.
--
-- Unlike 'runNodeMeta' which interprets and removes the effect from the stack,
-- 'withNodeMeta' intercepts the effect and provides a local value while keeping
-- the effect in the stack. This is useful for dispatch-time metadata injection.
--
-- = Usage
--
-- @
-- -- In DispatchNamedNode:
-- dispatchNamedNode graph payload = do
--   let nodeName = symbolVal (Proxy @name)
--       meta = NodeMetadata nodeName "MyGraph"
--   withNodeMeta meta $ callHandler handler payload
-- @
--
-- = How it works
--
-- Uses freer-simple's 'interpose' to intercept 'GetNodeMeta' requests and
-- return the specified metadata, without removing 'NodeMeta' from the stack.
-- This allows nested 'withNodeMeta' calls where inner calls shadow outer ones.
withNodeMeta :: Member NodeMeta effs => NodeMetadata -> Eff effs a -> Eff effs a
withNodeMeta meta = interpose $ \case
  GetNodeMeta -> pure meta

-- | Locally override the graph metadata for a computation.
--
-- Similar to 'withNodeMeta', this intercepts 'GraphMeta' without removing it
-- from the stack.
withGraphMeta :: Member GraphMeta effs => GraphMetadata -> Eff effs a -> Eff effs a
withGraphMeta meta = interpose $ \case
  GetGraphMeta -> pure meta
