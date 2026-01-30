-- |
-- Module: ExoMonad.WASM
-- Description: Minimal re-exports for WASM builds
--
-- This module provides a minimal surface area for WASM builds, excluding:
-- - Documentation generators (Mermaid, Docs)
-- - Example graphs (Graph.Example)
-- - Dev-only types (Anthropic.Types, Question, Tool, Schema, Delta)
--
-- Expected savings: ~500 KB in WASM binary size.
--
-- Usage in WASM packages:
-- @
-- import ExoMonad.WASM
-- @
--
-- Instead of:
-- @
-- import ExoMonad  -- pulls in everything
-- @
module ExoMonad.WASM
  ( -- * Graph System

    -- ** Core Type-Level Operators
    type (:@),
    Input,
    UsesEffects,
    Self,

    -- ** Generic Graph Modes
    GraphMode (..), -- Includes (:-) type operator
    AsGraph,
    EntryNode,
    ExitNode,
    LLMNode,
    LogicNode,

    -- ** Goto System
    module ExoMonad.Graph.Goto,
    module ExoMonad.Graph.Goto.Internal,

    -- ** Graph Reification
    module ExoMonad.Graph.Reify,

    -- * Effects
    module ExoMonad.Effect.Metadata,
  )
where

-- Graph types (selective re-export to avoid Exit conflict)

-- Goto system

-- Reification

-- Effect metadata
import ExoMonad.Effect.Metadata
import ExoMonad.Graph.Generic
  ( AsGraph,
    EntryNode,
    ExitNode,
    GraphMode (..),
    LLMNode,
    LogicNode,
    type (:-),
  )
import ExoMonad.Graph.Goto
import ExoMonad.Graph.Goto.Internal
import ExoMonad.Graph.Reify
import ExoMonad.Graph.Types (Input, Self, UsesEffects, type (:@))
