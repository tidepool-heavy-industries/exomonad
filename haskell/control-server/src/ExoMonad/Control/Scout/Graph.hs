{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}  -- Input annotation is deprecated but simpler for this use case

-- | DocGen graph definition using the ExoMonad Graph DSL.
--
-- This module defines the graph structure for teaching document generation.
-- It replaces the hand-rolled BFS loop in DocGen.hs with a proper graph DSL
-- implementation that can be intercepted by exomonad-teaching for training
-- data capture.
--
-- = Graph Structure
--
-- @
-- Entry (TeachQuery)
--    │
--    ▼
-- dgInit (LogicNode) ─────────────────────────────────────────┐
--    │                                                         │
--    │ (first symbol from frontier)                            │
--    ▼                                                         │
-- dgProcess (LogicNode) ◄─────────────────────┐                │
--    │                                         │                │
--    ├─► (no candidates) ──────────────────────┤                │
--    │                                         │                │
--    ▼                                         │                │
-- dgSelect (LLMNode) ──► SelectOutput          │                │
--    │                                         │                │
--    ▼                                         │                │
-- dgExpand (LogicNode)                         │                │
--    │                                         │                │
--    ├─► (more symbols) ───────────────────────┘                │
--    │                                         │ (via frontier) │
--    └─► (done) ───────────────────────────────┼────────────────┤
--                                              │                │
--                                              ▼                │
--                                       dgFinalize (LogicNode) ◄┘
--                                              │
--                                              ▼
--                                        Exit (TeachingDoc)
-- @
--
-- = Key Design Decisions
--
-- 1. **Single LLM node** (dgSelect): Only the symbol selection step needs
--    an LLM call. All other nodes are pure logic using LSP data.
--
-- 2. **Memory annotation**: ExploreState is shared across all nodes via
--    the Memory effect, tracking frontier, visited, and accumulated graph.
--
-- 3. **Self-loop pattern**: dgExpand routes back to dgProcess for BFS iteration.
--    This is achieved via explicit Goto targets rather than Goto Self because
--    the loop goes through multiple nodes.
--
-- 4. **Budget tracking**: The budget decreases with each LLM call (dgSelect),
--    not with each symbol processed.
module ExoMonad.Control.Scout.Graph
  ( -- * Graph Definition
    DocGenGraph(..)

    -- * Re-exports for convenience
  , TeachQuery(..)
  , TeachingDoc(..)
  , ExploreState(..)
  , ProcessInput(..)
  , SelectInput(..)
  , SelectOutput(..)
  , ExpandInput(..)
  , FinalizeInput(..)
  , FinalizeReason(..)
  ) where

import GHC.Generics (Generic)

import ExoMonad.Graph.Generic
  ( GraphMode(..)
  , EntryNode
  , ExitNode
  , LLMNode
  , LogicNode
  )
import ExoMonad.Graph.Types
  ( type (:@)
  , Input
  , Schema
  , Template
  , Memory
  , UsesEffects
  , LLMKind(..)
  , MCPExport
  , MCPToolDef
  )
import ExoMonad.Graph.Goto (Goto)
import qualified ExoMonad.Graph.Types as G

-- Re-exports from DocGen types (existing infrastructure)
import ExoMonad.Control.Scout.DocGen.Types
  ( TeachQuery(..)
  , TeachingDoc(..)
  )

-- Re-exports from Graph types (new node types)
import ExoMonad.Control.Scout.Graph.Types
  ( ProcessInput(..)
  , SelectInput(..)
  , SelectOutput(..)
  , ExpandInput(..)
  , FinalizeInput(..)
  , FinalizeReason(..)
  , ExploreState(..)
  )

-- Template for dgSelect LLM node
import ExoMonad.Control.Scout.Graph.Templates (SelectTpl)


-- | DocGen exploration graph.
--
-- This graph defines the structure for teaching document generation:
--
-- - **dgInit**: Initialize exploration state from query
-- - **dgProcess**: Process a symbol from the frontier (LSP lookup)
-- - **dgSelect**: LLM selects relevant type dependencies
-- - **dgExpand**: Add selected symbols to frontier, advance
-- - **dgFinalize**: Build final TeachingDoc from accumulated state
--
-- The graph uses @Memory ExploreState@ to share BFS state across nodes.
data DocGenGraph mode = DocGenGraph
  { dgEntry :: mode :- EntryNode TeachQuery
      :@ MCPExport
      :@ MCPToolDef '("teach-graph", "Explore a codebase concept using intelligent symbol selection. Returns a teaching document with symbols ordered by prerequisites - learn foundational types before the code that uses them.")
    -- ^ Graph entry point: receives the teaching query

  , dgInit :: mode :- LogicNode
      :@ Input TeachQuery
      :@ Memory ExploreState
      :@ UsesEffects '[Goto "dgProcess" ProcessInput, Goto "dgFinalize" FinalizeInput]
    -- ^ Initialize state, pick first symbol from frontier, or finalize if empty

  , dgProcess :: mode :- LogicNode
      :@ Input ProcessInput
      :@ Memory ExploreState
      :@ UsesEffects '[Goto "dgSelect" SelectInput, Goto "dgFinalize" FinalizeInput]
    -- ^ Process a symbol: LSP hover, extract candidates, route to select or finalize

  , dgSelect :: mode :- LLMNode 'API
      :@ Input SelectInput
      :@ Template SelectTpl
      :@ Schema SelectOutput
      :@ Memory ExploreState
      :@ UsesEffects '[Goto "dgExpand" SelectOutput]
    -- ^ LLM selects relevant candidates from the list

  , dgExpand :: mode :- LogicNode
      :@ Input SelectOutput
      :@ Memory ExploreState
      :@ UsesEffects '[Goto "dgProcess" ProcessInput, Goto "dgFinalize" FinalizeInput]
    -- ^ Add selected symbols to frontier, advance to next or finalize
    -- (Gets current symbol info from Memory)

  , dgFinalize :: mode :- LogicNode
      :@ Input FinalizeInput
      :@ Memory ExploreState
      :@ UsesEffects '[Goto G.Exit TeachingDoc]
    -- ^ Build TeachingDoc from accumulated state

  , dgExit :: mode :- ExitNode TeachingDoc
    -- ^ Graph exit: returns the teaching document
  }
  deriving Generic
