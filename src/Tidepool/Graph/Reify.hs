{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Runtime reification of type-level graph information.
--
-- This module provides typeclasses that convert compile-time type information
-- into runtime data structures. This enables:
--
-- * Mermaid diagram generation
-- * Runtime introspection for debugging
-- * Dynamic graph traversal
--
-- Note: Full reification of effect stacks is complex due to kind mismatches.
-- This module provides simplified versions that can be extended.
module Tidepool.Graph.Reify
  ( -- * Graph Info Types
    GraphInfo(..)
  , NodeInfo(..)
  , EdgeInfo(..)
  , RuntimeNodeKind(..)
  , RuntimeEdgeKind(..)
  , ToolInfo(..)

    -- * Reification Typeclasses
  , ReifyGraph(..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Typeable (TypeRep)

import Tidepool.Graph.Edges (EdgeKind(..))
import Tidepool.Graph.Tool (ToolInfo(..))

-- ════════════════════════════════════════════════════════════════════════════
-- RUNTIME INFO TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Complete runtime representation of a graph.
data GraphInfo = GraphInfo
  { giEntryType :: Maybe TypeRep   -- ^ Type accepted at Entry
  , giExitType :: Maybe TypeRep    -- ^ Type produced at Exit
  , giNodes :: [NodeInfo]          -- ^ All node declarations
  , giEdges :: [EdgeInfo]          -- ^ Derived edges
  , giGroups :: [(Text, [Text])]   -- ^ Groups annotation (for Mermaid subgraphs)
  }
  deriving (Show, Eq)

-- | Runtime representation of a node.
data NodeInfo = NodeInfo
  { niName :: Text               -- ^ Node name (from Symbol)
  , niKind :: RuntimeNodeKind    -- ^ LLM or Logic
  , niNeeds :: [TypeRep]         -- ^ Types this node needs
  , niSchema :: Maybe TypeRep    -- ^ Schema output type (LLM nodes)
  , niGotoTargets :: [(Text, TypeRep)]  -- ^ Goto targets (Logic nodes)
  , niHasGotoExit :: Bool        -- ^ Can this node exit the graph?
  , niIsConditional :: Bool      -- ^ Has When annotation?
  , niHasVision :: Bool          -- ^ Has Vision annotation?
  , niTools :: [TypeRep]         -- ^ Tool types (for backwards compat)
  , niToolInfos :: [ToolInfo]    -- ^ Full tool info with schemas (V2)
  , niTemplate :: Maybe TypeRep  -- ^ Template type
  }
  deriving (Show, Eq)

-- | Runtime representation of an edge.
data EdgeInfo = EdgeInfo
  { eiFrom :: Text               -- ^ Source node (or "Entry")
  , eiTo :: Text                 -- ^ Target node (or "Exit")
  , eiPayload :: Maybe TypeRep   -- ^ Type carried on edge
  , eiKind :: RuntimeEdgeKind    -- ^ Edge classification
  }
  deriving (Show, Eq)

-- | Runtime node kind.
data RuntimeNodeKind = RuntimeLLM | RuntimeLogic
  deriving (Show, Eq)

-- | Runtime edge kind.
data RuntimeEdgeKind
  = RuntimeImplicit    -- ^ Schema → Needs (data flow)
  | RuntimeExplicit    -- ^ Goto (transition)
  | RuntimeConditional -- ^ From When-guarded node
  deriving (Show, Eq)

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH REIFICATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for reifying a complete graph.
--
-- Instances should be generated via Template Haskell or written manually.
-- The default implementation provides a stub.
class ReifyGraph (g :: Type) where
  reifyGraph :: GraphInfo
  reifyGraph = GraphInfo
    { giEntryType = Nothing
    , giExitType = Nothing
    , giNodes = []
    , giEdges = []
    , giGroups = []
    }

-- Note: Full reification requires sophisticated type-level machinery to:
-- 1. Pattern match on polykinded effect stacks
-- 2. Extract Goto targets from Effect kind lists
-- 3. Properly handle the complex instance overlap
--
-- For production use, consider:
-- - Using Template Haskell to generate ReifyGraph instances
-- - Implementing a simpler DSL that uses Type kind consistently
-- - Building the GraphInfo at the call site with explicit parameters
--
-- The TH module can generate proper instances that have access to
-- the graph structure at compile time.
