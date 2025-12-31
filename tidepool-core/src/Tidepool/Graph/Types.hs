{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Core types for the Tidepool Graph DSL.
--
-- This module defines annotations and shared types for the Graph DSL.
-- The record-based DSL (see Tidepool.Graph.Generic) is now the preferred syntax.
module Tidepool.Graph.Types
  ( -- * Node Kind
    NodeKind(..)

    -- * Annotations
  , type (:@)
  , Needs
  , Schema
  , System
  , Template
  , Vision
  , Tools
  , UsesEffects
  , Memory

    -- * Graph-Level Annotations
  , type (:&)
  , Groups
  , Requires
  , Global

    -- * Special Goto Targets
  , Exit
  , Self
  ) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol)

-- ════════════════════════════════════════════════════════════════════════════
-- NODE KIND
-- ════════════════════════════════════════════════════════════════════════════

-- | The kind of a node determines its behavior:
--
-- * 'LLM' nodes call the language model and produce output via 'Schema'
-- * 'Logic' nodes run pure/effectful code and transition via 'Goto'
--
-- Note: For the record-based DSL, use LLMNode and LogicNode from Tidepool.Graph.Generic
data NodeKind
  = LLM    -- ^ Node that invokes the LLM. Output flows implicitly via Schema.
  | Logic  -- ^ Node with effect stack. Transitions explicitly via Goto.

-- ════════════════════════════════════════════════════════════════════════════
-- ANNOTATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Attach an annotation to a node. Annotations are applied right-to-left:
-- @"node" := LLM :@ Needs '[A] :@ Schema B@ has Needs and Schema annotations.
type (:@) :: Type -> Type -> Type
data node :@ annotation
infixl 7 :@

-- | Declares what types a node needs as input. These become handler parameters.
-- Edges are derived: any node producing a needed type (via Schema or Goto)
-- creates an edge to this node.
type Needs :: [Type] -> Type
data Needs types

-- | Declares the output type of an LLM node. This output flows implicitly
-- to any node that 'Needs' this type.
type Schema :: Type -> Type
data Schema output

-- | System prompt template for an LLM node. Rendered before the user prompt.
-- Uses a separate TemplateDef from the user 'Template' annotation.
--
-- @
-- "classify" := LLM
--     :@ System ClassifySystemTpl   -- System prompt (optional)
--     :@ Template ClassifyUserTpl   -- User prompt
--     :@ Schema Intent
-- @
type System :: Type -> Type
data System tpl

-- | User prompt template for an LLM node. This is the main prompt that
-- contains the request/context for the LLM.
type Template :: Type -> Type
data Template tpl

-- | Marker for LLM nodes that process images/vision input.
data Vision

-- | List of tools available to an LLM node during execution.
type Tools :: [Type] -> Type
data Tools tools

-- | Effect stack for Logic nodes. Contains the effects the handler can use,
-- including 'Goto' effects for transitions.
--
-- Note: This takes a list of effects with kind [Effect] where
-- Effect = (Type -> Type) -> Type -> Type
--
-- Renamed from 'Eff' to avoid conflict with Effectful's 'Eff' monad type.
--
-- @
-- UsesEffects '[State MyState, Goto "nextNode" PayloadType, Goto Exit ResultType]
-- @
type UsesEffects :: [k] -> Type
data UsesEffects effects

-- | Node-private persistent memory. Each node can declare its own state type
-- that persists across graph runs. Only this node can access its Memory.
--
-- @
-- "explore" := LLM
--     :@ Needs '[Query]
--     :@ Schema Findings
--     :@ Memory ExploreMem   -- Private state for this node
-- @
type Memory :: Type -> Type
data Memory stateType

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH-LEVEL ANNOTATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Attach a graph-level annotation. Applied after the Graph declaration:
-- @Graph '[...] :& Groups '[...] :& Requires '[...]@
type (:&) :: Type -> Type -> Type
data graph :& annotation
infixl 4 :&

-- | Organize nodes into named groups for Mermaid subgraph rendering.
--
-- @
-- Groups '[
--     "intake"   := '["classify", "route"]
--   , "handlers" := '["refund", "technical", "billing"]
--   ]
-- @
type Groups :: [(Symbol, [Symbol])] -> Type
data Groups groups

-- | Declare effects required by the graph at the top level.
-- Used for documentation and runner configuration.
type Requires :: [Type] -> Type
data Requires effects

-- | Graph-level shared state accessible to all nodes. Unlike node-private
-- 'Memory', Global state can be read and updated by any node in the graph.
--
-- Used with the (:&) operator for graph-level annotations.
type Global :: Type -> Type
data Global stateType

-- ════════════════════════════════════════════════════════════════════════════
-- SPECIAL GOTO TARGET
-- ════════════════════════════════════════════════════════════════════════════

-- | Special marker type used as a target for the Goto effect to exit the graph.
--
-- @
-- -- In a Logic node's effect stack:
-- UsesEffects '[State S, Goto "nextNode" A, Goto Exit FinalResult]
-- @
--
-- Note: The old list-based DSL syntax @Exit :<~ Type@ has been removed.
-- This @Exit@ type is retained solely as a special target for the @Goto@
-- effect in the record-based Graph DSL. Record-based graphs use @G.Exit@
-- from "Tidepool.Graph.Generic" for their exit field definitions.
data Exit

-- | Self-loop marker for transitions back to the current node.
--
-- Used for retry/continuation patterns:
--
-- @
-- Goto Self UpdatedState
-- @
data Self
