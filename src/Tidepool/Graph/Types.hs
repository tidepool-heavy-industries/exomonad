{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Core types for the Tidepool Graph DSL.
--
-- This module defines the type-level syntax for declaring state machine graphs.
-- Graphs consist of nodes (LLM or Logic) connected by edges derived from
-- 'Needs', 'Schema', and 'Goto' annotations.
--
-- = Example
--
-- @
-- type MyGraph = Graph '[
--     Entry :~> InputType
--   , "process" := LLM
--       :@ Needs '[InputType]
--       :@ Template ProcessTpl
--       :@ Schema OutputType
--   , "decide" := Logic
--       :@ Needs '[OutputType]
--       :@ Eff '[Goto Exit ResultType]
--   , Exit :<~ ResultType
--   ]
-- @
module Tidepool.Graph.Types
  ( -- * Graph Structure
    Graph
  , Entry
  , Exit
  , type (:~>)
  , type (:<~)

    -- * Node Definition
  , type (:=)
  , NodeKind(..)

    -- * Annotations
  , type (:@)
  , Needs
  , Schema
  , Template
  , Vision
  , Tools
  , When
  , Eff

    -- * Graph-Level Annotations
  , type (:&)
  , Groups
  , Requires

    -- * Type-Level Utilities
  , NodeName
  , GetNodeKind
  , GetAnnotations
  ) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol)

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH STRUCTURE
-- ════════════════════════════════════════════════════════════════════════════

-- | A graph is a list of node declarations.
--
-- The list should contain:
--
-- * Exactly one 'Entry' declaration: @Entry :~> InputType@
-- * Zero or more node declarations: @"name" := Kind :@ ...@
-- * Exactly one 'Exit' declaration: @Exit :<~ OutputType@
type Graph :: [Type] -> Type
data Graph nodes

-- | Entry point marker. Used with ':~>' to declare the graph's input type.
data Entry

-- | Exit point marker. Used with ':<~' to declare the graph's output type.
-- Also used as a 'Goto' target: @Goto Exit ResultType@.
data Exit

-- | Entry point declaration. @Entry :~> InputType@ declares that the graph
-- accepts @InputType@ as input.
type (:~>) :: Type -> Type -> Type
data entry :~> inputType
infixr 5 :~>

-- | Exit point declaration. @Exit :<~ OutputType@ declares that the graph
-- produces @OutputType@ as output.
type (:<~) :: Type -> Type -> Type
data exit :<~ outputType
infixr 5 :<~

-- ════════════════════════════════════════════════════════════════════════════
-- NODE DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Node declaration. @"nodeName" := LLM@ declares a node with the given name
-- and kind. Annotations are attached with ':@'.
type (:=) :: Symbol -> NodeKind -> Type
data name := kind
infixr 8 :=

-- | The kind of a node determines its behavior:
--
-- * 'LLM' nodes call the language model and produce output via 'Schema'
-- * 'Logic' nodes run pure/effectful code and transition via 'Goto'
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

-- | The prompt template type for an LLM node.
type Template :: Type -> Type
data Template tpl

-- | Marker for LLM nodes that process images/vision input.
data Vision

-- | List of tools available to an LLM node during execution.
type Tools :: [Type] -> Type
data Tools tools

-- | Conditional execution. The node only runs if the condition is satisfied.
-- Downstream nodes receive 'Maybe' of this node's output.
type When :: Type -> Type
data When condition

-- | Effect stack for Logic nodes. Contains the effects the handler can use,
-- including 'Goto' effects for transitions.
--
-- Note: This takes a list of effects with kind [Effect] where
-- Effect = (Type -> Type) -> Type -> Type
--
-- @
-- Eff '[State MyState, Goto "nextNode" PayloadType, Goto Exit ResultType]
-- @
type Eff :: [k] -> Type
data Eff effects

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

-- ════════════════════════════════════════════════════════════════════════════
-- TYPE-LEVEL UTILITIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract the name from a node declaration.
type family NodeName (node :: Type) :: Symbol where
  NodeName (name := kind) = name
  NodeName (node :@ _) = NodeName node

-- | Extract the kind from a node declaration.
type family GetNodeKind (node :: Type) :: NodeKind where
  GetNodeKind (_ := kind) = kind
  GetNodeKind (node :@ _) = GetNodeKind node

-- | Extract all annotations from a node as a type-level list.
type family GetAnnotations (node :: Type) :: [Type] where
  GetAnnotations (_ := _) = '[]
  GetAnnotations (node :@ ann) = ann ': GetAnnotations node
