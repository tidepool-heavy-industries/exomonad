-- | Type-Safe Graph DSL for LLM Agent State Machines
--
-- This module provides a type-level DSL for defining state machine graphs
-- that are validated at compile time and can generate Mermaid diagrams.
--
-- = Core Concepts
--
-- == Nodes
--
-- Nodes come in two kinds:
--
-- * __LLM nodes__: Call the language model, produce output via 'Schema'
-- * __Logic nodes__: Run effectful code, transition via 'Goto'
--
-- == Edges
--
-- Edges are derived automatically from:
--
-- * __Implicit edges__: When a node's 'Schema' output matches another
--   node's 'Needs' input
-- * __Explicit edges__: Each 'Goto' effect creates an edge to its target
--
-- == Effects
--
-- The DSL follows the principle: "Everything is an effect. Transitions
-- are the 'Goto' effect."
--
-- Logic nodes declare their effects via 'Eff', which includes 'Goto'
-- effects for possible transitions.
--
-- = Example
--
-- @
-- type SupportGraph = Graph '[
--     Entry :~> Message
--
--   , "classify" := LLM
--       :@ Needs '[Message]
--       :@ Template ClassifyTpl
--       :@ Schema Intent
--
--   , "route" := Logic
--       :@ Needs '[Message, Intent]
--       :@ Eff '[
--           Goto "refund" Message
--         , Goto "support" Message
--         , Goto Exit Response
--         ]
--
--   , "refund" := LLM
--       :@ Needs '[Message]
--       :@ Template RefundTpl
--       :@ Schema Response
--
--   , "support" := LLM
--       :@ Needs '[Message]
--       :@ Template SupportTpl
--       :@ Schema Response
--
--   , Exit :<~ Response
--   ]
-- @
--
-- = Validation
--
-- Graphs are validated at compile time:
--
-- * Must have exactly one Entry and one Exit
-- * All 'Needs' must be satisfied by Entry or some 'Schema'
-- * All 'Goto' targets must exist or be 'Exit'
--
-- = Diagram Generation
--
-- Generate Mermaid diagrams from graph definitions:
--
-- @
-- putStrLn $ toMermaid $ reifyGraph \@SupportGraph
-- @
module Tidepool.Graph
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
  , Memory

    -- * Graph-Level Annotations
  , type (:&)
  , Groups
  , Requires
  , Global

    -- * The Goto Effect
  , Goto(..)
  , goto

    -- * Validation
  , ValidGraph
  , HasEntry
  , HasExit
  , AllNeedsSatisfied
  , AllGotoTargetsExist
  , AllToolsHaveSchema
  , AllMemoriesValid

    -- * Reification
  , ReifyGraph(..)
  , GraphInfo(..)
  , NodeInfo(..)
  , EdgeInfo(..)
  , RuntimeNodeKind(..)
  , RuntimeEdgeKind(..)

    -- * Mermaid Diagrams
  , toMermaid
  , toMermaidWithConfig
  , MermaidConfig(..)
  , defaultConfig

    -- * Template Haskell
  , deriveHandlers
  , HandlersFor
  , HandlerType

    -- * Graph Execution
  , RunnableGraph(..)
  , runGraph
  , runGraphWith
  , GraphState(..)
  , GraphContext(..)
  , defaultContext

    -- * Type-Level Utilities
  , NodeName
  , GetNodeKind
  , GetAnnotations
  , GetNeeds
  , GetSchema
  , GetEff
  , GetGotoTargets
  , GetEntryType
  , GetExitType
  , GetMemory
  , GetGlobal

    -- * Tools
  , ToolDef(..)
  , ValidTool
  , ValidToolList
  , AllToolsValid
  , ToolInfo(..)
  , ReifyToolList(..)
  , toolInfoToJSON
  , toolToInfo
  ) where

import Tidepool.Graph.Types
import Tidepool.Graph.Goto
import Tidepool.Graph.Edges
import Tidepool.Graph.Validate
import Tidepool.Graph.Reify
import Tidepool.Graph.Mermaid
import Tidepool.Graph.TH
import Tidepool.Graph.Runner
import Tidepool.Graph.Tool
