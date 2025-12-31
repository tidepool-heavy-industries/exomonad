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
-- Logic nodes declare their effects via 'UsesEffects', which includes 'Goto'
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
--       :@ UsesEffects '[
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

    -- * The Goto Effect
  , Goto(..)
  , goto

    -- * The Memory Effect
    -- $memoryEffect
  , Mem.getMem
  , Mem.updateMem
  , Mem.modifyMem
  , Mem.runMemory
  , Mem.evalMemory
    -- For the Memory effect type itself, import Tidepool.Graph.Memory directly

    -- * Template Definitions
    -- $templateDef
  , Tpl.TemplateDef(..)
  , Tpl.renderTemplate
  , Tpl.GingerContext
  , Tpl.TypedTemplate
  , Tpl.typedTemplateFile
  , Tpl.runTypedTemplate
  , Tpl.makeTemplateCompiled
    -- ** Dependency Tracking
  , Tpl.TemplateDependency(..)
  , Tpl.DepRelation(..)
  , Tpl.DepLocation(..)
  , Tpl.TemplateContextInfo(..)
  , Tpl.templateDependencyTree
  , Tpl.flattenDeps

    -- * Template Documentation
    -- $templateDocs
  , Docs.renderDepTree
  , Docs.renderDepTreeCompact
  , Docs.templateDocBlock

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

    -- * Record-Based Graph DSL
    -- $recordDSL
  , G.GraphMode(..)
  , G.AsGraph
  , G.AsHandler
    -- For G.Entry, G.Exit, use: import qualified Tidepool.Graph.Generic as G
  , G.LLMNode
  , G.LogicNode
  , G.ValidGraphRecord
  , G.NodeHandler
  , G.GraphProduct(..)
  , G.FieldNamesOf
  , G.FieldsWithNamesOf
  , G.GenericGraph

    -- * Type-Level Utilities
  , NodeName
  , GetNodeKind
  , GetAnnotations
  , GetNeeds
  , GetSchema
  , GetUsesEffects
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
import qualified Tidepool.Graph.Memory as Mem
import qualified Tidepool.Graph.Template as Tpl
import qualified Tidepool.Graph.Docs as Docs
import qualified Tidepool.Graph.Generic as G

-- $memoryEffect
--
-- The 'Mem.Memory' effect provides typed persistent state for graph nodes.
-- Use the same effect type for both node-private and graph-level memory:
--
-- @
-- handler :: (Mem.Memory ExploreMem :> es, Mem.Memory SessionState :> es) => Eff es ()
-- handler = do
--   myMem <- Mem.getMem \@ExploreMem     -- Node's private state
--   global <- Mem.getMem \@SessionState  -- Graph's shared state
--
--   Mem.updateMem \@ExploreMem $ \\m -> m { visited = True }
--   Mem.modifyMem \@SessionState #counter (+ 1)
-- @
--
-- Note: The 'Memory' annotation (from "Tidepool.Graph.Types") declares which
-- memory type a node has. The 'Mem.Memory' effect is used at runtime to
-- access that memory. They share a name but have different kinds.

-- $templateDef
--
-- The 'Tpl.TemplateDef' typeclass defines typed templates for LLM nodes.
-- Templates combine a Jinja file (validated at compile time) with an
-- effectful context builder.
--
-- @
-- -- Phase 1: Compile template (TH validates against context type)
-- classifyCompiled :: TypedTemplate ClassifyContext SourcePos
-- classifyCompiled = $(typedTemplateFile ''ClassifyContext "templates/classify.jinja")
--
-- -- Phase 2: Define the template instance
-- data ClassifyTpl
--
-- instance TemplateDef ClassifyTpl where
--   type TemplateContext ClassifyTpl = ClassifyContext
--   type TemplateConstraint ClassifyTpl es = (State S :> es)
--
--   templateName = "classify"
--   templateDescription = "Classify user intent"
--   templateCompiled = classifyCompiled
--
--   buildContext = do
--     st <- get \@S
--     pure ClassifyContext { ... }
--
-- -- Usage:
-- prompt <- renderTemplate \@ClassifyTpl
-- @
--
-- For the full 'TemplateDef' typeclass, import "Tidepool.Graph.Template" directly.

-- $templateDocs
--
-- Generate documentation for templates showing their include hierarchy:
--
-- @
-- -- For a TemplateDef instance:
-- putStrLn $ templateDocBlock \@ClassifyTpl
--
-- -- Or render the dependency tree directly:
-- putStrLn $ renderDepTree (templateDepTree \@ClassifyTpl)
-- @
--
-- Example output:
--
-- @
-- templates/classify.jinja
--    ├─ partials/system.jinja
--    └─ partials/intent.jinja
-- @

-- $recordDSL
--
-- An alternative syntax uses mode-parameterized records, inspired by Servant's
-- @NamedRoutes@ pattern. This provides:
--
-- * __Field names as node names__ — No @:=@ annotation needed
-- * __Type-safe handler records__ — The @AsHandler@ mode computes handler types
-- * __Generic traversal__ — Validate and reify via @GHC.Generics@
--
-- @
-- data SupportGraph mode = SupportGraph
--   { sgEntry    :: mode :- G.Entry Message
--   , sgClassify :: mode :- G.LLMNode :@ Needs '[Message] :@ Template ClassifyTpl :@ Schema Intent
--   , sgRoute    :: mode :- G.LogicNode :@ Needs '[Intent] :@ UsesEffects '[Goto "refund", Goto "faq"]
--   , sgRefund   :: mode :- G.LLMNode :@ Needs '[Message] :@ Template RefundTpl :@ Schema Response
--   , sgFaq      :: mode :- G.LLMNode :@ Needs '[Message] :@ Template FaqTpl :@ Schema Response
--   , sgExit     :: mode :- G.Exit Response
--   }
--   deriving Generic
--
-- -- Validation uses the constraint:
-- validGraph :: G.ValidGraphRecord SupportGraph => ()
-- validGraph = ()
-- @
--
-- Key differences from list-based syntax:
--
-- * Use 'G.LLMNode' and 'G.LogicNode' instead of @LLM@ and @Logic@ (different kinds)
-- * Use 'G.Entry' and 'G.Exit' (parameterized) instead of @Entry :~>@ and @Exit :<~@
-- * Field names become node names automatically
-- * Use 'G.ValidGraphRecord' instead of 'ValidGraph' for validation
--
-- Note: Import @Tidepool.Graph.Generic@ qualified to access 'G.Entry' and 'G.Exit'
-- (they conflict with the unparameterized markers from @Tidepool.Graph.Types@).
