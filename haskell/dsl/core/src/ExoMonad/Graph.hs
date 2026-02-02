-- | Type-Safe Graph DSL for LLM Agent State Machines
--
-- This module provides the record-based (Servant-style) DSL for defining
-- state machine graphs that are validated at compile time and can generate
-- Mermaid diagrams.
--
-- This is the only supported graph syntax.
--
-- = Core Concepts
--
-- == Nodes
--
-- Nodes come in two kinds:
--
-- * __LLM nodes__: Call the language model, produce output via 'Schema'
-- * __Logic nodes__: Run pure or effect-based code, transition via 'Goto'
--
-- == Edges
--
-- Edges are derived automatically from:
--
-- * __Implicit edges__: When a node's 'Schema' output matches another
--   node's 'Input' type
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
-- = Example (Record-Based Syntax)
--
-- @
-- data SupportGraph mode = SupportGraph
--   { sgEntry    :: mode :- G.Entry Message
--   , sgClassify :: mode :- G.LLMNode :@ Input Message :@ Template ClassifyTpl :@ Schema Intent
--   , sgRoute    :: mode :- G.LogicNode :@ Input Intent :@ UsesEffects '[Goto "sgRefund", Goto "sgFaq"]
--   , sgRefund   :: mode :- G.LLMNode :@ Input Message :@ Template RefundTpl :@ Schema Response
--   , sgFaq      :: mode :- G.LLMNode :@ Input Message :@ Template FaqTpl :@ Schema Response
--   , sgExit     :: mode :- G.Exit Response
--   }
--   deriving Generic
-- @
--
-- = Validation
--
-- Graphs are validated at compile time:
--
-- * Must have exactly one Entry and one Exit field
-- * All 'Input' types must be satisfied by Entry or some 'Schema'
-- * All 'Goto' targets must reference valid field names
--
-- = Diagram Generation
--
-- Generate Mermaid diagrams from graph definitions using reification.
module ExoMonad.Graph
  ( -- * Node Kind
    NodeKind (..),

    -- * Annotations
    type (:@),
    Input,
    Schema,
    System,
    Template,
    Vision,
    Tools,
    UsesEffects,
    Memory,

    -- * Graph-Level Annotations
    type (:&),
    Groups,
    Requires,
    Global,
    Backend,

    -- * API Backend Types
    CloudflareAI,
    NativeAnthropic,

    -- * The Goto Effect
    Goto (..),
    goto,

    -- * The Memory Effect
    -- $memoryEffect
    Mem.getMem,
    Mem.updateMem,
    Mem.modifyMem,
    Mem.runMemory,
    Mem.evalMemory,
    -- For the Memory effect type itself, import ExoMonad.Graph.Memory directly

    -- * Template Definitions
    -- $templateDef
    Tpl.TemplateDef (..),
    Tpl.renderTemplate,
    Tpl.GingerContext,
    Tpl.TypedTemplate,
    Tpl.typedTemplateFile,
    Tpl.runTypedTemplate,
    Tpl.makeTemplateCompiled,

    -- ** Dependency Tracking
    Tpl.TemplateDependency (..),
    Tpl.DepRelation (..),
    Tpl.DepLocation (..),
    Tpl.TemplateContextInfo (..),
    Tpl.templateDependencyTree,
    Tpl.flattenDeps,

    -- * Template Documentation
    -- $templateDocs
    Docs.renderDepTree,
    Docs.renderDepTreeCompact,
    Docs.templateDocBlock,

    -- * Validation

    -- These constraints operate on graphs of kind @Type -> Type@,
    -- i.e. graphs defined with @mode@ parameter.
    AllNodesReachable,
    AllLogicNodesReachExit,
    NoDeadGotos,

    -- * Reification
    ReifyGraph (..),
    GraphInfo (..),
    NodeInfo (..),
    EdgeInfo (..),
    RuntimeNodeKind (..),
    RuntimeEdgeKind (..),

    -- ** Rich Node Info Types
    SchemaInfo (..),
    TemplateInfo (..),
    MemoryInfo (..),

    -- * JSON Export
    graphToExport,
    graphToJSON,
    GraphExport (..),
    NodeExport (..),
    EdgeExport (..),
    SchemaExport (..),
    TemplateExport (..),
    MemoryExport (..),

    -- * Record-Based Reification
    -- $recordReification
    ReifyRecordGraph (..),
    makeGraphInfo,

    -- * Mermaid Diagrams
    toMermaid,
    toMermaidWithConfig,
    MermaidConfig (..),
    defaultConfig,

    -- * Record-Based Graph DSL
    -- $recordDSL
    G.GraphMode (..),
    G.AsGraph,
    G.AsHandler,
    -- For G.Entry, G.Exit, use: import qualified ExoMonad.Graph.Generic as G
    G.LLMNode,
    G.LogicNode,
    G.ValidGraphRecord,
    G.NodeHandler,
    G.GraphProduct (..),
    G.FieldNamesOf,
    G.FieldsWithNamesOf,
    G.ElemC,
    G.gotoField,
    G.GenericGraph,

    -- * Annotation Extraction (used by record DSL)
    GetInput,
    GetSchema,
    GetUsesEffects,
    GetGotoTargets,
    GotosToTos,
    GetMemory,
    GetGlobal,
    GetBackend,

    -- * LLM Handler Variants
    LLMHandler (..),

    -- * Tools
    ToolDef (..),
    ValidTool,
    ValidToolList,
    AllToolsValid,
    ToolInfo (..),
    ReifyToolList (..),
    toolInfoToJSON,
    toolToInfo,
  )
where

import ExoMonad.Graph.Docs qualified as Docs
import ExoMonad.Graph.Edges
import ExoMonad.Graph.Export
import ExoMonad.Graph.Generic qualified as G
import ExoMonad.Graph.Goto
import ExoMonad.Graph.Memory qualified as Mem
import ExoMonad.Graph.Mermaid
import ExoMonad.Graph.Reify
import ExoMonad.Graph.Template qualified as Tpl
import ExoMonad.Graph.Tool
import ExoMonad.Graph.Types
import ExoMonad.Graph.Validate

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
-- Note: The 'Memory' annotation (from "ExoMonad.Graph.Types") declares which
-- memory type a node has. The 'Mem.Memory' effect is used at runtime to
-- access that memory. They share a name but have different kinds.

-- $templateDef
--
-- The 'Tpl.TemplateDef' typeclass defines typed templates for LLM nodes.
-- Templates combine a Jinja file (validated at compile time) with an
-- effect-based context builder.
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
-- For the full 'TemplateDef' typeclass, import "ExoMonad.Graph.Template" directly.

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
-- The Graph DSL uses mode-parameterized records, inspired by Servant's
-- @NamedRoutes@ pattern. This provides:
--
-- * __Field names as node names__ — No @:=@ annotation needed
-- * __Type-safe handler records__ — The @AsHandler@ mode computes handler types
-- * __Generic traversal__ — Validate and reify via @GHC.Generics@
--
-- @
-- data SupportGraph mode = SupportGraph
--   { sgEntry    :: mode :- G.Entry Message
--   , sgClassify :: mode :- G.LLMNode :@ Input Message :@ Template ClassifyTpl :@ Schema Intent
--   , sgRoute    :: mode :- G.LogicNode :@ Input Intent :@ UsesEffects '[Goto "refund", Goto "faq"]
--   , sgRefund   :: mode :- G.LLMNode :@ Input Message :@ Template RefundTpl :@ Schema Response
--   , sgFaq      :: mode :- G.LLMNode :@ Input Message :@ Template FaqTpl :@ Schema Response
--   , sgExit     :: mode :- G.Exit Response
--   }
--   deriving Generic
--
-- -- Validation uses the constraint:
-- validGraph :: G.ValidGraphRecord SupportGraph => ()
-- validGraph = ()
-- @
--
-- Key features:
--
-- * Use 'G.LLMNode' and 'G.LogicNode' for node types
-- * Use 'G.Entry' and 'G.Exit' for entry/exit points
-- * Field names become node names automatically
-- * Use 'G.ValidGraphRecord' for validation

-- $recordReification
--
-- Record-based graphs can be automatically reified to 'GraphInfo' for
-- Mermaid diagram generation:
--
-- @
-- instance ReifyRecordGraph SupportGraph where
--   reifyRecordGraph = makeGraphInfo
--
-- diagram :: Text
-- diagram = toMermaid (reifyRecordGraph (Proxy \@SupportGraph))
-- @
--
-- Note: Due to polykind limitations with @UsesEffects '[Effect]@, the
-- @niGotoTargets@ field is always empty. Goto transitions are validated
-- at compile-time but not represented at runtime. The generated Mermaid
-- diagram shows implicit edges (Entry/Schema → Input) only.
