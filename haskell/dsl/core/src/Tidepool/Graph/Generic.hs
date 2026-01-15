{-# LANGUAGE ConstraintKinds #-}

-- | Servant-style record-as-graph pattern for the Tidepool Graph DSL.
--
-- This module provides the infrastructure for defining graphs as records
-- parameterized by a mode type, following the pattern established by
-- Servant's NamedRoutes.
--
-- = Design Philosophy
--
-- "The record IS the graph. Modes determine interpretation."
--
-- Graphs are defined as mode-parameterized records:
--
-- @
-- data MyGraph mode = MyGraph
--   { entry    :: mode :- EntryNode Message
--   , classify :: mode :- LLM :@ Input Message :@ Schema Intent
--   , exit     :: mode :- ExitNode Response
--   }
--   deriving Generic
-- @
--
-- = Modes
--
-- * 'AsGraph' - Identity mode, returns node definitions as-is (for validation)
-- * 'AsHandler' - Computes handler types for each node
-- * 'AsDocs' - Produces documentation proxy types
--
-- = Servant Inspiration
--
-- This pattern mirrors Servant's GenericMode:
--
-- @
-- -- Servant
-- class GenericMode mode where
--   type mode :- api :: Type
--
-- -- Tidepool (this module)
-- class GraphMode mode where
--   type mode :- nodeDef :: Type
-- @
module Tidepool.Graph.Generic
  ( -- * Graph Mode Class
    GraphMode(..)

    -- * Modes
  , AsGraph
  , AsHandler

    -- * Node Handler Type Family
  , NodeHandler

    -- * Graph Product (Generic Traversal)
  , GraphProduct(..)

    -- * EntryNode/ExitNode Types
  , EntryNode
  , ExitNode

    -- * Node Kind Wrappers (for record DSL)
  , LLMNode
  , LogicNode
  , GraphNode
  , ForkNode
  , BarrierNode

    -- * Fork/Barrier Handler Types
  , SpawnPayloads
  , SpawnPayloadsInner
  , AwaitsHList

    -- * Field Name Extraction
  , FieldNames
  , FieldDefs
  , FieldsWithNames
  , FieldNamesOf
  , FieldsWithNamesOf

    -- * Node Definition Lookup
  , GetNodeDef
  , LookupField

    -- * Graph-Validated Goto
  , gotoField

    -- * Type-Level Utilities
  , Elem
  , ElemC
  , ElemCWithOptions
  , If
  , Append
  , type (||)
  , OrMaybe

    -- * Re-exports for LLM Handlers
  , LLMHandler(..)
  , ClaudeCodeLLMHandler(..)
  , ChooseLLMHandler

    -- * Record Validation
  , HasEntryField
  , HasExitField
  , CountEntries
  , CountExits
  , GetEntryType
  , GetExitType
  , ValidateEntryExit
  , ValidateGotoTargets
  , ValidGraphRecord

    -- * Convenience Constraints
  , GenericGraph
  ) where

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic(..), K1(..), M1(..), (:*:)(..), Meta(..), S, D, C)
import GHC.TypeLits (Symbol, KnownSymbol, TypeError, ErrorMessage(..), Nat, type (+))
import Tidepool.Graph.Errors
  ( HR, Blank, WhatHappened, HowItWorks, Fixes, Example
  , Indent, CodeLine, Bullet
  )

import Tidepool.Graph.Validate (FormatSymbolList)
import Control.Monad.Freer (Eff, Member)

import Tidepool.Graph.Types (type (:@), Input, Schema, Template, Vision, Tools, Memory, System, UsesEffects, ClaudeCode, ModelChoice, Spawn, Barrier, Awaits, HList(..))
import Tidepool.Graph.Template (TemplateContext)
import Tidepool.Graph.Edges (GetUsesEffects, GetGotoTargets, GotoEffectsToTargets, HasClaudeCode, GetClaudeCode, GetSpawnTargets, GetBarrierTarget, GetAwaits)
import Tidepool.Graph.Goto (Goto, goto, GotoChoice, To, LLMHandler(..), ClaudeCodeLLMHandler(..))
import Tidepool.Graph.Validate.RecordStructure
  ( AllFieldsReachable, AllLogicFieldsReachExit, NoDeadGotosRecord
  , AllLogicNodesHaveGoto, NoGotoSelfOnly
  )
import Tidepool.Graph.Validate.ForkBarrier (ValidateForkBarrierPairs)
import Tidepool.Graph.Generic.Core
  ( GraphMode(..)
  , AsGraph
  , LLMNode
  , LogicNode
  , GraphNode
  , ForkNode
  , BarrierNode
  , EntryNode
  , ExitNode
  )

-- | Effect type alias (freer-simple effects have kind Type -> Type).
type Effect = Type -> Type

-- ════════════════════════════════════════════════════════════════════════════
-- ASHANDLER MODE
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler mode - fields contain handler function types.
--
-- The @es@ parameter is the effect stack available to handlers.
--
-- @
-- handlers :: MyGraph (AsHandler '[LLM, Log])
-- handlers = MyGraph
--   { entry    = Proxy \@Message
--   , classify = \\msg -> do
--       log "Classifying..."
--       pure $ ClassifyContext msg.content
--   , exit     = Proxy \@Response
--   }
-- @
--
-- Handler types are computed by 'NodeHandler':
--
-- * LLM nodes: @Input -> Eff es (TemplateContext tpl)@
-- * Logic nodes: @Input -> Eff es (GotoChoice targets)@
-- * EntryNode\/Exit: @Proxy inputType@ / @Proxy outputType@
type AsHandler :: [Effect] -> Type
data AsHandler es

instance GraphMode (AsHandler es) where
  type (AsHandler es) :- nodeDef = NodeHandler nodeDef es

-- ════════════════════════════════════════════════════════════════════════════
-- NODE HANDLER TYPE FAMILY
-- ════════════════════════════════════════════════════════════════════════════

-- | Compute the handler type for a node definition.
--
-- This is the core type family that transforms node declarations into
-- handler function signatures.
--
-- Strategy: Peel annotations from the outside in, finding Input/Template/
-- Schema/UsesEffects. When we hit bare LLM or Logic, build the final
-- function type.
--
-- @
-- NodeHandler (LLM :@ Input Message :@ Template ClassifyTpl :@ Schema Intent) es
--   = Message -> Eff es ClassifyContext
--
-- NodeHandler (Logic :@ Input Intent :@ UsesEffects '[Goto "respond", GotoExit]) es
--   = Intent -> Eff es (GotoChoice targets)
--
-- NodeHandler (EntryNode Message) es = Proxy Message
-- NodeHandler (Exit Response) es = Proxy Response
-- @
type NodeHandler :: Type -> [Effect] -> Type
type family NodeHandler nodeDef es where
  -- EntryNode/Exit produce Proxy (self-documenting markers)
  NodeHandler (EntryNode a) es = Proxy a
  NodeHandler (ExitNode a) es = Proxy a

  -- GraphNode handler: runs child graph to completion.
  --
  -- The handler receives the graph's EntryNode type and returns its Exit type.
  -- GraphNode with Input annotation: function from input type to exit type.
  NodeHandler (GraphNode subgraph :@ Input inputT) es =
    inputT -> Eff es ()

  -- Any annotated node: dispatch to the appropriate accumulator based on base kind
  -- We peel from outside, so start with the full node
  -- Accumulator: (nodeDef, origNode, es, mInput, mTpl, mSchema, mEffs)
  NodeHandler (node :@ ann) es = NodeHandlerDispatch (node :@ ann) (node :@ ann) es 'Nothing 'Nothing 'Nothing 'Nothing

  -- Bare LLMNode/LogicNode without annotations - error
  NodeHandler (LLMNode _subtype) es = TypeError
    ( HR
      ':$$: 'Text "  LLMNode requires annotations"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "You wrote: LLMNode with no annotations"
      ':$$: Indent "The compiler can't determine what handler type to generate."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "LLM nodes call a language model. For this to work, we need:"
      ':$$: Blank
      ':$$: CodeLine "1. WHAT to send (Template) - your handler builds context, template renders it"
      ':$$: CodeLine "2. WHAT comes back (Schema) - the structured output type from the LLM"
      ':$$: Blank
      ':$$: Indent "The flow is:"
      ':$$: CodeLine "   Handler builds context -> Template renders prompt -> LLM responds -> Schema parses"
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add Template and Schema annotations:"
      ':$$: CodeLine "  myNode :: mode :- LLMNode :@ Input MyInput :@ Template MyTpl :@ Schema Output"
      ':$$: Blank
      ':$$: Example
      ':$$: CodeLine "-- In Context.hs (separate module for TH staging):"
      ':$$: CodeLine "data ClassifyContext = ClassifyContext { query :: Text }"
      ':$$: CodeLine ""
      ':$$: CodeLine "-- In your graph definition:"
      ':$$: CodeLine "classify :: mode :- LLMNode"
      ':$$: CodeLine "            :@ Input Message"
      ':$$: CodeLine "            :@ Template ClassifyTpl  -- references your context type"
      ':$$: CodeLine "            :@ Schema Intent         -- what the LLM returns"
    )
  NodeHandler LogicNode es = TypeError
    ( HR
      ':$$: 'Text "  LogicNode requires annotations"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "You wrote: LogicNode with no annotations"
      ':$$: Indent "The compiler can't determine what transitions this node can make."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "Logic nodes are pure decision points - they route to other nodes."
      ':$$: Indent "We need to know WHICH nodes they can transition to."
      ':$$: Blank
      ':$$: CodeLine "UsesEffects '[Goto \"nodeA\" Payload, Goto \"nodeB\" Payload, Goto Exit Result]"
      ':$$: CodeLine "              ^^^^^^^^^^^^^^^^      ^^^^^^^^^^^^^^^^       ^^^^^^^^^^^^^^^"
      ':$$: CodeLine "              These become the valid targets for gotoChoice/gotoExit"
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add UsesEffects with your transitions:"
      ':$$: CodeLine "  myRouter :: mode :- LogicNode"
      ':$$: CodeLine "               :@ Input Intent"
      ':$$: CodeLine "               :@ UsesEffects '[Goto \"process\" Data, Goto Exit Result]"
      ':$$: Blank
      ':$$: Example
      ':$$: CodeLine "-- Handler implementation:"
      ':$$: CodeLine "router :: Intent -> Eff es (GotoChoice '[To \"process\" Data, To Exit Result])"
      ':$$: CodeLine "router intent = case intent of"
      ':$$: CodeLine "  NeedsProcessing x -> pure $ gotoChoice @\"process\" x"
      ':$$: CodeLine "  Done result       -> pure $ gotoExit result"
    )
  NodeHandler ForkNode es = TypeError
    ( HR
      ':$$: 'Text "  ForkNode requires annotations"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "You wrote: ForkNode with no annotations"
      ':$$: Indent "The compiler can't determine what to spawn or where to collect results."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "Fork nodes spawn parallel execution paths. They need to know:"
      ':$$: CodeLine "  Input   -> What data to receive before spawning"
      ':$$: CodeLine "  Spawn   -> Which nodes to spawn and with what payloads"
      ':$$: CodeLine "  Barrier -> Where spawned paths deposit their results"
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add Input, Spawn, and Barrier annotations:"
      ':$$: CodeLine "  myFork :: mode :- ForkNode"
      ':$$: CodeLine "             :@ Input Task"
      ':$$: CodeLine "             :@ Spawn '[To \"worker1\" Task, To \"worker2\" Task]"
      ':$$: CodeLine "             :@ Barrier \"merge\""
    )
  NodeHandler BarrierNode es = TypeError
    ( HR
      ':$$: 'Text "  BarrierNode requires annotations"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "You wrote: BarrierNode with no annotations"
      ':$$: Indent "The compiler can't determine what results to collect or where to go next."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "Barrier nodes synchronize parallel paths. They need to know:"
      ':$$: CodeLine "  Awaits      -> What result types to collect from spawned paths"
      ':$$: CodeLine "  UsesEffects -> Where to route with the collected results"
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add Awaits and UsesEffects annotations:"
      ':$$: CodeLine "  myBarrier :: mode :- BarrierNode"
      ':$$: CodeLine "               :@ Awaits '[ResultA, ResultB]"
      ':$$: CodeLine "               :@ UsesEffects '[Goto Exit (ResultA, ResultB)]"
    )

-- | Unified accumulator that peels annotations and dispatches based on base kind.
--
-- = Strategy: Outside-In Annotation Peeling
--
-- Given a node like:
--
-- @
-- LLMNode :@ Input A :@ Template T :@ Schema S :@ Vision
-- @
--
-- The family processes annotations from left to right (outside-in), recording:
--
-- - Input type → @mInput@ (Maybe)
-- - Template type → @mTpl@ (Maybe)
-- - Schema type → @mSchema@ (Maybe)
-- - UsesEffects → @mEffs@ (Maybe)
--
-- When it reaches the bare @LLMNode@ or @LogicNode@, it has all the information
-- needed to construct the handler type.
--
-- = Parameters (7 total)
--
-- @
-- NodeHandlerDispatch nodeDef origNode es mInput mTpl mSchema mEffs
--                     ^        ^         ^  ^      ^    ^       ^
--                     |        |         |  |      |    |       |
--                     |        |         |  |      |    |       +-- Maybe UsesEffects (for routing)
--                     |        |         |  |      |    +---------- Maybe Schema type (LLM output)
--                     |        |         |  |      +--------------- Maybe Template type (LLM context)
--                     |        |         |  +---------------------- Maybe Input type (handler param)
--                     |        |         +------------------------- Effect stack from AsHandler
--                     |        +----------------------------------- Original node (for errors)
--                     +-------------------------------------------- Current node being processed
-- @
--
-- = Clause Groups
--
-- 1. Input recording (with duplicate detection)
-- 2. Template/Schema recording (with duplicate detection)
-- 3. Skipped annotations (Vision, Tools, Memory, System)
-- 4. UsesEffects recording (with duplicate detection)
-- 5. LLMNode terminal cases (3 handler variants)
-- 6. LogicNode terminal cases
--
-- = Why origNode is Preserved
--
-- The @origNode@ parameter never changes. It's the original full node definition
-- used in error messages to show the user what they wrote, even after annotations
-- have been peeled away.

-- | Choose between LLMHandler and ClaudeCodeLLMHandler based on GetClaudeCode.
--
-- When a node has the ClaudeCode annotation, we use ClaudeCodeLLMHandler with
-- the model from the annotation. The type parameter ensures compile-time
-- validation that the handler matches the annotation.
type ChooseLLMHandler :: Maybe ModelChoice -> Type -> Type -> [Type] -> [Effect] -> Type -> Type
type family ChooseLLMHandler mClaudeCode input schema targets effs tpl where
  ChooseLLMHandler ('Just model) input schema targets effs tpl =
    ClaudeCodeLLMHandler model input schema targets effs tpl
  ChooseLLMHandler 'Nothing input schema targets effs tpl =
    LLMHandler input schema targets effs tpl

type NodeHandlerDispatch :: Type -> Type -> [Effect] -> Maybe Type -> Maybe Type -> Maybe Type -> Maybe Type -> Type
type family NodeHandlerDispatch nodeDef origNode es mInput mTpl mSchema mEffs where
  -- Peel Input annotation - record it
  NodeHandlerDispatch (node :@ Input t) orig es 'Nothing mTpl mSchema mEffs =
    NodeHandlerDispatch node orig es ('Just t) mTpl mSchema mEffs

  -- Detect duplicate Input annotations
  NodeHandlerDispatch (node :@ Input _) orig es ('Just _) mTpl mSchema mEffs = TypeError
    ( HR
      ':$$: 'Text "  Duplicate Input annotation"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your node has multiple Input annotations."
      ':$$: Indent "Each node can only have one input type."
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Remove the duplicate Input annotation"
      ':$$: Bullet "Use a tuple or custom type to combine inputs: Input (A, B)"
    )

  -- Peel Template annotation - record it (for LLM nodes)
  NodeHandlerDispatch (node :@ Template tpl) orig es mInput 'Nothing mSchema mEffs =
    NodeHandlerDispatch node orig es mInput ('Just tpl) mSchema mEffs

  -- Detect duplicate Template annotations
  NodeHandlerDispatch (node :@ Template _) orig es mInput ('Just _) mSchema mEffs = TypeError
    ( HR
      ':$$: 'Text "  Duplicate Template annotation"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your node has multiple Template annotations."
      ':$$: Indent "Each LLM node can only have one prompt template."
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Remove the duplicate Template annotation"
      ':$$: Bullet "Combine templates if you need multiple prompts"
    )

  -- Peel Schema annotation - record it (for LLM nodes)
  NodeHandlerDispatch (node :@ Schema s) orig es mInput mTpl 'Nothing mEffs =
    NodeHandlerDispatch node orig es mInput mTpl ('Just s) mEffs

  -- Detect duplicate Schema annotations
  NodeHandlerDispatch (node :@ Schema _) orig es mInput mTpl ('Just _) mEffs = TypeError
    ( HR
      ':$$: 'Text "  Duplicate Schema annotation"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your node has multiple Schema annotations."
      ':$$: Indent "Each LLM node produces exactly one output type."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "Schema defines the structured output type the LLM returns."
      ':$$: Indent "Multiple Schema would be ambiguous - which type is the output?"
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Remove the duplicate Schema annotation"
      ':$$: Bullet "Use a sum type if you need multiple output shapes"
    )

  -- Skip other annotations (Vision, Tools, Memory, System, ClaudeCode)
  NodeHandlerDispatch (node :@ Vision) orig es mInput mTpl mSchema mEffs =
    NodeHandlerDispatch node orig es mInput mTpl mSchema mEffs
  NodeHandlerDispatch (node :@ Tools _) orig es mInput mTpl mSchema mEffs =
    NodeHandlerDispatch node orig es mInput mTpl mSchema mEffs
  NodeHandlerDispatch (node :@ Memory _) orig es mInput mTpl mSchema mEffs =
    NodeHandlerDispatch node orig es mInput mTpl mSchema mEffs
  NodeHandlerDispatch (node :@ System _) orig es mInput mTpl mSchema mEffs =
    NodeHandlerDispatch node orig es mInput mTpl mSchema mEffs
  NodeHandlerDispatch (node :@ ClaudeCode _) orig es mInput mTpl mSchema mEffs =
    NodeHandlerDispatch node orig es mInput mTpl mSchema mEffs

  -- Skip Fork/Barrier annotations (extracted directly in terminal cases)
  NodeHandlerDispatch (node :@ Spawn _) orig es mInput mTpl mSchema mEffs =
    NodeHandlerDispatch node orig es mInput mTpl mSchema mEffs
  NodeHandlerDispatch (node :@ Barrier _) orig es mInput mTpl mSchema mEffs =
    NodeHandlerDispatch node orig es mInput mTpl mSchema mEffs
  NodeHandlerDispatch (node :@ Awaits _) orig es mInput mTpl mSchema mEffs =
    NodeHandlerDispatch node orig es mInput mTpl mSchema mEffs

  -- Peel UsesEffects annotation - record effects
  -- (Validation that To is not used here happens at ValidGraphRecord level)
  NodeHandlerDispatch (node :@ UsesEffects effs) orig es mInput mTpl mSchema 'Nothing =
    NodeHandlerDispatch node orig es mInput mTpl mSchema ('Just (EffStack effs))

  -- Detect duplicate UsesEffects annotations
  NodeHandlerDispatch (node :@ UsesEffects _) orig es mInput mTpl mSchema ('Just _) = TypeError
    ( HR
      ':$$: 'Text "  Duplicate UsesEffects annotation"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your node has multiple UsesEffects annotations."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "UsesEffects declares the effect stack for this handler."
      ':$$: Indent "All effects should be listed in a single annotation."
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Combine into one: UsesEffects '[Effect1, Effect2, ...]"
    )

  -- ══════════════════════════════════════════════════════════════════════════
  -- LLMNode Base Cases
  -- ══════════════════════════════════════════════════════════════════════════
  --
  -- LLM handlers require Template + Schema + UsesEffects for full execution:
  --
  --    Handler: @LLMHandler input schema targets es tpl@
  --    Flow: Build context → Render template → LLM call → Route based on output
  --    Routing: Explicit via gotoChoice/gotoExit
  --
  -- ══════════════════════════════════════════════════════════════════════════

  -- LLMNode with Template only - missing UsesEffects for routing
  NodeHandlerDispatch (LLMNode _subtype) orig es mInput ('Just tpl) ('Just schema) 'Nothing = TypeError
    ( HR
      ':$$: 'Text "  LLM node missing routing: has Template but no UsesEffects"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your LLM node has Template and Schema but no UsesEffects."
      ':$$: Indent "We don't know where to route after the LLM call."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "LLM nodes need all three annotations:"
      ':$$: CodeLine "  Template    -> Context type for prompt rendering"
      ':$$: CodeLine "  Schema      -> Output type from LLM"
      ':$$: CodeLine "  UsesEffects -> Where to route (Goto targets)"
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add UsesEffects to specify routing:"
      ':$$: CodeLine "  myNode :: mode :- LLMNode"
      ':$$: CodeLine "             :@ Input MyInput"
      ':$$: CodeLine "             :@ Template MyTpl"
      ':$$: CodeLine "             :@ Schema " ':<>: 'ShowType schema
      ':$$: CodeLine "             :@ UsesEffects '[Goto Exit Result]"
    )

  -- LLMNode with Template AND UsesEffects: the complete form
  -- Uses ChooseLLMHandler to dispatch between regular LLM and ClaudeCode execution.
  -- GetClaudeCode extracts the model from the annotation for compile-time validation.
  NodeHandlerDispatch (LLMNode _subtype) orig es ('Just input) ('Just tpl) ('Just schema) ('Just (EffStack effs)) =
    ChooseLLMHandler (GetClaudeCode orig) input schema (GotoEffectsToTargets effs) es (TemplateContext tpl)

  -- LLMNode with UsesEffects but no Template - missing context for prompts
  NodeHandlerDispatch (LLMNode _subtype) orig es mInput 'Nothing ('Just schema) ('Just (EffStack effs)) = TypeError
    ( HR
      ':$$: 'Text "  LLM node missing Template: has UsesEffects but no context for prompts"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your LLM node has Schema and UsesEffects but no Template."
      ':$$: Indent "We don't know what context type to use for prompt rendering."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "LLM nodes need all three annotations:"
      ':$$: CodeLine "  Template    -> Context type for prompt rendering"
      ':$$: CodeLine "  Schema      -> Output type from LLM"
      ':$$: CodeLine "  UsesEffects -> Where to route (Goto targets)"
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add Template to specify the prompt context:"
      ':$$: CodeLine "  myNode :: mode :- LLMNode"
      ':$$: CodeLine "             :@ Input MyInput"
      ':$$: CodeLine "             :@ Template MyTpl"
      ':$$: CodeLine "             :@ Schema " ':<>: 'ShowType schema
      ':$$: CodeLine "             :@ UsesEffects '[...]"
    )

  -- LLMNode with Schema only (no Template or UsesEffects) - error
  NodeHandlerDispatch (LLMNode _subtype) orig es _ 'Nothing ('Just schema) 'Nothing = TypeError
    ( HR
      ':$$: 'Text "  LLM node incomplete: has Schema but missing Template and UsesEffects"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your LLM node has:"
      ':$$: CodeLine "Schema " ':<>: 'ShowType schema ':<>: 'Text "  -- what the LLM returns"
      ':$$: Blank
      ':$$: Indent "But we don't know:"
      ':$$: Bullet "What prompt to send (no Template)"
      ':$$: Bullet "Where to go next (no UsesEffects routing)"
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "LLM nodes need all three annotations:"
      ':$$: CodeLine "  Template    -> Context type for prompt rendering"
      ':$$: CodeLine "  Schema      -> Output type from LLM"
      ':$$: CodeLine "  UsesEffects -> Where to route (Goto targets)"
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add Template and UsesEffects:"
      ':$$: CodeLine "  myNode :: mode :- LLMNode"
      ':$$: CodeLine "             :@ Input MyInput"
      ':$$: CodeLine "             :@ Template MyTpl"
      ':$$: CodeLine "             :@ Schema " ':<>: 'ShowType schema
      ':$$: CodeLine "             :@ UsesEffects '[Goto Exit Result]"
    )

  -- LLMNode missing both Template and Schema - error
  NodeHandlerDispatch (LLMNode _subtype) orig es _ 'Nothing 'Nothing _ = TypeError
    ( HR
      ':$$: 'Text "  LLM node missing required annotations"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your LLM node has neither Template nor Schema."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "LLM nodes need all three annotations:"
      ':$$: CodeLine "  Template    -> Context type for prompt rendering"
      ':$$: CodeLine "  Schema      -> Output type from LLM"
      ':$$: CodeLine "  UsesEffects -> Where to route (Goto targets)"
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add Template, Schema, and UsesEffects:"
      ':$$: CodeLine "  myNode :: mode :- LLMNode"
      ':$$: CodeLine "             :@ Input MyInput"
      ':$$: CodeLine "             :@ Template MyContextTpl"
      ':$$: CodeLine "             :@ Schema MyOutputType"
      ':$$: CodeLine "             :@ UsesEffects '[Goto Exit Result]"
    )

  -- LLMNode missing Schema - error
  NodeHandlerDispatch (LLMNode _subtype) orig es _ _ 'Nothing _ = TypeError
    ( HR
      ':$$: 'Text "  LLM node missing Schema annotation"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your LLM node has no Schema annotation."
      ':$$: Indent "We don't know what type the LLM should return."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "Schema specifies the structured output format for the LLM call."
      ':$$: Indent "It must be a type with:"
      ':$$: Bullet "Aeson FromJSON instance (to parse LLM response)"
      ':$$: Bullet "HasJSONSchema instance (to generate JSON Schema for the LLM)"
      ':$$: Blank
      ':$$: CodeLine "-- Example output type:"
      ':$$: CodeLine "data Intent = IntentRefund | IntentQuestion | IntentOther"
      ':$$: CodeLine "  deriving (Generic, FromJSON)"
      ':$$: CodeLine ""
      ':$$: CodeLine "-- In node definition:"
      ':$$: CodeLine "classify :: mode :- LLMNode :@ Template T :@ Schema Intent"
      ':$$: CodeLine "                                              ^^^^^^^^^^^^"
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add a Schema annotation:"
      ':$$: CodeLine "  myNode :: mode :- LLMNode :@ ... :@ Schema YourOutputType"
    )

  -- ══════════════════════════════════════════════════════════════════════════
  -- LogicNode Base Cases
  -- ══════════════════════════════════════════════════════════════════════════

  -- LogicNode with Input and UsesEffects: returns GotoChoice
  NodeHandlerDispatch LogicNode orig es ('Just input) _ _ ('Just (EffStack effs)) =
    input -> Eff es (GotoChoice (GotoEffectsToTargets effs))

  -- LogicNode without UsesEffects - error
  NodeHandlerDispatch LogicNode orig es _ _ _ 'Nothing = TypeError
    ( HR
      ':$$: 'Text "  Logic node missing UsesEffects annotation"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your LogicNode has no UsesEffects annotation."
      ':$$: Indent "We don't know what transitions this node can make."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "Logic nodes are routing points in your graph. They receive input,"
      ':$$: Indent "make a decision, and transition to another node (or Exit)."
      ':$$: Blank
      ':$$: Indent "The UsesEffects annotation declares ALL possible destinations:"
      ':$$: Blank
      ':$$: CodeLine "UsesEffects '[Goto \"nodeA\" PayloadA, Goto \"nodeB\" PayloadB, Goto Exit Result]"
      ':$$: CodeLine "              ^^^^^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^"
      ':$$: CodeLine "              Can go to nodeA       Can go to nodeB       Can exit graph"
      ':$$: Blank
      ':$$: Indent "Your handler then uses gotoChoice or gotoExit to pick one:"
      ':$$: Blank
      ':$$: CodeLine "router intent = case intent of"
      ':$$: CodeLine "  CaseA x -> pure $ gotoChoice @\"nodeA\" x"
      ':$$: CodeLine "  CaseB y -> pure $ gotoChoice @\"nodeB\" y"
      ':$$: CodeLine "  Done r  -> pure $ gotoExit r"
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add UsesEffects with your transitions:"
      ':$$: CodeLine "  myRouter :: mode :- LogicNode"
      ':$$: CodeLine "               :@ Input YourInput"
      ':$$: CodeLine "               :@ UsesEffects '[Goto \"target\" Payload, Goto Exit Result]"
    )

  -- ══════════════════════════════════════════════════════════════════════════
  -- ForkNode Base Cases
  -- ══════════════════════════════════════════════════════════════════════════
  --
  -- ForkNode handlers receive input and return spawn payloads as an HList.
  -- The interpreter spawns parallel paths for each target.
  --
  -- Handler: @input -> Eff es (SpawnPayloads targets)@
  --
  -- ══════════════════════════════════════════════════════════════════════════

  -- ForkNode with Input: extracts Spawn targets from original node
  -- Handler returns HList of payloads for spawned paths
  NodeHandlerDispatch ForkNode orig es ('Just input) _ _ _ =
    input -> Eff es (SpawnPayloads (GetSpawnTargets orig))

  -- ForkNode without Input - error
  NodeHandlerDispatch ForkNode orig es 'Nothing _ _ _ = TypeError
    ( HR
      ':$$: 'Text "  ForkNode missing Input annotation"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your ForkNode has no Input annotation."
      ':$$: Indent "We don't know what type to receive before spawning parallel paths."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "ForkNode receives input, then spawns parallel execution paths."
      ':$$: Indent "Each spawned path runs independently until it calls Arrive."
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add Input, Spawn, and Barrier annotations:"
      ':$$: CodeLine "  myFork :: mode :- ForkNode"
      ':$$: CodeLine "             :@ Input Task"
      ':$$: CodeLine "             :@ Spawn '[To \"worker1\" Task, To \"worker2\" Task]"
      ':$$: CodeLine "             :@ Barrier \"merge\""
    )

  -- ══════════════════════════════════════════════════════════════════════════
  -- BarrierNode Base Cases
  -- ══════════════════════════════════════════════════════════════════════════
  --
  -- BarrierNode handlers receive collected results and decide where to go.
  --
  -- Handler: @AwaitsHList awaits -> Eff es (GotoChoice targets)@
  --
  -- ══════════════════════════════════════════════════════════════════════════

  -- BarrierNode with UsesEffects: extracts Awaits types from original node
  -- Handler receives collected results as HList, returns GotoChoice
  NodeHandlerDispatch BarrierNode orig es _ _ _ ('Just (EffStack effs)) =
    AwaitsHList (GetAwaits orig) -> Eff es (GotoChoice (GotoEffectsToTargets effs))

  -- BarrierNode without UsesEffects - error
  NodeHandlerDispatch BarrierNode orig es _ _ _ 'Nothing = TypeError
    ( HR
      ':$$: 'Text "  BarrierNode missing UsesEffects annotation"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your BarrierNode has no UsesEffects annotation."
      ':$$: Indent "We don't know where to route after collecting results."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "BarrierNode collects results from spawned paths, then continues."
      ':$$: Indent "UsesEffects declares where to go with the collected results."
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add Awaits and UsesEffects annotations:"
      ':$$: CodeLine "  myBarrier :: mode :- BarrierNode"
      ':$$: CodeLine "               :@ Awaits '[ResultA, ResultB]"
      ':$$: CodeLine "               :@ UsesEffects '[Goto Exit (ResultA, ResultB)]"
    )

-- | Wrapper to distinguish Template types from EffStack in the Maybe
data EffStack (effs :: [Effect])

-- | Wrapper to distinguish Spawn targets in the accumulator
data SpawnTargets (targets :: [Type])

-- | Wrapper to distinguish Barrier target in the accumulator
data BarrierTarget (target :: Symbol)

-- | Wrapper to distinguish Awaits types in the accumulator
data AwaitsTypes (types :: [Type])

-- ════════════════════════════════════════════════════════════════════════════
-- FORK/BARRIER TYPE FAMILIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract payload types from spawn targets into an HList.
--
-- ForkNode handlers return an HList of payloads, one for each spawn target.
-- Using HList enables recursive type class instances for parallel dispatch.
--
-- @
-- SpawnPayloads '[To "w1" A, To "w2" B]
--   = HList '[A, B]
--
-- -- Handler returns:
-- forkHandler :: Task -> Eff es (HList '[TaskA, TaskB])
-- forkHandler task = pure (taskA ::: taskB ::: HNil)
-- @
type SpawnPayloads :: [Type] -> Type
type family SpawnPayloads targets where
  SpawnPayloads '[] = HList '[]
  SpawnPayloads (To _ a ': rest) = HList (a ': SpawnPayloadsInner rest)

-- | Helper to extract inner payload types (returns type-level list, not HList).
type SpawnPayloadsInner :: [Type] -> [Type]
type family SpawnPayloadsInner targets where
  SpawnPayloadsInner '[] = '[]
  SpawnPayloadsInner (To _ a ': rest) = a ': SpawnPayloadsInner rest

-- | Convert awaited types to an HList for BarrierNode handlers.
--
-- BarrierNode handlers receive collected results as an HList.
-- Using HList enables recursive type class instances for collecting arrivals.
--
-- @
-- AwaitsHList '[ResultA, ResultB]
--   = HList '[ResultA, ResultB]
--
-- -- Handler receives:
-- barrierHandler :: HList '[ResultA, ResultB] -> Eff es (GotoChoice targets)
-- barrierHandler (ra ::: rb ::: HNil) = ...
-- @
type AwaitsHList :: [Type] -> Type
type family AwaitsHList ts where
  AwaitsHList ts = HList ts

-- | Validate that UsesEffects doesn't contain 'To' markers.
--
-- 'To' markers are for 'GotoChoice' return types in handler signatures,
-- not for 'UsesEffects' in graph definitions. This validation catches
-- the common mistake of using the wrong type constructor.
--
-- = Example Error Scenario
--
-- @
-- -- Wrong: Using 'To' markers in UsesEffects
-- type MyEffects = '[To "nodeA" Data, To Exit Result]
--
-- myNode :: mode :- LogicNode :@ Input SomeType :@ UsesEffects MyEffects
--          -- ERROR: UsesEffects needs 'Goto' effects, not 'To' markers
--
-- -- Correct: Using 'Goto' effects in UsesEffects
-- type MyEffects = '[Goto "nodeA" Data, Goto Exit Result]
--
-- myNode :: mode :- LogicNode :@ Input SomeInput :@ UsesEffects MyEffects
--          -- OK: Goto effects are correct for UsesEffects
-- @
--
-- = Why This Distinction Exists
--
-- 'Goto' is an Effect (kind @(Type -> Type) -> Type -> Type@) that represents
-- a possible transition. 'To' is a phantom marker (kind @k -> Type -> Type@)
-- that appears in 'GotoChoice' to indicate which target was selected.
--
-- The 'GotosToTos' type family converts between them:
--
-- @
-- type MyGotos = '[Goto "a" A, Goto Exit R]           -- For UsesEffects
-- type MyTargets = GotosToTos MyGotos                 -- '[To "a" A, To Exit R] for GotoChoice
-- @
type ValidateNotToMarkers :: forall k. [k] -> Constraint
type family ValidateNotToMarkers effs where
  ValidateNotToMarkers '[] = ()
  ValidateNotToMarkers (To name payload ': _) = TypeError
    ( HR
      ':$$: 'Text "  Wrong type in UsesEffects"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "UsesEffects contains 'To' markers, but needs 'Goto' effects."
      ':$$: Blank
      ':$$: CodeLine "UsesEffects '[To \"x\" A, ...]   -- WRONG (To is for GotoChoice)"
      ':$$: CodeLine "UsesEffects '[Goto \"x\" A, ...]  -- CORRECT (Goto is the effect)"
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "'Goto' is the effect type used in graph definitions (UsesEffects)."
      ':$$: Indent "'To' is the target marker used in handler return types (GotoChoice)."
      ':$$: Blank
      ':$$: Indent "They look similar but have different kinds and purposes:"
      ':$$: CodeLine "  Goto :: k -> Type -> Effect   -- for declaring transitions"
      ':$$: CodeLine "  To   :: k -> Type -> Type     -- for selecting transitions"
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Change 'To' to 'Goto' in your UsesEffects list"
      ':$$: Blank
      ':$$: Example
      ':$$: CodeLine "-- If you have:"
      ':$$: CodeLine "type MyEffects = '[To \"process\" Data, To Exit Result]"
      ':$$: CodeLine ""
      ':$$: CodeLine "-- Change to:"
      ':$$: CodeLine "type MyEffects = '[Goto \"process\" Data, Goto Exit Result]"
    )
  ValidateNotToMarkers (_ ': rest) = ValidateNotToMarkers rest

-- ════════════════════════════════════════════════════════════════════════════
-- TYPE-LEVEL UTILITIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Append two type-level lists.
type Append :: [k] -> [k] -> [k]
type family Append xs ys where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

-- | Check if a Symbol is in a type-level list (returns Bool).
type Elem :: Symbol -> [Symbol] -> Bool
type family Elem x xs where
  Elem _ '[] = 'False
  Elem x (x ': _) = 'True
  Elem x (_ ': xs) = Elem x xs

-- | Constraint-level membership check with type error on failure.
--
-- Use this to validate that a field name exists in a graph:
--
-- @
-- gotoField
--   :: forall graph (name :: Symbol) payload es.
--      ( ElemC name (FieldNamesOf graph)
--      , Goto name payload :> es
--      )
--   => payload -> Eff es ()
-- @
type ElemC :: Symbol -> [Symbol] -> Constraint
type family ElemC s ss where
  ElemC s '[] = TypeError
    ( HR
      ':$$: 'Text "  Field \"" ':<>: 'Text s ':<>: 'Text "\" not found"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "You referenced a field that doesn't exist in this graph."
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Check the field name spelling"
      ':$$: Bullet "Ensure you're using the correct graph type"
    )
  ElemC s (s ': _) = ()
  ElemC s (_ ': rest) = ElemC s rest

-- | Enhanced membership check that shows available options on failure.
--
-- This variant takes the full list to display in error messages.
type ElemCWithOptions :: Symbol -> [Symbol] -> [Symbol] -> Constraint
type family ElemCWithOptions s ss allOptions where
  ElemCWithOptions s '[] allOptions = TypeError
    ( HR
      ':$$: 'Text "  Field \"" ':<>: 'Text s ':<>: 'Text "\" not found"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "You referenced a field that doesn't exist in this graph."
      ':$$: Blank
      ':$$: Indent "Available fields:"
      ':$$: FormatSymbolList allOptions
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Check the field name spelling"
      ':$$: Bullet "Use one of the available fields listed above"
    )
  ElemCWithOptions s (s ': _) _ = ()
  ElemCWithOptions s (_ ': rest) allOptions = ElemCWithOptions s rest allOptions

-- | Type-level If (returns Constraint).
type If :: Bool -> Constraint -> Constraint -> Constraint
type family If cond t f where
  If 'True  t _ = t
  If 'False _ f = f

-- ════════════════════════════════════════════════════════════════════════════
-- FIELD NAME EXTRACTION (from Generic)
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract field names as type-level Symbols from a Generic representation.
--
-- This is the key insight: GHC.Generics' 'MetaSel' contains field names
-- as type-level @Maybe Symbol@. For records with named fields, this is
-- @'Just fieldName@.
--
-- @
-- data MyGraph mode = MyGraph
--   { entry    :: mode :- EntryNode Message
--   , classify :: mode :- LLM :@ ...
--   }
--
-- FieldNames (Rep (MyGraph AsGraph)) = '["entry", "classify"]
-- @
type FieldNames :: (Type -> Type) -> [Symbol]
type family FieldNames f where
  FieldNames (M1 D _ f) = FieldNames f                              -- Datatype wrapper
  FieldNames (M1 C _ f) = FieldNames f                              -- Constructor wrapper
  FieldNames (M1 S ('MetaSel ('Just name) _ _ _) _) = '[name]       -- Named field!
  FieldNames (M1 S ('MetaSel 'Nothing _ _ _) _) = '[]               -- Unnamed (positional)
  FieldNames (l :*: r) = Append (FieldNames l) (FieldNames r)       -- Product
  FieldNames (K1 _ _) = '[]                                          -- Leaf value (no name)

-- | Extract node definitions from each field.
--
-- For AsGraph mode, the field type IS the node definition directly
-- (since @AsGraph :- def = def@).
--
-- Note: Use this on @Rep (graph AsGraph)@.
type FieldDefs :: (Type -> Type) -> [Type]
type family FieldDefs f where
  FieldDefs (M1 D _ f) = FieldDefs f
  FieldDefs (M1 C _ f) = FieldDefs f
  FieldDefs (M1 S _ (K1 _ def)) = '[def]                             -- Field value = node def
  FieldDefs (l :*: r) = Append (FieldDefs l) (FieldDefs r)

-- | Pair field names with their node definitions.
--
-- For AsGraph mode, the field type IS the node definition.
--
-- @
-- FieldsWithNames (Rep (MyGraph AsGraph))
--   = '[ '("entry", EntryNode Message)
--      , '("classify", LLM :@ Input Message :@ Schema Intent)
--      ]
-- @
type FieldsWithNames :: (Type -> Type) -> [(Symbol, Type)]
type family FieldsWithNames f where
  FieldsWithNames (M1 D _ f) = FieldsWithNames f
  FieldsWithNames (M1 C _ f) = FieldsWithNames f
  FieldsWithNames (M1 S ('MetaSel ('Just name) _ _ _) (K1 _ def)) = '[ '(name, def) ]
  FieldsWithNames (M1 S ('MetaSel 'Nothing _ _ _) _) = '[]
  FieldsWithNames (l :*: r) = Append (FieldsWithNames l) (FieldsWithNames r)
  FieldsWithNames _ = '[]

-- | Get field names from a graph type.
--
-- @
-- type MyFieldNames = FieldNamesOf SupportGraph
-- -- = '["sgEntry", "sgClassify", "sgRoute", ...]
-- @
type FieldNamesOf :: (Type -> Type) -> [Symbol]
type FieldNamesOf graph = FieldNames (Rep (graph AsGraph))

-- | Get fields with names from a graph type.
type FieldsWithNamesOf :: (Type -> Type) -> [(Symbol, Type)]
type FieldsWithNamesOf graph = FieldsWithNames (Rep (graph AsGraph))


-- ════════════════════════════════════════════════════════════════════════════
-- NODE DEFINITION LOOKUP
-- ════════════════════════════════════════════════════════════════════════════

-- | Get the node definition for a named field in a graph.
--
-- This enables type-level dispatch based on node kind. For example:
--
-- @
-- IsForkNode (GetNodeDef "myFork" MyGraph) ~ 'True
-- GetSpawnTargets (GetNodeDef "myFork" MyGraph) ~ '[To "w1" A, To "w2" B]
-- @
--
-- Used by ForkNode-specific DispatchGoto instances to detect when a named
-- target is a ForkNode and handle it specially.
type GetNodeDef :: Symbol -> (Type -> Type) -> Type
type GetNodeDef name graph = LookupField name (FieldsWithNamesOf graph)

-- | Look up a field definition by name in a list of (name, def) pairs.
--
-- Returns the definition type if found. Behavior is undefined if not found
-- (relies on HasField constraint to ensure field exists).
type LookupField :: Symbol -> [(Symbol, Type)] -> Type
type family LookupField name fields where
  LookupField name ('(name, def) ': _) = def
  LookupField name (_ ': rest) = LookupField name rest


-- ════════════════════════════════════════════════════════════════════════════
-- RECORD VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if the Generic representation contains an EntryNode field.
--
-- Returns 'True if any field has type @EntryNode a@ for some @a@.
type HasEntryField :: (Type -> Type) -> Bool
type family HasEntryField f where
  HasEntryField (M1 D _ f) = HasEntryField f
  HasEntryField (M1 C _ f) = HasEntryField f
  HasEntryField (M1 S _ (K1 _ (EntryNode _))) = 'True
  HasEntryField (M1 S _ _) = 'False
  HasEntryField (l :*: r) = HasEntryField l || HasEntryField r
  HasEntryField _ = 'False

-- | Check if the Generic representation contains an Exit field.
type HasExitField :: (Type -> Type) -> Bool
type family HasExitField f where
  HasExitField (M1 D _ f) = HasExitField f
  HasExitField (M1 C _ f) = HasExitField f
  HasExitField (M1 S _ (K1 _ (ExitNode _))) = 'True
  HasExitField (M1 S _ _) = 'False
  HasExitField (l :*: r) = HasExitField l || HasExitField r
  HasExitField _ = 'False

-- | Type-level Or for Bool.
type (||) :: Bool -> Bool -> Bool
type family a || b where
  'True  || _ = 'True
  'False || b = b

-- | Count EntryNode fields in a Generic representation.
--
-- Returns the number of fields with type @EntryNode a@ for some @a@.
type CountEntries :: (Type -> Type) -> Nat
type family CountEntries f where
  CountEntries (M1 D _ f) = CountEntries f
  CountEntries (M1 C _ f) = CountEntries f
  CountEntries (M1 S _ (K1 _ (EntryNode _))) = 1
  CountEntries (M1 S _ _) = 0
  CountEntries (l :*: r) = CountEntries l + CountEntries r
  CountEntries _ = 0

-- | Count Exit fields in a Generic representation.
--
-- Returns the number of fields with type @Exit a@ for some @a@.
type CountExits :: (Type -> Type) -> Nat
type family CountExits f where
  CountExits (M1 D _ f) = CountExits f
  CountExits (M1 C _ f) = CountExits f
  CountExits (M1 S _ (K1 _ (ExitNode _))) = 1
  CountExits (M1 S _ _) = 0
  CountExits (l :*: r) = CountExits l + CountExits r
  CountExits _ = 0

-- | Extract EntryNode type from a graph record.
type GetEntryType :: (Type -> Type) -> Maybe Type
type family GetEntryType f where
  GetEntryType (M1 D _ f) = GetEntryType f
  GetEntryType (M1 C _ f) = GetEntryType f
  GetEntryType (M1 S _ (K1 _ (EntryNode a))) = 'Just a
  GetEntryType (M1 S _ _) = 'Nothing
  GetEntryType (l :*: r) = OrMaybe (GetEntryType l) (GetEntryType r)
  GetEntryType _ = 'Nothing

-- | Extract Exit type from a graph record.
type GetExitType :: (Type -> Type) -> Maybe Type
type family GetExitType f where
  GetExitType (M1 D _ f) = GetExitType f
  GetExitType (M1 C _ f) = GetExitType f
  GetExitType (M1 S _ (K1 _ (ExitNode a))) = 'Just a
  GetExitType (M1 S _ _) = 'Nothing
  GetExitType (l :*: r) = OrMaybe (GetExitType l) (GetExitType r)
  GetExitType _ = 'Nothing

-- | Return first Just, or Nothing if both Nothing.
type OrMaybe :: Maybe k -> Maybe k -> Maybe k
type family OrMaybe a b where
  OrMaybe ('Just x) _ = 'Just x
  OrMaybe 'Nothing b = b

-- | Validate a graph record has exactly one EntryNode and one Exit field.
--
-- Produces type errors if EntryNode or Exit are missing, or if there are duplicates.
type ValidateEntryExit :: (Type -> Type) -> Constraint
type family ValidateEntryExit graph where
  ValidateEntryExit graph =
    ( ValidateEntryCount (CountEntries (Rep (graph AsGraph)))
    , ValidateExitCount (CountExits (Rep (graph AsGraph)))
    )

-- | Validate EntryNode count is exactly 1.
type ValidateEntryCount :: Nat -> Constraint
type family ValidateEntryCount n where
  ValidateEntryCount 0 = DelayedTypeError
    ( HR
      ':$$: 'Text "  Missing EntryNode field"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your graph record has no EntryNode field."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "Every graph needs exactly one entry point that defines"
      ':$$: Indent "what type of input the graph accepts."
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add an entry field:"
      ':$$: CodeLine "entry :: mode :- EntryNode YourInputType"
    )
  ValidateEntryCount 1 = ()
  ValidateEntryCount _ = DelayedTypeError
    ( HR
      ':$$: 'Text "  Multiple EntryNode fields"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your graph record has more than one EntryNode field."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "A graph can only have one entry point. Multiple entries"
      ':$$: Indent "would be ambiguous - which one starts the graph?"
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Remove duplicate EntryNode fields, keeping just one"
    )

-- | Validate Exit count is exactly 1.
type ValidateExitCount :: Nat -> Constraint
type family ValidateExitCount n where
  ValidateExitCount 0 = DelayedTypeError
    ( HR
      ':$$: 'Text "  Missing Exit field"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your graph record has no Exit field."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "Every graph needs exactly one exit point that defines"
      ':$$: Indent "what type of result the graph produces."
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add an exit field:"
      ':$$: CodeLine "exit :: mode :- ExitNode YourResultType"
    )
  ValidateExitCount 1 = ()
  ValidateExitCount _ = DelayedTypeError
    ( HR
      ':$$: 'Text "  Multiple Exit fields"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your graph record has more than one Exit field."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "A graph can only have one exit point. Multiple exits"
      ':$$: Indent "would be ambiguous - which defines the output type?"
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Remove duplicate Exit fields, keeping just one"
      ':$$: Bullet "Use a sum type if you need multiple result shapes"
    )

-- | Helper to delay TypeError evaluation.
type DelayedTypeError :: ErrorMessage -> Constraint
type family DelayedTypeError msg where
  DelayedTypeError msg = TypeError msg

-- ════════════════════════════════════════════════════════════════════════════
-- GOTO TARGET VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Collect all Goto targets from all fields in a graph.
--
-- For each field, extracts its UsesEffects (if any), then GetGotoTargets
-- from that effect list.
--
-- @
-- AllGotoTargetsFrom '[ '("route", Logic :@ UsesEffects '[Goto "foo" A, Goto "bar" B]) ]
--   = '[ '("foo", A), '("bar", B) ]
-- @
type AllGotoTargetsFrom :: [(Symbol, Type)] -> [(Symbol, Type)]
type family AllGotoTargetsFrom fields where
  AllGotoTargetsFrom '[] = '[]
  AllGotoTargetsFrom ('(name, def) ': rest) =
    Append (GotoTargetsFromDef def) (AllGotoTargetsFrom rest)

-- | Extract Goto targets from a single node definition.
--
-- Note: We fix the kind to Effect explicitly via @Effect to avoid ambiguous
-- type inference. Without this kind application, GHC cannot determine which
-- kind to use for the polykinded GetUsesEffects family when the result isn't
-- immediately constrained. UsesEffects annotations contain Effect types
-- (kind: (Type -> Type) -> Type -> Type).
type GotoTargetsFromDef :: Type -> [(Symbol, Type)]
type family GotoTargetsFromDef def where
  GotoTargetsFromDef def = GotoTargetsFromEffects (GetUsesEffects @Effect def)

-- | Extract Goto targets from Maybe effect list.
type GotoTargetsFromEffects :: Maybe [Effect] -> [(Symbol, Type)]
type family GotoTargetsFromEffects mEffs where
  GotoTargetsFromEffects 'Nothing = '[]
  GotoTargetsFromEffects ('Just effs) = GetGotoTargets @Effect effs

-- | Validate all Goto targets exist as field names.
--
-- For each target name in the Goto effects, check that it exists in the
-- list of field names. Produces type error for invalid targets.
type ValidateGotoTargets :: (Type -> Type) -> Constraint
type family ValidateGotoTargets graph where
  ValidateGotoTargets graph =
    ValidateGotoTargetsList
      (FieldNamesOf graph)
      (AllGotoTargetsFrom (FieldsWithNamesOf graph))

-- | Validate each Goto target in the list.
type ValidateGotoTargetsList :: [Symbol] -> [(Symbol, Type)] -> Constraint
type family ValidateGotoTargetsList fieldNames gotos where
  ValidateGotoTargetsList _ '[] = ()
  ValidateGotoTargetsList fieldNames ('(target, _) ': rest) =
    ( If (Elem target fieldNames)
         (() :: Constraint)
         (InvalidGotoTargetError target fieldNames)
    , ValidateGotoTargetsList fieldNames rest
    )

-- | Validate that no UsesEffects annotation contains To markers.
--
-- To markers are for GotoChoice return types, not for UsesEffects.
-- This catches a common mistake when maintaining parallel type aliases.
type ValidateNoToInEffects :: (Type -> Type) -> Constraint
type family ValidateNoToInEffects graph where
  ValidateNoToInEffects graph =
    ValidateNoToInEffectsList (FieldsWithNamesOf graph)

-- | Validate each field's UsesEffects doesn't contain To markers.
type ValidateNoToInEffectsList :: [(Symbol, Type)] -> Constraint
type family ValidateNoToInEffectsList fields where
  ValidateNoToInEffectsList '[] = ()
  ValidateNoToInEffectsList ('(_name, def) ': rest) =
    ( ValidateEffectsFromDef def
    , ValidateNoToInEffectsList rest
    )

-- | Extract and validate effects from a node definition.
--
-- We check both @Effect and @Type kinds because:
-- - Correct usage: UsesEffects '[Goto "x" A] has kind [Effect]
-- - Incorrect usage: UsesEffects '[To "x" A] has kind [Type]
--
-- By checking both, we catch when users accidentally use To markers.
type ValidateEffectsFromDef :: Type -> Constraint
type family ValidateEffectsFromDef def where
  ValidateEffectsFromDef def =
    ( ValidateEffectsMaybeEffect (GetUsesEffects @Effect def)
    , ValidateEffectsMaybeType (GetUsesEffects @Type def)
    )

-- | Apply validation to Maybe effect list (Effect kind).
type ValidateEffectsMaybeEffect :: Maybe [Effect] -> Constraint
type family ValidateEffectsMaybeEffect mEffs where
  ValidateEffectsMaybeEffect 'Nothing = ()
  ValidateEffectsMaybeEffect ('Just effs) = ValidateNotToMarkers effs

-- | Apply validation to Maybe type list (Type kind).
-- This catches To markers which have kind Type, not Effect.
type ValidateEffectsMaybeType :: Maybe [Type] -> Constraint
type family ValidateEffectsMaybeType mTypes where
  ValidateEffectsMaybeType 'Nothing = ()
  ValidateEffectsMaybeType ('Just types) = ValidateNotToMarkers types

-- | Error for invalid Goto target.
type InvalidGotoTargetError :: Symbol -> [Symbol] -> Constraint
type family InvalidGotoTargetError target fieldNames where
  InvalidGotoTargetError target fieldNames = DelayedTypeError
    ( HR
      ':$$: 'Text "  Goto target \"" ':<>: 'Text target ':<>: 'Text "\" not found"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your handler has:"
      ':$$: CodeLine "Goto \"" ':<>: 'Text target ':<>: 'Text "\" payload"
      ':$$: Blank
      ':$$: Indent "But no field named \"" ':<>: 'Text target ':<>: 'Text "\" exists in the graph."
      ':$$: Blank
      ':$$: Indent "Available fields:"
      ':$$: FormatSymbolList fieldNames
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add a field: " ':<>: 'Text target ':<>: 'Text " :: mode :- ..."
      ':$$: Bullet "Use Goto Exit to terminate the graph"
      ':$$: Bullet "Check spelling of the target name"
    )

-- ════════════════════════════════════════════════════════════════════════════
-- VALID GRAPH RECORD
-- ════════════════════════════════════════════════════════════════════════════

-- | Bundle all validation constraints for a graph record.
--
-- Use this as the constraint on functions that work with validated graphs:
--
-- @
-- runGraph :: ValidGraphRecord SupportGraph => SupportGraph (AsHandler es) -> ...
-- @
--
-- Validates:
--
-- * Has Generic instance (for field extraction)
-- * Has an EntryNode field
-- * Has an Exit field
-- * All Goto targets reference existing fields
-- * All nodes are reachable from EntryNode
-- * All Logic nodes have a path to Exit
-- * All Goto targets can receive their payload type
-- * All Logic nodes have at least one Goto effect
-- * No nodes have only Goto Self (infinite loop)
type ValidGraphRecord :: (Type -> Type) -> Constraint
type ValidGraphRecord graph =
  ( RequireGeneric graph
  , ValidateEntryExit graph
  , ValidateGotoTargets graph
  , ValidateNoToInEffects graph  -- Check for To markers in UsesEffects
  -- Structural validation
  , AllFieldsReachable graph
  , AllLogicFieldsReachExit graph
  , NoDeadGotosRecord graph
  -- Transition validation
  , AllLogicNodesHaveGoto graph
  , NoGotoSelfOnly graph
  -- Fork/Barrier validation
  , ValidateForkBarrierPairs graph
  )

-- | Require Generic instance with a helpful error message.
--
-- This is a class rather than a type family so that GHC produces
-- a clearer "No instance" error that mentions deriving Generic.
class Generic (graph AsGraph) => RequireGeneric (graph :: Type -> Type)

-- Default instance for all types that have Generic
instance Generic (graph AsGraph) => RequireGeneric graph

-- | Error message when Generic is missing (as a type error for documentation)
type MissingGenericError :: (Type -> Type) -> Constraint
type MissingGenericError graph = TypeError
  ('Text "Graph validation failed: missing Generic instance"
   ':$$: 'Text ""
   ':$$: 'Text "Your graph type needs 'deriving Generic' to work with ValidGraphRecord."
   ':$$: 'Text ""
   ':$$: 'Text "Fix: Add Generic to the deriving clause:"
   ':$$: 'Text ""
   ':$$: 'Text "  data MyGraph mode = MyGraph"
   ':$$: 'Text "    { entry :: mode :- EntryNode Input"
   ':$$: 'Text "    , ..."
   ':$$: 'Text "    }"
   ':$$: 'Text "    deriving Generic  -- <- Add this"
  )

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH PRODUCT (GENERIC TRAVERSAL)
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for generic product traversal of graph records.
--
-- This mirrors Servant's GServantProduct class, enabling transformation
-- between record representations and product types.
--
-- Instances handle the Generic representation constructors:
--
-- * M1 (metadata wrappers) - pass through
-- * :*: (products) - combine as tuples
-- * K1 (fields) - the actual field values
class GraphProduct (f :: Type -> Type) where
  -- | The product type that this representation maps to.
  type GProductType f :: Type

  -- | Convert from Generic representation to product.
  gToProduct :: f p -> GProductType f

  -- | Convert from product to Generic representation.
  gFromProduct :: GProductType f -> f p

-- M1 (metadata) instance - pass through
instance GraphProduct f => GraphProduct (M1 i c f) where
  type GProductType (M1 i c f) = GProductType f
  gToProduct = gToProduct . unM1
  gFromProduct = M1 . gFromProduct

-- Product instance - combine as tuple
instance (GraphProduct l, GraphProduct r) => GraphProduct (l :*: r) where
  type GProductType (l :*: r) = (GProductType l, GProductType r)
  gToProduct (l :*: r) = (gToProduct l, gToProduct r)
  gFromProduct (l, r) = gFromProduct l :*: gFromProduct r

-- K1 (field) instance - the field value itself
instance GraphProduct (K1 i c) where
  type GProductType (K1 i c) = c
  gToProduct = unK1
  gFromProduct = K1

-- ════════════════════════════════════════════════════════════════════════════
-- CONVENIENCE CONSTRAINTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Constraint for graph record types.
--
-- Bundles the requirements for working with a graph record in a given mode.
type GenericGraph :: (Type -> Type) -> Type -> Constraint
type GenericGraph graph mode =
  ( GraphMode mode
  , Generic (graph mode)
  , GraphProduct (Rep (graph mode))
  )

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH-VALIDATED GOTO
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph-validated goto using TypeApplications.
--
-- Like 'goto', but also validates at compile time that the target field
-- exists in the specified graph type. This catches typos and prevents
-- accidentally referencing fields from other graphs.
--
-- @
-- -- Define a graph
-- data SupportGraph mode = SupportGraph
--   { sgEntry  :: mode :- EntryNode Message
--   , sgRefund :: mode :- LLMNode :@ ...
--   , sgFaq    :: mode :- LLMNode :@ ...
--   , sgExit   :: mode :- ExitNode Response
--   }
--
-- -- In a handler:
-- routeHandler :: (...) => Intent -> Eff es ()
-- routeHandler intent = case intent of
--   Refund -> gotoField @SupportGraph @"sgRefund" msg   -- Validated!
--   FAQ    -> gotoField @SupportGraph @"sgFaq" msg      -- Validated!
--   _      -> gotoField @SupportGraph @"sgTypo" msg     -- Compile error!
-- @
--
-- If the target field doesn't exist, you get a helpful error listing valid fields:
--
-- @
-- Field 'sgTypo' not found in graph
--
-- Available fields:
--   • sgEntry
--   • sgRefund
--   • sgFaq
--   • sgExit
-- @
gotoField
  :: forall (graph :: Type -> Type) (name :: Symbol) payload effs.
     ( KnownSymbol name
     , Generic (graph AsGraph)
     , ElemCWithOptions name (FieldNamesOf graph) (FieldNamesOf graph)
     , Member (Goto name payload) effs
     )
  => payload
  -> Eff effs ()
gotoField = goto @name
