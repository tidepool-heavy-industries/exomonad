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
-- Instead of type-level lists:
--
-- @
-- type MyGraph = Graph '[
--     Entry :~> Message
--   , "classify" := LLM :@ Needs '[Message] :@ Schema Intent
--   , Exit :<~ Response
--   ]
-- @
--
-- We use mode-parameterized records:
--
-- @
-- data MyGraph mode = MyGraph
--   { entry    :: mode :- Entry Message
--   , classify :: mode :- LLM :@ Needs '[Message] :@ Schema Intent
--   , exit     :: mode :- Exit Response
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

    -- * Entry/Exit Types
  , Entry
  , Exit

    -- * Node Kind Wrappers (for record DSL)
  , LLMNode
  , LogicNode

    -- * Field Name Extraction
  , FieldNames
  , FieldDefs
  , FieldsWithNames
  , FieldNamesOf
  , FieldsWithNamesOf

    -- * Graph-Validated Goto
  , gotoField

    -- * Type-Level Utilities
  , Elem
  , ElemC
  , ElemCWithOptions
  , If
  , Append
  , TupleOf
  , type (||)
  , OrMaybe

    -- * Re-exports for LLM Handlers
  , LLMHandler(..)

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

import Tidepool.Graph.Types (type (:@), Needs, Schema, Template, Vision, Tools, Memory, System, UsesEffects)
import Tidepool.Graph.Template (TemplateContext)
import Tidepool.Graph.Edges (GetUsesEffects, GetGotoTargets, GotoEffectsToTargets)
import Tidepool.Graph.Goto (Goto, goto, GotoChoice, To, LLMHandler(..))
import Tidepool.Graph.Validate.RecordStructure
  ( AllFieldsReachable, AllLogicFieldsReachExit, NoDeadGotosRecord
  , AllLogicNodesHaveGoto, NoGotoSelfOnly
  )
import Tidepool.Graph.Generic.Core
  ( GraphMode(..)
  , AsGraph
  , LLMNode
  , LogicNode
  , Entry
  , Exit
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
-- * LLM nodes: @Needs[0] -> Needs[1] -> ... -> Eff es (TemplateContext tpl)@
-- * Logic nodes: @Needs[0] -> Needs[1] -> ... -> Eff '[effects...] ()@
-- * Entry\/Exit: @Proxy inputType@ / @Proxy outputType@
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
-- Strategy: Peel annotations from the outside in, accumulating Needs types
-- and finding Template/UsesEffects. When we hit bare LLM or Logic, build
-- the final function type.
--
-- @
-- NodeHandler (LLM :@ Needs '[Message] :@ Template ClassifyTpl :@ Schema Intent) es
--   = Message -> Eff es ClassifyContext
--
-- NodeHandler (Logic :@ Needs '[Intent] :@ UsesEffects '[Goto "respond", GotoExit]) es
--   = Intent -> Eff '[Goto "respond", GotoExit] ()
--
-- NodeHandler (Entry Message) es = Proxy Message
-- NodeHandler (Exit Response) es = Proxy Response
-- @
type NodeHandler :: Type -> [Effect] -> Type
type family NodeHandler nodeDef es where
  -- Entry/Exit produce Proxy (self-documenting markers)
  NodeHandler (Entry a) es = Proxy a
  NodeHandler (Exit a) es = Proxy a

  -- Any annotated node: dispatch to the appropriate accumulator based on base kind
  -- We peel from outside, so start with the full node
  -- Accumulator: (nodeDef, origNode, es, needs, mTpl, mSchema, mEffs)
  NodeHandler (node :@ ann) es = NodeHandlerDispatch (node :@ ann) (node :@ ann) es '[] 'Nothing 'Nothing 'Nothing

  -- Bare LLMNode/LogicNode without annotations - error
  NodeHandler LLMNode es = TypeError
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
      ':$$: CodeLine "  myNode :: mode :- LLMNode :@ Needs '[Input] :@ Template MyTpl :@ Schema Output"
      ':$$: Blank
      ':$$: Example
      ':$$: CodeLine "-- In Context.hs (separate module for TH staging):"
      ':$$: CodeLine "data ClassifyContext = ClassifyContext { query :: Text }"
      ':$$: CodeLine ""
      ':$$: CodeLine "-- In your graph definition:"
      ':$$: CodeLine "classify :: mode :- LLMNode"
      ':$$: CodeLine "            :@ Needs '[Message]"
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
      ':$$: CodeLine "               :@ Needs '[Intent]"
      ':$$: CodeLine "               :@ UsesEffects '[Goto \"process\" Data, Goto Exit Result]"
      ':$$: Blank
      ':$$: Example
      ':$$: CodeLine "-- Handler implementation:"
      ':$$: CodeLine "router :: Intent -> Eff es (GotoChoice '[To \"process\" Data, To Exit Result])"
      ':$$: CodeLine "router intent = case intent of"
      ':$$: CodeLine "  NeedsProcessing x -> pure $ gotoChoice @\"process\" x"
      ':$$: CodeLine "  Done result       -> pure $ gotoExit result"
    )

-- | Unified accumulator that peels annotations and dispatches based on base kind.
--
-- = Strategy: Outside-In Annotation Peeling
--
-- Given a node like:
--
-- @
-- LLMNode :@ Needs '[A, B] :@ Template T :@ Schema S :@ Vision
-- @
--
-- The family processes annotations from left to right (outside-in), accumulating:
--
-- - Needs types → @needs@ parameter
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
-- NodeHandlerDispatch nodeDef origNode es needs mTpl mSchema mEffs
--                     ^        ^         ^  ^     ^    ^       ^
--                     |        |         |  |     |    |       |
--                     |        |         |  |     |    |       +-- Maybe UsesEffects (for routing)
--                     |        |         |  |     |    +---------- Maybe Schema type (LLM output)
--                     |        |         |  |     +--------------- Maybe Template type (LLM context)
--                     |        |         |  +--------------------- Accumulated Needs types (handler params)
--                     |        |         +------------------------ Effect stack from AsHandler
--                     |        +---------------------------------- Original node (for errors)
--                     +------------------------------------------- Current node being processed
-- @
--
-- = Clause Groups
--
-- 1. **Lines 286-287**: Needs accumulation (appends to list)
-- 2. **Lines 289-329**: Template/Schema recording (with duplicate detection)
-- 3. **Lines 331-339**: Skipped annotations (Vision, Tools, Memory, System)
-- 4. **Lines 341-360**: UsesEffects recording (with duplicate detection)
-- 5. **Lines 362-468**: LLMNode terminal cases (3 handler variants)
-- 6. **Lines 470-510**: LogicNode terminal cases
--
-- = Why origNode is Preserved
--
-- The @origNode@ parameter never changes. It's the original full node definition
-- used in error messages to show the user what they wrote, even after annotations
-- have been peeled away.
type NodeHandlerDispatch :: Type -> Type -> [Effect] -> [Type] -> Maybe Type -> Maybe Type -> Maybe Type -> Type
type family NodeHandlerDispatch nodeDef origNode es needs mTpl mSchema mEffs where
  -- Peel Needs annotation - accumulate types
  NodeHandlerDispatch (node :@ Needs ts) orig es needs mTpl mSchema mEffs =
    NodeHandlerDispatch node orig es (Append needs ts) mTpl mSchema mEffs

  -- Peel Template annotation - record it (for LLM nodes)
  NodeHandlerDispatch (node :@ Template tpl) orig es needs 'Nothing mSchema mEffs =
    NodeHandlerDispatch node orig es needs ('Just tpl) mSchema mEffs

  -- Detect duplicate Template annotations
  NodeHandlerDispatch (node :@ Template _) orig es needs ('Just _) mSchema mEffs = TypeError
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
  NodeHandlerDispatch (node :@ Schema s) orig es needs mTpl 'Nothing mEffs =
    NodeHandlerDispatch node orig es needs mTpl ('Just s) mEffs

  -- Detect duplicate Schema annotations
  NodeHandlerDispatch (node :@ Schema _) orig es needs mTpl ('Just _) mEffs = TypeError
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

  -- Skip other annotations (Vision, Tools, Memory, System)
  NodeHandlerDispatch (node :@ Vision) orig es needs mTpl mSchema mEffs =
    NodeHandlerDispatch node orig es needs mTpl mSchema mEffs
  NodeHandlerDispatch (node :@ Tools _) orig es needs mTpl mSchema mEffs =
    NodeHandlerDispatch node orig es needs mTpl mSchema mEffs
  NodeHandlerDispatch (node :@ Memory _) orig es needs mTpl mSchema mEffs =
    NodeHandlerDispatch node orig es needs mTpl mSchema mEffs
  NodeHandlerDispatch (node :@ System _) orig es needs mTpl mSchema mEffs =
    NodeHandlerDispatch node orig es needs mTpl mSchema mEffs

  -- Peel UsesEffects annotation - record effects
  -- (Validation that To is not used here happens at ValidGraphRecord level)
  NodeHandlerDispatch (node :@ UsesEffects effs) orig es needs mTpl mSchema 'Nothing =
    NodeHandlerDispatch node orig es needs mTpl mSchema ('Just (EffStack effs))

  -- Detect duplicate UsesEffects annotations
  NodeHandlerDispatch (node :@ UsesEffects _) orig es needs mTpl mSchema ('Just _) = TypeError
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
  -- LLM handlers come in three variants based on what annotations are present:
  --
  -- 1. **BEFORE-ONLY** (Template + Schema, no UsesEffects)
  --
  --    Handler: @needs -> Eff es (TemplateContext tpl)@
  --    Flow: Build context → Render template → LLM call → Parse schema
  --    Routing: Implicit via Schema → Needs data flow
  --    Use case: Linear graphs where LLM output flows to next node
  --
  -- 2. **AFTER-ONLY** (Schema + UsesEffects, no Template)
  --
  --    Handler: @needs -> Eff es (GotoChoice targets)@
  --    Flow: Use default context → LLM call → Route based on output
  --    Routing: Explicit via gotoChoice/gotoExit
  --    Use case: Branching based on LLM classification
  --
  -- 3. **BOTH** (Template + Schema + UsesEffects)
  --
  --    Handler: @LLMHandler needs schema targets es tpl@
  --    Flow: Build context → Render template → LLM call → Route based on output
  --    Routing: Explicit via gotoChoice/gotoExit
  --    Use case: Custom prompts AND custom routing
  --
  -- ══════════════════════════════════════════════════════════════════════════

  -- LLMNode with Template only (before-only): handler must use LLMBefore constructor
  NodeHandlerDispatch LLMNode orig es needs ('Just tpl) ('Just schema) 'Nothing =
    LLMHandler (TupleOf needs) schema '[] es (TemplateContext tpl)

  -- LLMNode with Template AND UsesEffects (both): handler must use LLMBoth constructor
  NodeHandlerDispatch LLMNode orig es needs ('Just tpl) ('Just schema) ('Just (EffStack effs)) =
    LLMHandler (TupleOf needs) schema (GotoEffectsToTargets effs) es (TemplateContext tpl)

  -- LLMNode with UsesEffects but no Template (after-only): handler must use LLMAfter constructor
  NodeHandlerDispatch LLMNode orig es needs 'Nothing ('Just schema) ('Just (EffStack effs)) =
    LLMHandler (TupleOf needs) schema (GotoEffectsToTargets effs) es ()

  -- LLMNode with Schema only (no Template or UsesEffects) - error
  NodeHandlerDispatch LLMNode orig es needs 'Nothing ('Just schema) 'Nothing = TypeError
    ( HR
      ':$$: 'Text "  LLM node incomplete: has Schema but no Template or routing"
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
      ':$$: Indent "LLM nodes have three possible configurations:"
      ':$$: Blank
      ':$$: CodeLine "1. BEFORE-ONLY (most common):"
      ':$$: CodeLine "   LLMNode :@ Template T :@ Schema S"
      ':$$: CodeLine "   -> Handler builds context, routing is implicit via Needs"
      ':$$: Blank
      ':$$: CodeLine "2. AFTER-ONLY (custom routing):"
      ':$$: CodeLine "   LLMNode :@ Schema S :@ UsesEffects '[Goto ...]"
      ':$$: CodeLine "   -> Uses default context, handler routes based on output"
      ':$$: Blank
      ':$$: CodeLine "3. BOTH (full control):"
      ':$$: CodeLine "   LLMNode :@ Template T :@ Schema S :@ UsesEffects '[Goto ...]"
      ':$$: CodeLine "   -> Custom context AND custom routing"
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add Template for custom prompts:"
      ':$$: CodeLine "  myNode :: mode :- LLMNode :@ Template MyTpl :@ Schema " ':<>: 'ShowType schema
      ':$$: Bullet "Or add UsesEffects for custom routing:"
      ':$$: CodeLine "  myNode :: mode :- LLMNode :@ Schema " ':<>: 'ShowType schema ':<>: 'Text " :@ UsesEffects '[Goto \"next\" X]"
    )

  -- LLMNode missing both Template and Schema - error
  NodeHandlerDispatch LLMNode orig es needs 'Nothing 'Nothing _ = TypeError
    ( HR
      ':$$: 'Text "  LLM node missing required annotations"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your LLM node has neither Template nor Schema."
      ':$$: Indent "We need at least Schema to know what the LLM returns."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "The LLM call flow requires knowing:"
      ':$$: Blank
      ':$$: CodeLine "Template  -> What prompt to render (context type)"
      ':$$: CodeLine "Schema    -> What the LLM returns (output type)"
      ':$$: Blank
      ':$$: Indent "Schema is ALWAYS required."
      ':$$: Indent "Template is required unless you use UsesEffects for after-only routing."
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add both Template and Schema:"
      ':$$: CodeLine "  myNode :: mode :- LLMNode"
      ':$$: CodeLine "             :@ Needs '[Input]"
      ':$$: CodeLine "             :@ Template MyContextTpl"
      ':$$: CodeLine "             :@ Schema MyOutputType"
    )

  -- LLMNode missing Schema - error
  NodeHandlerDispatch LLMNode orig es needs _ 'Nothing _ = TypeError
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

  -- LogicNode with UsesEffects: returns GotoChoice
  NodeHandlerDispatch LogicNode orig es needs _ _ ('Just (EffStack effs)) =
    BuildFunctionType needs (Eff es (GotoChoice (GotoEffectsToTargets effs)))

  -- LogicNode without UsesEffects - error
  NodeHandlerDispatch LogicNode orig es needs _ _ 'Nothing = TypeError
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
      ':$$: CodeLine "               :@ Needs '[YourInput]"
      ':$$: CodeLine "               :@ UsesEffects '[Goto \"target\" Payload, Goto Exit Result]"
    )

-- | Convert a Needs list to a single type (unit, single, or tuple).
type TupleOf :: [Type] -> Type
type family TupleOf ts where
  TupleOf '[] = ()
  TupleOf '[t] = t
  TupleOf '[t1, t2] = (t1, t2)
  TupleOf '[t1, t2, t3] = (t1, t2, t3)
  TupleOf '[t1, t2, t3, t4] = (t1, t2, t3, t4)
  TupleOf '[t1, t2, t3, t4, t5] = (t1, t2, t3, t4, t5)
  TupleOf '[t1, t2, t3, t4, t5, t6] = (t1, t2, t3, t4, t5, t6)
  TupleOf '[t1, t2, t3, t4, t5, t6, t7] = (t1, t2, t3, t4, t5, t6, t7)
  TupleOf '[t1, t2, t3, t4, t5, t6, t7, t8] = (t1, t2, t3, t4, t5, t6, t7, t8)
  TupleOf ts = TypeError
    ( HR
      ':$$: 'Text "  Too many Needs types (maximum 8)"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your node has more than 8 Needs types."
      ':$$: Indent "The tuple-based handler signature can't accommodate this many."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "Handler parameters come from the Needs list:"
      ':$$: Blank
      ':$$: CodeLine "Needs '[A]           -> handler :: A -> Eff es ..."
      ':$$: CodeLine "Needs '[A, B]        -> handler :: A -> B -> Eff es ..."
      ':$$: CodeLine "Needs '[A, B, C]     -> handler :: (A, B, C) -> Eff es ..."
      ':$$: CodeLine "Needs '[A, B, ..., H] -> handler :: (A, B, ..., H) -> Eff es ..."
      ':$$: Blank
      ':$$: Indent "Beyond 8 types, we can't form a tuple."
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Group related types into a record:"
      ':$$: CodeLine "  -- Instead of: Needs '[A, B, C, D, E, F, G, H, I]"
      ':$$: CodeLine "  -- Use:"
      ':$$: CodeLine "  data MyContext = MyContext { a :: A, b :: B, ... }"
      ':$$: CodeLine "  Needs '[MyContext]"
      ':$$: Blank
      ':$$: Bullet "Or split the node into multiple nodes"
    )

-- | Wrapper to distinguish Template types from EffStack in the Maybe
data EffStack (effs :: [Effect])

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
-- myNode :: mode :- LogicNode :@ Needs '[Input] :@ UsesEffects MyEffects
--          -- ERROR: UsesEffects needs 'Goto' effects, not 'To' markers
--
-- -- Correct: Using 'Goto' effects in UsesEffects
-- type MyEffects = '[Goto "nodeA" Data, Goto Exit Result]
--
-- myNode :: mode :- LogicNode :@ Needs '[Input] :@ UsesEffects MyEffects
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

-- | Build a function type from a list of parameters and a return type.
--
-- @
-- BuildFunctionType '[A, B, C] R = A -> B -> C -> R
-- BuildFunctionType '[] R = R
-- @
type BuildFunctionType :: [Type] -> Type -> Type
type family BuildFunctionType params ret where
  BuildFunctionType '[] ret = ret
  BuildFunctionType (p ': ps) ret = p -> BuildFunctionType ps ret

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
--   { entry    :: mode :- Entry Message
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
--   = '[ '("entry", Entry Message)
--      , '("classify", LLM :@ Needs '[Message] :@ Schema Intent)
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
-- RECORD VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if the Generic representation contains an Entry field.
--
-- Returns 'True if any field has type @Entry a@ for some @a@.
type HasEntryField :: (Type -> Type) -> Bool
type family HasEntryField f where
  HasEntryField (M1 D _ f) = HasEntryField f
  HasEntryField (M1 C _ f) = HasEntryField f
  HasEntryField (M1 S _ (K1 _ (Entry _))) = 'True
  HasEntryField (M1 S _ _) = 'False
  HasEntryField (l :*: r) = HasEntryField l || HasEntryField r
  HasEntryField _ = 'False

-- | Check if the Generic representation contains an Exit field.
type HasExitField :: (Type -> Type) -> Bool
type family HasExitField f where
  HasExitField (M1 D _ f) = HasExitField f
  HasExitField (M1 C _ f) = HasExitField f
  HasExitField (M1 S _ (K1 _ (Exit _))) = 'True
  HasExitField (M1 S _ _) = 'False
  HasExitField (l :*: r) = HasExitField l || HasExitField r
  HasExitField _ = 'False

-- | Type-level Or for Bool.
type (||) :: Bool -> Bool -> Bool
type family a || b where
  'True  || _ = 'True
  'False || b = b

-- | Count Entry fields in a Generic representation.
--
-- Returns the number of fields with type @Entry a@ for some @a@.
type CountEntries :: (Type -> Type) -> Nat
type family CountEntries f where
  CountEntries (M1 D _ f) = CountEntries f
  CountEntries (M1 C _ f) = CountEntries f
  CountEntries (M1 S _ (K1 _ (Entry _))) = 1
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
  CountExits (M1 S _ (K1 _ (Exit _))) = 1
  CountExits (M1 S _ _) = 0
  CountExits (l :*: r) = CountExits l + CountExits r
  CountExits _ = 0

-- | Extract Entry type from a graph record.
type GetEntryType :: (Type -> Type) -> Maybe Type
type family GetEntryType f where
  GetEntryType (M1 D _ f) = GetEntryType f
  GetEntryType (M1 C _ f) = GetEntryType f
  GetEntryType (M1 S _ (K1 _ (Entry a))) = 'Just a
  GetEntryType (M1 S _ _) = 'Nothing
  GetEntryType (l :*: r) = OrMaybe (GetEntryType l) (GetEntryType r)
  GetEntryType _ = 'Nothing

-- | Extract Exit type from a graph record.
type GetExitType :: (Type -> Type) -> Maybe Type
type family GetExitType f where
  GetExitType (M1 D _ f) = GetExitType f
  GetExitType (M1 C _ f) = GetExitType f
  GetExitType (M1 S _ (K1 _ (Exit a))) = 'Just a
  GetExitType (M1 S _ _) = 'Nothing
  GetExitType (l :*: r) = OrMaybe (GetExitType l) (GetExitType r)
  GetExitType _ = 'Nothing

-- | Return first Just, or Nothing if both Nothing.
type OrMaybe :: Maybe k -> Maybe k -> Maybe k
type family OrMaybe a b where
  OrMaybe ('Just x) _ = 'Just x
  OrMaybe 'Nothing b = b

-- | Validate a graph record has exactly one Entry and one Exit field.
--
-- Produces type errors if Entry or Exit are missing, or if there are duplicates.
type ValidateEntryExit :: (Type -> Type) -> Constraint
type family ValidateEntryExit graph where
  ValidateEntryExit graph =
    ( ValidateEntryCount (CountEntries (Rep (graph AsGraph)))
    , ValidateExitCount (CountExits (Rep (graph AsGraph)))
    )

-- | Validate Entry count is exactly 1.
type ValidateEntryCount :: Nat -> Constraint
type family ValidateEntryCount n where
  ValidateEntryCount 0 = DelayedTypeError
    ( HR
      ':$$: 'Text "  Missing Entry field"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your graph record has no Entry field."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "Every graph needs exactly one entry point that defines"
      ':$$: Indent "what type of input the graph accepts."
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add an entry field:"
      ':$$: CodeLine "entry :: mode :- Entry YourInputType"
    )
  ValidateEntryCount 1 = ()
  ValidateEntryCount _ = DelayedTypeError
    ( HR
      ':$$: 'Text "  Multiple Entry fields"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your graph record has more than one Entry field."
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "A graph can only have one entry point. Multiple entries"
      ':$$: Indent "would be ambiguous - which one starts the graph?"
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Remove duplicate Entry fields, keeping just one"
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
      ':$$: CodeLine "exit :: mode :- Exit YourResultType"
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
-- * Has an Entry field
-- * Has an Exit field
-- * All Goto targets reference existing fields
-- * All nodes are reachable from Entry
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
   ':$$: 'Text "    { entry :: mode :- Entry Input"
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
--   { sgEntry  :: mode :- Entry Message
--   , sgRefund :: mode :- LLMNode :@ ...
--   , sgFaq    :: mode :- LLMNode :@ ...
--   , sgExit   :: mode :- Exit Response
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
