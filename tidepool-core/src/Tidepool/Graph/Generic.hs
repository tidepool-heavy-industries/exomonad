{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
  , If
  , Append
  , type (||)
  , OrMaybe

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
import Effectful (Effect)
import Effectful qualified as E

import Tidepool.Graph.Types (type (:@), Needs, Schema, Template, Vision, Tools, Memory, System, UsesEffects)
import Tidepool.Graph.Template (TemplateContext)
import Tidepool.Graph.Edges (GetUsesEffects, GetGotoTargets)
import Tidepool.Graph.Goto (Goto, goto)

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH MODE CLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Mode determines how graph record fields are interpreted.
--
-- This mirrors Servant's 'GenericMode' class. Each mode defines how
-- to transform a node definition into a concrete type.
--
-- @
-- data MyGraph mode = MyGraph
--   { classify :: mode :- LLM :@ Needs '[Message] :@ Schema Intent
--   , ...
--   }
-- @
class GraphMode mode where
  type mode :- nodeDef :: Type

infixl 0 :-

-- ════════════════════════════════════════════════════════════════════════════
-- ENTRY/EXIT MARKER TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Entry point marker type.
--
-- Used in graph records to declare the input type:
--
-- @
-- data MyGraph mode = MyGraph
--   { entry :: mode :- Entry Message
--   , ...
--   }
-- @
--
-- In 'AsHandler' mode, Entry fields become @Proxy inputType@.
type Entry :: Type -> Type
data Entry inputType

-- | Exit point marker type.
--
-- Used in graph records to declare the output type:
--
-- @
-- data MyGraph mode = MyGraph
--   { ...
--   , exit :: mode :- Exit Response
--   }
-- @
--
-- In 'AsHandler' mode, Exit fields become @Proxy outputType@.
type Exit :: Type -> Type
data Exit outputType

-- ════════════════════════════════════════════════════════════════════════════
-- NODE KIND WRAPPER TYPES (for record DSL)
-- ════════════════════════════════════════════════════════════════════════════

-- | LLM node marker type (kind Type) for record-based graph DSL.
--
-- Use this instead of the 'LLM' data constructor (which has kind 'NodeKind').
-- In the list-based DSL, @"name" := LLM@ lifts to Type, but in the record DSL
-- field names serve as node names, so we need a Type-kinded marker.
--
-- @
-- data MyGraph mode = MyGraph
--   { classify :: mode :- LLMNode :@ Needs '[Message] :@ Template T :@ Schema Intent
--   }
-- @
type LLMNode :: Type
data LLMNode

-- | Logic node marker type (kind Type) for record-based graph DSL.
--
-- Use this instead of the 'Logic' data constructor (which has kind 'NodeKind').
--
-- @
-- data MyGraph mode = MyGraph
--   { route :: mode :- LogicNode :@ Needs '[Intent] :@ UsesEffects '[Goto "a", Goto "b"]
--   }
-- @
type LogicNode :: Type
data LogicNode

-- ════════════════════════════════════════════════════════════════════════════
-- ASGRAPH MODE (IDENTITY)
-- ════════════════════════════════════════════════════════════════════════════

-- | Identity mode - fields contain node definitions as-is.
--
-- Used for:
--
-- * Type-level validation (ValidGraph constraint)
-- * Documentation generation
-- * Reification to runtime info
--
-- @
-- graph :: MyGraph AsGraph
-- -- All fields are the node definition types themselves
-- @
data AsGraph

instance GraphMode AsGraph where
  type AsGraph :- nodeDef = nodeDef

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
  NodeHandler (node :@ ann) es = NodeHandlerDispatch (node :@ ann) es '[] 'Nothing

  -- Bare LLMNode/LogicNode without annotations - error
  NodeHandler LLMNode es = LLMRequiresAnnotations
  NodeHandler LogicNode es = LogicRequiresAnnotations

-- Placeholder error types (will be TypeError in full implementation)
data LLMRequiresAnnotations
data LogicRequiresAnnotations

-- | Unified accumulator that peels annotations and dispatches based on base kind.
--
-- Parameters:
--   nodeDef - current node being processed (may have :@ annotations)
--   es      - effect stack from AsHandler
--   needs   - accumulated Needs types (collected in order, not reversed)
--   mTpl    - Maybe found Template type (for LLM nodes)
type NodeHandlerDispatch :: Type -> [Effect] -> [Type] -> Maybe Type -> Type
type family NodeHandlerDispatch nodeDef es needs mTpl where
  -- Peel Needs annotation - accumulate types
  NodeHandlerDispatch (node :@ Needs ts) es needs mTpl =
    NodeHandlerDispatch node es (Append needs ts) mTpl

  -- Peel Template annotation - record it (for LLM nodes)
  NodeHandlerDispatch (node :@ Template tpl) es needs 'Nothing =
    NodeHandlerDispatch node es needs ('Just tpl)

  -- Skip other annotations (Schema, Vision, Tools, Memory, System)
  NodeHandlerDispatch (node :@ Schema _) es needs mTpl =
    NodeHandlerDispatch node es needs mTpl
  NodeHandlerDispatch (node :@ Vision) es needs mTpl =
    NodeHandlerDispatch node es needs mTpl
  NodeHandlerDispatch (node :@ Tools _) es needs mTpl =
    NodeHandlerDispatch node es needs mTpl
  NodeHandlerDispatch (node :@ Memory _) es needs mTpl =
    NodeHandlerDispatch node es needs mTpl
  NodeHandlerDispatch (node :@ System _) es needs mTpl =
    NodeHandlerDispatch node es needs mTpl

  -- Peel UsesEffects annotation - build Logic handler type immediately
  -- Logic handlers use their own effect stack, not the es from AsHandler
  NodeHandlerDispatch (node :@ UsesEffects effs) es needs mTpl =
    NodeHandlerDispatch node es needs ('Just (EffStack effs))

  -- Base case: bare LLMNode with Template found
  NodeHandlerDispatch LLMNode es needs ('Just tpl) =
    BuildFunctionType needs (E.Eff es (TemplateContext tpl))

  -- Base case: bare LLMNode without Template - error
  NodeHandlerDispatch LLMNode es needs 'Nothing = LLMRequiresTemplate

  -- Base case: bare LogicNode with UsesEffects found
  NodeHandlerDispatch LogicNode es needs ('Just (EffStack effs)) =
    BuildFunctionType needs (E.Eff effs ())

  -- Base case: bare LogicNode without UsesEffects - error
  NodeHandlerDispatch LogicNode es needs _ = LogicRequiresUsesEffects

-- | Wrapper to distinguish Template types from EffStack in the Maybe
data EffStack (effs :: [Effect])

-- Placeholder error types
data LLMRequiresTemplate
data LogicRequiresUsesEffects

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
    ('Text "Field '" ':<>: 'Text s ':<>: 'Text "' not found in graph"
     ':$$: 'Text "Check that the field name matches a record field in the graph type."
    )
  ElemC s (s ': _) = ()
  ElemC s (_ ': rest) = ElemC s rest

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
    ('Text "Graph record validation failed: missing Entry field"
     ':$$: 'Text "Add a field like: entry :: mode :- Entry YourInputType"
    )
  ValidateEntryCount 1 = ()
  ValidateEntryCount _ = DelayedTypeError
    ('Text "Graph record validation failed: multiple Entry fields"
     ':$$: 'Text "A graph record must have exactly one Entry field."
    )

-- | Validate Exit count is exactly 1.
type ValidateExitCount :: Nat -> Constraint
type family ValidateExitCount n where
  ValidateExitCount 0 = DelayedTypeError
    ('Text "Graph record validation failed: missing Exit field"
     ':$$: 'Text "Add a field like: exit :: mode :- Exit YourOutputType"
    )
  ValidateExitCount 1 = ()
  ValidateExitCount _ = DelayedTypeError
    ('Text "Graph record validation failed: multiple Exit fields"
     ':$$: 'Text "A graph record must have exactly one Exit field."
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
-- Note: We fix the kind to Effect to avoid ambiguous type inference.
-- UsesEffects annotations contain effectful Effects.
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

-- | Error for invalid Goto target.
type InvalidGotoTargetError :: Symbol -> [Symbol] -> Constraint
type family InvalidGotoTargetError target fieldNames where
  InvalidGotoTargetError target _ = DelayedTypeError
    ( 'Text "Graph validation failed: invalid Goto target"
      ':$$: 'Text "  Goto \"" ':<>: 'Text target ':<>: 'Text "\" references a node that doesn't exist."
      ':$$: 'Text "Fix: Add a field named \"" ':<>: 'Text target ':<>: 'Text "\" or use Goto Exit for termination."
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
type ValidGraphRecord :: (Type -> Type) -> Constraint
type ValidGraphRecord graph =
  ( Generic (graph AsGraph)
  , ValidateEntryExit graph
  , ValidateGotoTargets graph
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
gotoField
  :: forall (graph :: Type -> Type) (name :: Symbol) payload es.
     ( KnownSymbol name
     , Generic (graph AsGraph)
     , ElemC name (FieldNamesOf graph)
     , Goto name payload E.:> es
     )
  => payload
  -> E.Eff es ()
gotoField = goto @name
