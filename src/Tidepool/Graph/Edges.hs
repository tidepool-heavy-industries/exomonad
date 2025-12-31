{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Edge derivation for the Graph DSL.
--
-- Edges are not declared explicitly in the DSL. Instead, they are derived
-- automatically from two sources:
--
-- 1. __Implicit edges__ (data flow): When a node's 'Schema' output matches
--    another node's 'Needs' input, an edge is created.
--
-- 2. __Explicit edges__ (transitions): Each 'Goto' effect in a Logic node's
--    'Eff' stack creates an edge to the target node.
--
-- = Edge Derivation Algorithm
--
-- For each node N in the graph:
--
-- * If N has @Schema T@, find all nodes M where @T ∈ Needs M@
--   → Create implicit edge @N → M@ carrying @T@
--
-- * If N has @Eff '[..., Goto "target" T, ...]@
--   → Create explicit edge @N → "target"@ carrying @T@
--
-- Additionally:
--
-- * Entry provides the graph input type to all nodes that need it
module Tidepool.Graph.Edges
  ( -- * Edge Type
    EdgeKind(..)

    -- * Annotation Extraction
  , GetNeeds
  , GetSchema
  , GetUsesEffects
  , GetSystem
  , GetTemplate
  , GetVision
  , GetTools
  , GetMemory

    -- * Graph-Level Extraction
  , GetGlobal

    -- * Goto Extraction
  , GetGotoTargets
  , ExtractGotoTarget
  , ExtractGotoPayload

    -- * Node Queries
  , HasAnnotation
  , FindAnnotation

    -- * Graph Queries
  , GetNodes
  , GetEntryType
  , GetExitType
  , FindNodeByName
  , FindProducers
  ) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Tidepool.Graph.Types
import Tidepool.Graph.Goto (Goto)

-- ════════════════════════════════════════════════════════════════════════════
-- EDGE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Classification of edges for Mermaid rendering.
data EdgeKind
  = ImplicitEdge      -- ^ Data flow via Schema → Needs (solid arrow)
  | ExplicitEdge      -- ^ Transition via Goto (solid arrow)
  deriving (Show, Eq)

-- ════════════════════════════════════════════════════════════════════════════
-- ANNOTATION EXTRACTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract the Needs types from a node declaration.
--
-- @
-- GetNeeds ("foo" := LLM :@ Needs '[A, B] :@ Schema C)
--   = '[A, B]
-- @
type GetNeeds :: Type -> [Type]
type family GetNeeds node where
  GetNeeds (_ := _) = '[]
  GetNeeds (node :@ Needs ts) = ts
  GetNeeds (node :@ _) = GetNeeds node

-- | Extract the Schema output type from a node declaration.
--
-- @
-- GetSchema ("foo" := LLM :@ Needs '[A] :@ Schema B)
--   = 'Just B
-- @
type GetSchema :: Type -> Maybe Type
type family GetSchema node where
  GetSchema (_ := _) = 'Nothing
  GetSchema (node :@ Schema t) = 'Just t
  GetSchema (node :@ _) = GetSchema node

-- | Extract the UsesEffects stack from a Logic node.
--
-- @
-- GetUsesEffects ("foo" := Logic :@ Needs '[A] :@ UsesEffects '[State S, Goto "bar" B])
--   = 'Just '[State S, Goto "bar" B]
-- @
--
-- Note: The effect list can have any kind (usually Effect).
type GetUsesEffects :: forall k. Type -> Maybe [k]
type family GetUsesEffects node where
  GetUsesEffects (_ := _) = 'Nothing
  GetUsesEffects (node :@ UsesEffects effs) = 'Just effs
  GetUsesEffects (node :@ _) = GetUsesEffects node

-- | Extract the System template type from a node.
--
-- @
-- GetSystem ("classify" := LLM :@ System SysTpl :@ Template UserTpl :@ Schema Intent)
--   = 'Just SysTpl
-- @
type GetSystem :: Type -> Maybe Type
type family GetSystem node where
  GetSystem (_ := _) = 'Nothing
  GetSystem (node :@ System t) = 'Just t
  GetSystem (node :@ _) = GetSystem node

-- | Extract the Template type from a node.
type GetTemplate :: Type -> Maybe Type
type family GetTemplate node where
  GetTemplate (_ := _) = 'Nothing
  GetTemplate (node :@ Template t) = 'Just t
  GetTemplate (node :@ _) = GetTemplate node

-- | Check if a node has Vision.
type GetVision :: Type -> Bool
type family GetVision node where
  GetVision (_ := _) = 'False
  GetVision (node :@ Vision) = 'True
  GetVision (node :@ _) = GetVision node

-- | Extract Tools from a node.
type GetTools :: Type -> [Type]
type family GetTools node where
  GetTools (_ := _) = '[]
  GetTools (node :@ Tools ts) = ts
  GetTools (node :@ _) = GetTools node

-- | Extract the Memory type from a node declaration.
--
-- @
-- GetMemory ("explore" := LLM :@ Schema Findings :@ Memory ExploreMem)
--   = 'Just ExploreMem
-- @
type GetMemory :: Type -> Maybe Type
type family GetMemory node where
  GetMemory (_ := _) = 'Nothing
  GetMemory (node :@ Memory t) = 'Just t
  GetMemory (node :@ _) = GetMemory node

-- ════════════════════════════════════════════════════════════════════════════
-- GOTO EXTRACTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract all Goto targets from an effect list.
--
-- Returns a list of (target, payload) type pairs.
-- Note: This works with effectful's Effect kind.
--
-- @
-- GetGotoTargets '[State S, Goto "foo" A, Log, Goto "bar" B]
--   = '[ '("foo", A), '("bar", B) ]
-- @
type GetGotoTargets :: forall k. [k] -> [(Symbol, Type)]
type family GetGotoTargets effs where
  GetGotoTargets '[] = '[]
  GetGotoTargets (Goto (name :: Symbol) payload ': rest) =
    '(name, payload) ': GetGotoTargets rest
  GetGotoTargets (Goto Exit payload ': rest) =
    -- Exit is handled specially, not included in named targets
    GetGotoTargets rest
  GetGotoTargets (_ ': rest) = GetGotoTargets rest

-- | Extract the target from a Goto effect type.
type ExtractGotoTarget :: forall k. k -> Maybe Symbol
type family ExtractGotoTarget eff where
  ExtractGotoTarget (Goto (name :: Symbol) _) = 'Just name
  ExtractGotoTarget _ = 'Nothing

-- | Extract the payload type from a Goto effect.
type ExtractGotoPayload :: forall k. k -> Maybe Type
type family ExtractGotoPayload eff where
  ExtractGotoPayload (Goto _ payload) = 'Just payload
  ExtractGotoPayload _ = 'Nothing

-- | Check if effect list contains Goto Exit.
type HasGotoExit :: forall k. [k] -> Bool
type family HasGotoExit effs where
  HasGotoExit '[] = 'False
  HasGotoExit (Goto Exit _ ': _) = 'True
  HasGotoExit (_ ': rest) = HasGotoExit rest

-- | Get the Exit payload type if present.
type GetGotoExitPayload :: forall k. [k] -> Maybe Type
type family GetGotoExitPayload effs where
  GetGotoExitPayload '[] = 'Nothing
  GetGotoExitPayload (Goto Exit payload ': _) = 'Just payload
  GetGotoExitPayload (_ ': rest) = GetGotoExitPayload rest

-- ════════════════════════════════════════════════════════════════════════════
-- NODE QUERIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if a node has a specific annotation type.
type HasAnnotation :: Type -> Type -> Bool
type family HasAnnotation node annType where
  HasAnnotation (_ := _) _ = 'False
  HasAnnotation (node :@ ann) annType =
    Or (SameAnnotationType ann annType) (HasAnnotation node annType)

-- | Helper to check if annotation matches a type constructor.
type SameAnnotationType :: Type -> Type -> Bool
type family SameAnnotationType ann target where
  SameAnnotationType (Needs _) (Needs _) = 'True
  SameAnnotationType (Schema _) (Schema _) = 'True
  SameAnnotationType (System _) (System _) = 'True
  SameAnnotationType (Template _) (Template _) = 'True
  SameAnnotationType (Tools _) (Tools _) = 'True
  SameAnnotationType (UsesEffects _) (UsesEffects _) = 'True
  SameAnnotationType (Memory _) (Memory _) = 'True
  SameAnnotationType Vision Vision = 'True
  SameAnnotationType _ _ = 'False

-- | Find a specific annotation in a node.
type FindAnnotation :: Type -> Type -> Maybe Type
type family FindAnnotation node annType where
  FindAnnotation (_ := _) _ = 'Nothing
  FindAnnotation (node :@ ann) annType =
    If (SameAnnotationType ann annType)
       ('Just ann)
       (FindAnnotation node annType)

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH QUERIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract all node declarations from a graph.
--
-- Filters out Entry and Exit declarations.
type GetNodes :: Type -> [Type]
type family GetNodes graph where
  GetNodes (Graph nodes) = FilterNodes nodes
  GetNodes (graph :& _) = GetNodes graph

-- | Filter node list to only actual node declarations.
type FilterNodes :: [Type] -> [Type]
type family FilterNodes nodes where
  FilterNodes '[] = '[]
  FilterNodes ((Entry :~> _) ': rest) = FilterNodes rest
  FilterNodes ((Exit :<~ _) ': rest) = FilterNodes rest
  FilterNodes (node ': rest) = node ': FilterNodes rest

-- | Get the Entry input type from a graph.
type GetEntryType :: Type -> Type
type family GetEntryType graph where
  GetEntryType (Graph nodes) = FindEntryType nodes
  GetEntryType (graph :& _) = GetEntryType graph

type FindEntryType :: [Type] -> Type
type family FindEntryType nodes where
  FindEntryType ((Entry :~> t) ': _) = t
  FindEntryType (_ ': rest) = FindEntryType rest
  -- Note: Missing Entry will cause a type error here (stuck type family)

-- | Get the Exit output type from a graph.
type GetExitType :: Type -> Type
type family GetExitType graph where
  GetExitType (Graph nodes) = FindExitType nodes
  GetExitType (graph :& _) = GetExitType graph

type FindExitType :: [Type] -> Type
type family FindExitType nodes where
  FindExitType ((Exit :<~ t) ': _) = t
  FindExitType (_ ': rest) = FindExitType rest
  -- Note: Missing Exit will cause a type error here (stuck type family)

-- | Get the Global state type from a graph-level annotation.
--
-- @
-- GetGlobal (Graph '[...] :& Global SessionState)
--   = 'Just SessionState
-- @
type GetGlobal :: Type -> Maybe Type
type family GetGlobal graph where
  GetGlobal (Graph _) = 'Nothing
  GetGlobal (graph :& Global t) = 'Just t
  GetGlobal (graph :& _) = GetGlobal graph

-- | Find a node by its name.
type FindNodeByName :: [Type] -> Symbol -> Maybe Type
type family FindNodeByName nodes name where
  FindNodeByName '[] _ = 'Nothing
  FindNodeByName (node ': rest) name =
    If (NodeName node == name)
       ('Just node)
       (FindNodeByName rest name)

-- | Find all nodes that produce a given type via Schema.
--
-- Used for implicit edge derivation: Schema T → Needs T.
type FindProducers :: [Type] -> Type -> [Symbol]
type family FindProducers nodes t where
  FindProducers '[] _ = '[]
  FindProducers (node ': rest) t =
    If (ProducesType node t)
       (NodeName node ': FindProducers rest t)
       (FindProducers rest t)

-- | Check if a node produces a given type.
type ProducesType :: Type -> Type -> Bool
type family ProducesType node t where
  ProducesType node t = MaybeEq (GetSchema node) ('Just t)

-- | Type-level equality for Maybe Type.
type MaybeEq :: Maybe Type -> Maybe Type -> Bool
type family MaybeEq a b where
  MaybeEq 'Nothing 'Nothing = 'True
  MaybeEq ('Just a) ('Just a) = 'True
  MaybeEq _ _ = 'False

-- ════════════════════════════════════════════════════════════════════════════
-- TYPE-LEVEL UTILITIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Type-level If.
type If :: Bool -> k -> k -> k
type family If cond t f where
  If 'True  t _ = t
  If 'False _ f = f

-- | Type-level Or.
type Or :: Bool -> Bool -> Bool
type family Or a b where
  Or 'True _ = 'True
  Or _ 'True = 'True
  Or 'False 'False = 'False

-- | Type-level Maybe check.
type IsJust :: Maybe k -> Bool
type family IsJust m where
  IsJust 'Nothing = 'False
  IsJust ('Just _) = 'True

-- | Type-level list membership.
type Elem :: k -> [k] -> Bool
type family Elem x xs where
  Elem _ '[] = 'False
  Elem x (x ': _) = 'True
  Elem x (_ ': rest) = Elem x rest

-- | Type-level equality for Symbols.
type (==) :: Symbol -> Symbol -> Bool
type family a == b where
  a == a = 'True
  _ == _ = 'False
