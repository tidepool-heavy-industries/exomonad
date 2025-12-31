{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Annotation extraction for the Graph DSL.
--
-- This module provides type families to extract information from node annotations.
-- These work with both list-based and record-based graph definitions.
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
  , GotoEffectsToTargets
  , HasGotoExit
  , ExtractGotoTarget
  , ExtractGotoPayload

    -- * Node Queries
  , HasAnnotation
  , FindAnnotation
  ) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Tidepool.Graph.Types
import Tidepool.Graph.Goto (Goto, To)

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
-- GetNeeds (LLMNode :@ Needs '[A, B] :@ Schema C)
--   = '[A, B]
-- @
type GetNeeds :: Type -> [Type]
type family GetNeeds node where
  GetNeeds (node :@ Needs ts) = ts
  GetNeeds (node :@ _) = GetNeeds node
  GetNeeds _ = '[]

-- | Extract the Schema output type from a node declaration.
--
-- @
-- GetSchema (LLMNode :@ Needs '[A] :@ Schema B)
--   = 'Just B
-- @
type GetSchema :: Type -> Maybe Type
type family GetSchema node where
  GetSchema (node :@ Schema t) = 'Just t
  GetSchema (node :@ _) = GetSchema node
  GetSchema _ = 'Nothing

-- | Extract the UsesEffects stack from a Logic node.
--
-- @
-- GetUsesEffects (LogicNode :@ Needs '[A] :@ UsesEffects '[State S, Goto "bar" B])
--   = 'Just '[State S, Goto "bar" B]
-- @
--
-- Note: The effect list can have any kind (usually Effect).
type GetUsesEffects :: forall k. Type -> Maybe [k]
type family GetUsesEffects node where
  GetUsesEffects (node :@ UsesEffects effs) = 'Just effs
  GetUsesEffects (node :@ _) = GetUsesEffects node
  GetUsesEffects _ = 'Nothing

-- | Extract the System template type from a node.
--
-- @
-- GetSystem (LLMNode :@ System SysTpl :@ Template UserTpl :@ Schema Intent)
--   = 'Just SysTpl
-- @
type GetSystem :: Type -> Maybe Type
type family GetSystem node where
  GetSystem (node :@ System t) = 'Just t
  GetSystem (node :@ _) = GetSystem node
  GetSystem _ = 'Nothing

-- | Extract the Template type from a node.
type GetTemplate :: Type -> Maybe Type
type family GetTemplate node where
  GetTemplate (node :@ Template t) = 'Just t
  GetTemplate (node :@ _) = GetTemplate node
  GetTemplate _ = 'Nothing

-- | Check if a node has Vision.
type GetVision :: Type -> Bool
type family GetVision node where
  GetVision (node :@ Vision) = 'True
  GetVision (node :@ _) = GetVision node
  GetVision _ = 'False

-- | Extract Tools from a node.
type GetTools :: Type -> [Type]
type family GetTools node where
  GetTools (node :@ Tools ts) = ts
  GetTools (node :@ _) = GetTools node
  GetTools _ = '[]

-- | Extract the Memory type from a node declaration.
--
-- @
-- GetMemory (LLMNode :@ Schema Findings :@ Memory ExploreMem)
--   = 'Just ExploreMem
-- @
type GetMemory :: Type -> Maybe Type
type family GetMemory node where
  GetMemory (node :@ Memory t) = 'Just t
  GetMemory (node :@ _) = GetMemory node
  GetMemory _ = 'Nothing

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

-- | Convert Goto effects to To markers for GotoChoice.
--
-- @
-- GotoEffectsToTargets '[State S, Goto "foo" A, Goto Exit B, Goto Self C]
--   = '[To "foo" A, To Exit B, To Self C]
-- @
type GotoEffectsToTargets :: forall k. [k] -> [Type]
type family GotoEffectsToTargets effs where
  GotoEffectsToTargets '[] = '[]
  GotoEffectsToTargets (Goto (name :: Symbol) payload ': rest) =
    To name payload ': GotoEffectsToTargets rest
  GotoEffectsToTargets (Goto Exit payload ': rest) =
    To Exit payload ': GotoEffectsToTargets rest
  GotoEffectsToTargets (Goto Self payload ': rest) =
    To Self payload ': GotoEffectsToTargets rest
  GotoEffectsToTargets (_ ': rest) = GotoEffectsToTargets rest

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
  HasAnnotation (node :@ ann) annType =
    Or (SameAnnotationType ann annType) (HasAnnotation node annType)
  HasAnnotation _ _ = 'False

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
  FindAnnotation (node :@ ann) annType =
    If (SameAnnotationType ann annType)
       ('Just ann)
       (FindAnnotation node annType)
  FindAnnotation _ _ = 'Nothing

-- | Get the Global state type from a graph-level annotation.
--
-- @
-- GetGlobal (graph :& Global SessionState)
--   = 'Just SessionState
-- @
type GetGlobal :: Type -> Maybe Type
type family GetGlobal graph where
  GetGlobal (graph :& Global t) = 'Just t
  GetGlobal (graph :& _) = GetGlobal graph
  GetGlobal _ = 'Nothing

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
