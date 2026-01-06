{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Annotation extraction for the Graph DSL.
--
-- This module provides type families to extract information from node annotations.
-- These type families power the record-based (Servant-style) graph definitions.
module Tidepool.Graph.Edges
  ( -- * Edge Type
    EdgeKind(..)

    -- * Annotation Extraction
  , GetInput
  , GetSchema
  , GetUsesEffects
  , GetSystem
  , GetTemplate
  , GetVision
  , GetTools
  , GetMemory

    -- * Graph-Level Extraction
  , GetGlobal
  , GetBackend

    -- * ClaudeCode Extraction
  , GetClaudeCode
  , HasClaudeCode

    -- * Goto Extraction
  , GetGotoTargets
  , GotoEffectsToTargets
  , GotosToTos
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
  ( type (:@), type (:&)
  , Input, Schema, System, Template, Vision, Tools, UsesEffects, Memory
  , Global, Backend
  , ClaudeCode, ModelChoice
  , Exit, Self
  )
import Tidepool.Graph.Goto (Goto, To)

-- ════════════════════════════════════════════════════════════════════════════
-- EDGE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Classification of edges for Mermaid rendering.
data EdgeKind
  = ImplicitEdge      -- ^ Data flow via Schema → Input (solid arrow)
  | ExplicitEdge      -- ^ Transition via Goto (solid arrow)
  deriving (Show, Eq)

-- ════════════════════════════════════════════════════════════════════════════
-- ANNOTATION EXTRACTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract the Input type from a node declaration.
--
-- @
-- GetInput (LLMNode :@ Input A :@ Schema B)
--   = 'Just A
-- @
type GetInput :: Type -> Maybe Type
type family GetInput node where
  GetInput (node :@ Input t) = 'Just t
  GetInput (node :@ _) = GetInput node
  GetInput _ = 'Nothing

-- | Extract the Schema output type from a node declaration.
--
-- @
-- GetSchema (LLMNode :@ Input A :@ Schema B)
--   = 'Just B
-- @
type GetSchema :: Type -> Maybe Type
type family GetSchema node where
  GetSchema (node :@ Schema t) = 'Just t
  GetSchema (node :@ _) = GetSchema node
  GetSchema _ = 'Nothing

-- | Extract the UsesEffects stack from a Logic node.
--
-- = Polykind Design
--
-- Uses @forall k@ to support polykinded effect lists. This is critical because:
--
-- 1. The 'Effect' kind is @(Type -> Type) -> Type -> Type@, not @Type@
-- 2. Without polykind quantification, GHC cannot determine the kind of @effs@
-- 3. Pattern matching on @Goto@ effects requires knowing the list has kind @[Effect]@
--
-- = How It Works
--
-- The type variable @k@ remains polymorphic, enabling this family to work with:
--
-- - @UsesEffects '[State S, Goto \"foo\" A]@ -- kind @[Effect]@
-- - Future annotations with different kinds
--
-- This flexibility is essential for downstream type families like 'GetGotoTargets'
-- (line 154) which need to pattern match on individual effects.
--
-- = Usage Example
--
-- @
-- GetUsesEffects (LogicNode :@ Input A :@ UsesEffects '[State S, Goto \"bar\" B])
--   = 'Just '[State S, Goto \"bar\" B]  -- kind: Maybe [Effect]
-- @
--
-- = Why This Matters
--
-- Downstream type families like 'GotoTargetsFromDef' in Generic.hs apply @\@Effect@
-- explicitly to resolve kind ambiguity:
--
-- @
-- type GotoTargetsFromDef def = GotoTargetsFromEffects (GetUsesEffects \@Effect def)
-- @
--
-- Without polykind here, that explicit kind application would fail.
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
-- Returns a list of (target, payload) type pairs for named node targets.
-- 'Goto Exit' transitions are excluded (handled separately by 'HasGotoExit').
--
-- = Pattern Matching Strategy
--
-- The type family uses three distinct patterns:
--
-- 1. @Goto (name :: Symbol) payload@ -- Named node targets
--
--    - The @:: Symbol@ annotation disambiguates from @Goto Exit@
--    - Yields @'(name, payload)@ pair
--
-- 2. @Goto Exit payload@ -- Exit transitions (skipped)
--
--    - Not included in named targets
--    - See 'GetGotoExitPayload' for exit handling
--
-- 3. @_ ': rest@ -- All other effects
--
--    - Recursively process the rest of the list
--
-- = Example
--
-- @
-- GetGotoTargets '[State S, Goto \"foo\" A, Log, Goto \"bar\" B, Goto Exit R]
--   = '[ '(\"foo\", A), '(\"bar\", B) ]
--   -- Exit is excluded, non-Goto effects are skipped
-- @
--
-- = Polykind Context
--
-- Note the @forall k@ on line 183. This allows the effect list to have kind @[Effect]@
-- while still pattern matching on individual effects. The polykind is inherited from
-- 'GetUsesEffects' (line 119) which initially extracts the effect list.
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

-- | Alias for 'GotoEffectsToTargets' for use in handler signatures.
--
-- When you have a type alias for your graph's UsesEffects:
--
-- @
-- type MyEffects = '[Goto "process" Data, Goto "fallback" Data, Goto Exit Result]
-- @
--
-- You can derive the handler return type without duplicating the list:
--
-- @
-- myHandler :: Input -> Eff es (GotoChoice (GotosToTos MyEffects))
-- myHandler input = case classify input of
--   Process x -> pure $ gotoChoice @"process" x
--   Fallback x -> pure $ gotoChoice @"fallback" x
--   Done r -> pure $ gotoExit r
-- @
--
-- This eliminates the need for parallel type aliases like:
--
-- @
-- -- Before: Two aliases that must stay in sync
-- type MyGotos = '[Goto "a" A, Goto "b" B]    -- for UsesEffects
-- type MyTargets = '[To "a" A, To "b" B]      -- for GotoChoice
--
-- -- After: One alias, derive the other
-- type MyGotos = '[Goto "a" A, Goto "b" B]
-- -- GotosToTos MyGotos = '[To "a" A, To "b" B]
-- @
type GotosToTos :: forall k. [k] -> [Type]
type GotosToTos effs = GotoEffectsToTargets effs

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
  SameAnnotationType (Input _) (Input _) = 'True
  SameAnnotationType (Schema _) (Schema _) = 'True
  SameAnnotationType (System _) (System _) = 'True
  SameAnnotationType (Template _) (Template _) = 'True
  SameAnnotationType (Tools _) (Tools _) = 'True
  SameAnnotationType (UsesEffects _) (UsesEffects _) = 'True
  SameAnnotationType (Memory _) (Memory _) = 'True
  SameAnnotationType Vision Vision = 'True
  SameAnnotationType (ClaudeCode _ _) (ClaudeCode _ _) = 'True
  SameAnnotationType (Backend _) (Backend _) = 'True
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

-- | Get the Backend type from a graph-level annotation.
--
-- @
-- GetBackend (graph :& Backend NativeAnthropic)
--   = 'Just NativeAnthropic
-- @
type GetBackend :: Type -> Maybe Type
type family GetBackend graph where
  GetBackend (graph :& Backend t) = 'Just t
  GetBackend (graph :& _) = GetBackend graph
  GetBackend _ = 'Nothing

-- ════════════════════════════════════════════════════════════════════════════
-- CLAUDE CODE EXTRACTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract ClaudeCode annotation from a node.
--
-- Returns the model and cwd as a type-level tuple.
--
-- @
-- GetClaudeCode (LLMNode :@ Schema Result :@ ClaudeCode 'Sonnet ('Just "/path"))
--   = 'Just '( 'Sonnet, 'Just "/path")
-- @
type GetClaudeCode :: Type -> Maybe (ModelChoice, Maybe Symbol)
type family GetClaudeCode node where
  GetClaudeCode (node :@ ClaudeCode m c) = 'Just '(m, c)
  GetClaudeCode (node :@ _) = GetClaudeCode node
  GetClaudeCode _ = 'Nothing

-- | Check if a node has the ClaudeCode annotation.
--
-- @
-- HasClaudeCode (LLMNode :@ ClaudeCode 'Haiku 'Nothing) = 'True
-- HasClaudeCode (LLMNode :@ Schema Result) = 'False
-- @
type HasClaudeCode :: Type -> Bool
type family HasClaudeCode node where
  HasClaudeCode (node :@ ClaudeCode _ _) = 'True
  HasClaudeCode (node :@ _) = HasClaudeCode node
  HasClaudeCode _ = 'False

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
