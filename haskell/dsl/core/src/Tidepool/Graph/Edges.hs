{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

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

    -- * EntryNode/Exit Extraction
  , GetEntries
  , GetExits
  , HasEntries
  , HasExits

    -- * Graph-Level Extraction
  , GetGlobal
  , GetBackend

    -- * ClaudeCode Extraction
  , GetClaudeCode
  , HasClaudeCode

    -- * Gemini Extraction
  , GetGeminiModel
  , HasGeminiModel

    -- * Goto Extraction
  , GetGotoTargets
  , GotoEffectsToTargets
  , GotosToTos
  , HasGotoExit
  , ExtractGotoTarget
  , ExtractGotoPayload

    -- * Fork/Barrier Extraction
  , GetSpawnTargets
  , GetBarrierTarget
  , GetAwaits
  , HasArrive
  , GetArriveType

    -- * Node Queries
  , HasAnnotation
  , FindAnnotation

    -- * MCP Export Detection
  , HasMCPExport
  , GetMCPToolDef
  , HasMCPToolDef
  , GetMCPEntries
  ) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Tidepool.Graph.Types
  ( type (:@), type (:&)
  , Input, Schema, System, Template, Vision, Tools, UsesEffects, Memory
  , MCPExport, MCPToolDef
  , Spawn, Barrier, Awaits, Arrive
  , Global, Backend
  , ClaudeCode, ModelChoice
  , Gemini, GeminiModel
  , Self
  , Entries, Exits
  )
import qualified Tidepool.Graph.Types as Types
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
--
-- NOTE: Tools changed from type-level list to record type in Phase 1.
-- Old: Tools '[SearchTool, CalcTool]
-- New: Tools MyToolsRecord (where MyToolsRecord :: Type -> Type)
--
-- Returns the tools record constructor (kind: Type -> Type).
type GetTools :: Type -> Maybe (Type -> Type)
type family GetTools node where
  GetTools (node :@ Tools record) = 'Just record
  GetTools (node :@ _) = GetTools node
  GetTools _ = 'Nothing

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
-- ENTRY/EXIT EXTRACTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract Entries record type from a node declaration.
--
-- Returns the record type that defines named entry points for this node.
--
-- @
-- GetEntries (LLMNode :@ Entries WorkEntries :@ Template WorkTpl)
--   = 'Just WorkEntries
--
-- GetEntries (LLMNode :@ Input TaskSpec :@ Schema Result)
--   = 'Nothing  -- Old-style Input, no Entries
-- @
--
-- NOTE: Returns record constructor (kind: Type -> Type).
type GetEntries :: Type -> Maybe (Type -> Type)
type family GetEntries node where
  GetEntries (node :@ Entries record) = 'Just record
  GetEntries (node :@ _) = GetEntries node
  GetEntries _ = 'Nothing

-- | Extract Exits record type from a node declaration.
--
-- Returns the record type that defines named exit points for this node.
--
-- @
-- GetExits (LLMNode :@ Exits WorkExits :@ Template WorkTpl)
--   = 'Just WorkExits
--
-- GetExits (LLMNode :@ Schema Result :@ Template WorkTpl)
--   = 'Nothing  -- Old-style Schema, no Exits
-- @
--
-- NOTE: Returns record constructor (kind: Type -> Type).
type GetExits :: Type -> Maybe (Type -> Type)
type family GetExits node where
  GetExits (node :@ Exits record) = 'Just record
  GetExits (node :@ _) = GetExits node
  GetExits _ = 'Nothing

-- | Check if a node has Entries annotation.
--
-- @
-- HasEntries (LLMNode :@ Entries WorkEntries :@ Template WorkTpl)
--   = 'True
--
-- HasEntries (LLMNode :@ Input TaskSpec :@ Schema Result)
--   = 'False
-- @
type HasEntries :: Type -> Bool
type family HasEntries node where
  HasEntries node = IsJust (GetEntries node)

-- | Check if a node has Exits annotation.
--
-- @
-- HasExits (LLMNode :@ Exits WorkExits :@ Template WorkTpl)
--   = 'True
--
-- HasExits (LLMNode :@ Schema Result :@ Template WorkTpl)
--   = 'False
-- @
type HasExits :: Type -> Bool
type family HasExits node where
  HasExits node = IsJust (GetExits node)

-- ════════════════════════════════════════════════════════════════════════════
-- GOTO EXTRACTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract all Goto targets from an effect list.
--
-- Returns a list of (target, payload) type pairs for named node targets.
-- 'Goto Types.Exit' transitions are excluded (handled separately by 'HasGotoExit').
--
-- = Pattern Matching Strategy
--
-- The type family uses three distinct patterns:
--
-- 1. @Goto (name :: Symbol) payload@ -- Named node targets
--
--    - The @:: Symbol@ annotation disambiguates from @Goto Types.Exit@
--    - Yields @'(name, payload)@ pair
--
-- 2. @Goto Types.Exit payload@ -- Exit transitions (skipped)
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
-- GetGotoTargets '[State S, Goto \"foo\" A, Log, Goto \"bar\" B, Goto Types.Exit R]
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
  GetGotoTargets (Goto Types.Exit payload ': rest) =
    -- Exit is handled specially, not included in named targets
    GetGotoTargets rest
  GetGotoTargets (_ ': rest) = GetGotoTargets rest

-- | Convert Goto effects to To markers for GotoChoice.
--
-- @
-- GotoEffectsToTargets '[State S, Goto "foo" A, Goto Types.Exit B, Goto Self C, Arrive "hJoin" R]
--   = '[To "foo" A, To Types.Exit B, To Self C, To (Arrive "hJoin") R]
-- @
type GotoEffectsToTargets :: forall k. [k] -> [Type]
type family GotoEffectsToTargets effs where
  GotoEffectsToTargets '[] = '[]
  GotoEffectsToTargets (Goto (name :: Symbol) payload ': rest) =
    To name payload ': GotoEffectsToTargets rest
  GotoEffectsToTargets (Goto Types.Exit payload ': rest) =
    To Types.Exit payload ': GotoEffectsToTargets rest
  GotoEffectsToTargets (Goto Self payload ': rest) =
    To Self payload ': GotoEffectsToTargets rest
  GotoEffectsToTargets (Arrive barrierName result ': rest) =
    To (Arrive barrierName) result ': GotoEffectsToTargets rest
  GotoEffectsToTargets (_ ': rest) = GotoEffectsToTargets rest

-- | Alias for 'GotoEffectsToTargets' for use in handler signatures.
--
-- When you have a type alias for your graph's UsesEffects:
--
-- @
-- type MyEffects = '[Goto "process" Data, Goto "fallback" Data, Goto Types.Exit Result]
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

-- | Check if effect list contains Goto Types.Exit.
type HasGotoExit :: forall k. [k] -> Bool
type family HasGotoExit effs where
  HasGotoExit '[] = 'False
  HasGotoExit (Goto Types.Exit _ ': _) = 'True
  HasGotoExit (_ ': rest) = HasGotoExit rest

-- | Get the Exit payload type if present.
type GetGotoExitPayload :: forall k. [k] -> Maybe Type
type family GetGotoExitPayload effs where
  GetGotoExitPayload '[] = 'Nothing
  GetGotoExitPayload (Goto Types.Exit payload ': _) = 'Just payload
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
  SameAnnotationType (Entries _) (Entries _) = 'True
  SameAnnotationType (Exits _) (Exits _) = 'True
  SameAnnotationType Vision Vision = 'True
  SameAnnotationType (ClaudeCode _) (ClaudeCode _) = 'True
  SameAnnotationType (Gemini _) (Gemini _) = 'True
  SameAnnotationType (Backend _) (Backend _) = 'True
  SameAnnotationType (Spawn _) (Spawn _) = 'True
  SameAnnotationType (Barrier _) (Barrier _) = 'True
  SameAnnotationType (Awaits _) (Awaits _) = 'True
  SameAnnotationType (Arrive _ _ _) (Arrive _ _ _) = 'True
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
-- Returns the model choice.
--
-- @
-- GetClaudeCode (LLMNode :@ Schema Result :@ ClaudeCode 'Sonnet) = 'Just 'Sonnet
-- @
type GetClaudeCode :: Type -> Maybe ModelChoice
type family GetClaudeCode node where
  GetClaudeCode (node :@ ClaudeCode m) = 'Just m
  GetClaudeCode (node :@ _) = GetClaudeCode node
  GetClaudeCode _ = 'Nothing

-- | Check if a node has the ClaudeCode annotation.
--
-- @
-- HasClaudeCode (LLMNode :@ ClaudeCode 'Haiku) = 'True
-- HasClaudeCode (LLMNode :@ Schema Result) = 'False
-- @
type HasClaudeCode :: Type -> Bool
type family HasClaudeCode node where
  HasClaudeCode (node :@ ClaudeCode _) = 'True
  HasClaudeCode (node :@ _) = HasClaudeCode node
  HasClaudeCode _ = 'False

-- ════════════════════════════════════════════════════════════════════════════
-- GEMINI EXTRACTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract Gemini annotation from a node.
--
-- Returns the model choice.
type GetGeminiModel :: Type -> Maybe GeminiModel
type family GetGeminiModel node where
  GetGeminiModel (node :@ Gemini m) = 'Just m
  GetGeminiModel (node :@ _) = GetGeminiModel node
  GetGeminiModel _ = 'Nothing

-- | Check if a node has the Gemini annotation.
type HasGeminiModel :: Type -> Bool
type family HasGeminiModel node where
  HasGeminiModel (node :@ Gemini _) = 'True
  HasGeminiModel (node :@ _) = HasGeminiModel node
  HasGeminiModel _ = 'False

-- ════════════════════════════════════════════════════════════════════════════
-- FORK/BARRIER EXTRACTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract Spawn targets from a ForkNode declaration.
--
-- @
-- GetSpawnTargets (ForkNode :@ Input Task :@ Spawn '[To "w1" A, To "w2" B] :@ Barrier "merge")
--   = '[To "w1" A, To "w2" B]
-- @
type GetSpawnTargets :: Type -> [Type]
type family GetSpawnTargets node where
  GetSpawnTargets (node :@ Spawn targets) = targets
  GetSpawnTargets (node :@ _) = GetSpawnTargets node
  GetSpawnTargets _ = '[]

-- | Extract Barrier target name from a ForkNode declaration.
--
-- @
-- GetBarrierTarget (ForkNode :@ Spawn '[...] :@ Barrier "merge")
--   = 'Just "merge"
-- @
type GetBarrierTarget :: Type -> Maybe Symbol
type family GetBarrierTarget node where
  GetBarrierTarget (node :@ Barrier target) = 'Just target
  GetBarrierTarget (node :@ _) = GetBarrierTarget node
  GetBarrierTarget _ = 'Nothing

-- | Extract Awaits types from a BarrierNode declaration.
--
-- @
-- GetAwaits (BarrierNode :@ Awaits '[ResultA, ResultB] :@ UsesEffects '[...])
--   = '[ResultA, ResultB]
-- @
type GetAwaits :: Type -> [Type]
type family GetAwaits node where
  GetAwaits (node :@ Awaits types) = types
  GetAwaits (node :@ _) = GetAwaits node
  GetAwaits _ = '[]

-- | Check if an effect list contains Arrive.
--
-- @
-- HasArrive '[Goto Self Task, Arrive "hJoin" Result] = 'True
-- HasArrive '[Goto Self Task, Goto Types.Exit Result] = 'False
-- @
type HasArrive :: forall k. [k] -> Bool
type family HasArrive effs where
  HasArrive '[] = 'False
  HasArrive (Arrive _ _ ': _) = 'True
  HasArrive (_ ': rest) = HasArrive rest

-- | Extract the Arrive result type from an effect list.
--
-- @
-- GetArriveType '[Goto Self Task, Arrive "hJoin" Result] = 'Just Result
-- GetArriveType '[Goto Self Task, Goto Types.Exit Result] = 'Nothing
-- @
type GetArriveType :: forall k. [k] -> Maybe Type
type family GetArriveType effs where
  GetArriveType '[] = 'Nothing
  GetArriveType (Arrive _ result ': _) = 'Just result
  GetArriveType (_ ': rest) = GetArriveType rest

-- ════════════════════════════════════════════════════════════════════════════
-- MCP EXPORT DETECTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if a node definition has MCPExport annotation.
--
-- MCPExport can appear at any position in the annotation chain:
--
-- @
-- HasMCPExport (EntryNode SearchInput :@ MCPExport) = 'True
-- HasMCPExport (EntryNode SearchInput :@ MCPExport :@ ToolDef '("search", "desc")) = 'True
-- HasMCPExport (EntryNode SearchInput :@ ToolDef '("search", "desc") :@ MCPExport) = 'True
-- HasMCPExport (EntryNode SearchInput) = 'False
-- @
--
-- The type family recursively strips annotations to handle all orderings.
type HasMCPExport :: Type -> Bool
type family HasMCPExport node where
  -- Direct match: MCPExport is the annotation
  HasMCPExport (node :@ MCPExport) = 'True
  -- MCPExport with one more annotation after it
  HasMCPExport (node :@ MCPExport :@ _) = 'True
  -- Recursively strip other annotations
  HasMCPExport (node :@ _) = HasMCPExport node
  -- Base case: no MCPExport found
  HasMCPExport _ = 'False

-- | Extract ToolDef annotation if present.
--
-- Returns the type-level tuple (name, description) from the MCPToolDef annotation.
-- Works with both EntryNode and LLMNode.
--
-- @
-- GetMCPToolDef (EntryNode X :@ MCPExport :@ MCPToolDef '("name", "desc"))
--   = 'Just '("name", "desc")
--
-- GetMCPToolDef (LLMNode :@ Input Query :@ MCPToolDef '("name", "desc"))
--   = 'Just '("name", "desc")
--
-- GetMCPToolDef (EntryNode X :@ MCPExport)
--   = 'Nothing
-- @
type GetMCPToolDef :: Type -> Maybe (Symbol, Symbol)
type family GetMCPToolDef node where
  -- Direct match: MCPToolDef is the annotation
  GetMCPToolDef (node :@ MCPToolDef meta) = 'Just meta
  -- Recursively strip other annotations
  GetMCPToolDef (node :@ _) = GetMCPToolDef node
  -- Base case: no MCPToolDef found
  GetMCPToolDef _ = 'Nothing

-- | Check if node has MCPToolDef annotation.
--
-- Helper predicate that returns True if the node has a MCPToolDef annotation,
-- False otherwise.
--
-- @
-- HasMCPToolDef (LLMNode :@ Input Query :@ Schema Response :@ MCPToolDef '("scout", "desc"))
--   = 'True
--
-- HasMCPToolDef (LLMNode :@ Input Query :@ Schema Response)
--   = 'False
-- @
type HasMCPToolDef :: Type -> Bool
type family HasMCPToolDef node where
  HasMCPToolDef (node :@ MCPToolDef _) = 'True
  HasMCPToolDef (node :@ _) = HasMCPToolDef node
  HasMCPToolDef _ = 'False

-- | Collect all MCP-exported EntryNode field names from a graph.
--
-- Returns a type-level list of (fieldName, inputType) tuples for EntryNode nodes
-- that have MCPExport annotation. Used by ReifyMCPTools to generate tool
-- definitions.
--
-- NOTE: Full implementation requires a typeclass-based approach due to GHC's
-- limitation on pattern matching type family applications. The `:-` mode operator
-- is a type family, and type families cannot pattern match on applications of
-- other type families. This will be implemented as a ReifyMCPTools typeclass
-- in Stream B (05-mcp-reify.md).
--
-- For now, this type family provides the signature that downstream code can
-- reference. The actual collection happens at runtime via Generic reification.
--
-- @
-- data MyGraph mode = MyGraph
--   { gEntry  :: mode :- EntryNode Input
--   , gSearch :: mode :- EntryNode SearchInput :@ MCPExport :@ ToolDef '("search", "desc")
--   , gCalc   :: mode :- EntryNode CalcInput :@ MCPExport
--   , gExit   :: mode :- ExitNode Output
--   }
--
-- -- Desired (implemented via typeclass in Stream B):
-- GetMCPEntries MyGraph = '[ '("gSearch", SearchInput), '("gCalc", CalcInput) ]
-- @
type GetMCPEntries :: (Type -> Type) -> [(Symbol, Type)]
type family GetMCPEntries graph where
  GetMCPEntries g = '[]  -- Placeholder: actual implementation in Stream B via typeclass


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

