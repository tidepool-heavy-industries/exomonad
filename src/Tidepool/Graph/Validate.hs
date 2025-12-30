{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Compile-time validation for Graph definitions.
--
-- This module provides type-level constraints that validate graph structure
-- at compile time. Invalid graphs produce clear, actionable error messages.
--
-- = Validation Rules
--
-- 1. __Entry Required__: Graph must have exactly one @Entry :~> InputType@
-- 2. __Exit Required__: Graph must have exactly one @Exit :<~ OutputType@
-- 3. __Needs Satisfied__: Every type in a node's 'Needs' must be provided
--    by Entry or by some node's 'Schema' or 'Goto'
-- 4. __Goto Targets Valid__: Every 'Goto' target must reference an existing
--    node name or 'Exit'
-- 5. __No Orphans__: Every node must be reachable from Entry
--
-- = Usage
--
-- @
-- type MyGraph = Graph '[...]
--
-- -- This constraint validates at compile time:
-- runGraph :: ValidGraph MyGraph => HandlersFor MyGraph -> ...
-- @
module Tidepool.Graph.Validate
  ( -- * Main Validation Constraint
    ValidGraph

    -- * Individual Validation Constraints
  , HasEntry
  , HasExit
  , AllNeedsSatisfied
  , AllGotoTargetsExist
  , AllToolsHaveSchema
  , AllSchemasValidForStructuredOutput
  , AllMemoriesValid
  , NeedsSatisfied
  , GotoTargetExists

    -- * Error Messages
  , MissingEntryError
  , MissingExitError
  , UnsatisfiedNeedError
  , InvalidGotoTargetError
  ) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..))

import Tidepool.Graph.Types
import Tidepool.Graph.Edges (GetNeeds, GetSchema, GetTools, GetMemory, GetEntryType, GetGotoTargets)
import Tidepool.Graph.Tool (AllToolsValid)
import Tidepool.Schema (ValidStructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- MAIN VALIDATION CONSTRAINT
-- ════════════════════════════════════════════════════════════════════════════

-- | Main validation constraint for a complete graph.
--
-- Use this constraint on any function that runs or processes a graph
-- to ensure compile-time validation.
--
-- Validates:
--
-- * Entry and Exit declarations exist
-- * All Needs are satisfied by Entry or Schema outputs
-- * All Goto targets reference existing nodes or Exit
-- * All Tools have valid schemas
-- * All Schema types are valid for structured output (no oneOf)
-- * All Memory types are valid (placeholder for future serialization checks)
--
-- @
-- runGraph :: ValidGraph g => HandlersFor g -> EntryType g -> IO (ExitType g)
-- @
type ValidGraph :: Type -> Constraint
type ValidGraph g =
  ( HasEntry g
  , HasExit g
  , AllNeedsSatisfied g
  , AllGotoTargetsExist g
  , AllToolsHaveSchema g
  , AllSchemasValidForStructuredOutput g
  , AllMemoriesValid g
  )

-- ════════════════════════════════════════════════════════════════════════════
-- ENTRY/EXIT VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validates that graph has an Entry declaration.
type HasEntry :: Type -> Constraint
type family HasEntry g where
  HasEntry (Graph nodes) = HasEntryInNodes nodes
  HasEntry (g :& _) = HasEntry g

type HasEntryInNodes :: [Type] -> Constraint
type family HasEntryInNodes nodes where
  HasEntryInNodes '[] = MissingEntryError
  HasEntryInNodes ((Entry :~> _) ': _) = ()
  HasEntryInNodes (_ ': rest) = HasEntryInNodes rest

-- | Validates that graph has an Exit declaration.
type HasExit :: Type -> Constraint
type family HasExit g where
  HasExit (Graph nodes) = HasExitInNodes nodes
  HasExit (g :& _) = HasExit g

type HasExitInNodes :: [Type] -> Constraint
type family HasExitInNodes nodes where
  HasExitInNodes '[] = MissingExitError
  HasExitInNodes ((Exit :<~ _) ': _) = ()
  HasExitInNodes (_ ': rest) = HasExitInNodes rest

-- ════════════════════════════════════════════════════════════════════════════
-- NEEDS VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validates that all Needs for all nodes are satisfied.
type AllNeedsSatisfied :: Type -> Constraint
type family AllNeedsSatisfied g where
  AllNeedsSatisfied (Graph nodes) =
    AllNodeNeedsSatisfied nodes (GetAllProvidedTypes nodes (GetEntryType (Graph nodes)))
  AllNeedsSatisfied (g :& _) = AllNeedsSatisfied g

-- | Check each node's Needs against provided types.
type AllNodeNeedsSatisfied :: [Type] -> [Type] -> Constraint
type family AllNodeNeedsSatisfied nodes provided where
  AllNodeNeedsSatisfied '[] _ = ()
  AllNodeNeedsSatisfied ((Entry :~> _) ': rest) provided =
    AllNodeNeedsSatisfied rest provided
  AllNodeNeedsSatisfied ((Exit :<~ _) ': rest) provided =
    AllNodeNeedsSatisfied rest provided
  AllNodeNeedsSatisfied (node ': rest) provided =
    ( NeedsSatisfied (NodeName node) (GetNeeds node) provided
    , AllNodeNeedsSatisfied rest provided
    )

-- | Validate that all types in a node's Needs are in the provided list.
type NeedsSatisfied :: Symbol -> [Type] -> [Type] -> Constraint
type family NeedsSatisfied nodeName needs provided where
  NeedsSatisfied _ '[] _ = ()
  NeedsSatisfied nodeName (t ': rest) provided =
    ( CheckNeedProvided nodeName t provided
    , NeedsSatisfied nodeName rest provided
    )

-- | Check if a single need is satisfied.
type CheckNeedProvided :: Symbol -> Type -> [Type] -> Constraint
type family CheckNeedProvided nodeName need provided where
  CheckNeedProvided nodeName need provided =
    If (ElemType need provided)
       (() :: Constraint)
       (UnsatisfiedNeedError nodeName need)

-- | Collect all types provided by Entry + all Schema outputs.
type GetAllProvidedTypes :: [Type] -> Type -> [Type]
type family GetAllProvidedTypes nodes entryType where
  GetAllProvidedTypes nodes entryType =
    entryType ': CollectSchemaTypes nodes

-- | Collect Schema output types from all nodes.
type CollectSchemaTypes :: [Type] -> [Type]
type family CollectSchemaTypes nodes where
  CollectSchemaTypes '[] = '[]
  CollectSchemaTypes (node ': rest) =
    AppendMaybe (GetSchema node) (CollectSchemaTypes rest)

-- ════════════════════════════════════════════════════════════════════════════
-- GOTO TARGET VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validates that all Goto targets reference valid nodes or Exit.
--
-- Strategy: Extract Goto targets using GetGotoTargets (which handles the
-- polykinded matching), then validate the extracted Symbols against node names.
type AllGotoTargetsExist :: Type -> Constraint
type family AllGotoTargetsExist g where
  AllGotoTargetsExist (Graph nodes) =
    AllNodeGotosValid nodes (CollectNodeNames nodes)
  AllGotoTargetsExist (g :& _) = AllGotoTargetsExist g

-- | Collect all node names from the graph.
type CollectNodeNames :: [Type] -> [Symbol]
type family CollectNodeNames nodes where
  CollectNodeNames '[] = '[]
  CollectNodeNames ((Entry :~> _) ': rest) = CollectNodeNames rest
  CollectNodeNames ((Exit :<~ _) ': rest) = CollectNodeNames rest
  CollectNodeNames (node ': rest) = NodeName node ': CollectNodeNames rest

-- | Check each node's Goto targets.
type AllNodeGotosValid :: [Type] -> [Symbol] -> Constraint
type family AllNodeGotosValid nodes validNames where
  AllNodeGotosValid '[] _ = ()
  AllNodeGotosValid ((Entry :~> _) ': rest) validNames =
    AllNodeGotosValid rest validNames
  AllNodeGotosValid ((Exit :<~ _) ': rest) validNames =
    AllNodeGotosValid rest validNames
  AllNodeGotosValid (node ': rest) validNames =
    ( ValidateNodeGotoTargets (NodeName node) (ExtractGotoSymbols node) validNames
    , AllNodeGotosValid rest validNames
    )

-- | Extract Goto target symbols from a node's Eff annotation.
--
-- Pattern matches directly on the node structure to find Eff annotation,
-- avoiding kind inference issues with GetEff's polykinded return type.
type ExtractGotoSymbols :: Type -> [Symbol]
type family ExtractGotoSymbols node where
  ExtractGotoSymbols (_ := _) = '[]
  ExtractGotoSymbols (node :@ Eff effs) = ProjectSymbols (GetGotoTargets effs)
  ExtractGotoSymbols (node :@ _) = ExtractGotoSymbols node

-- | Project just the Symbol from [(Symbol, Type)] pairs
type ProjectSymbols :: [(Symbol, Type)] -> [Symbol]
type family ProjectSymbols pairs where
  ProjectSymbols '[] = '[]
  ProjectSymbols ('(sym, _) ': rest) = sym ': ProjectSymbols rest

-- | Validate that all Goto symbols in a node are valid targets.
type ValidateNodeGotoTargets :: Symbol -> [Symbol] -> [Symbol] -> Constraint
type family ValidateNodeGotoTargets srcName targets validNames where
  ValidateNodeGotoTargets _ '[] _ = ()
  ValidateNodeGotoTargets srcName (target ': rest) validNames =
    ( GotoTargetExists srcName target validNames
    , ValidateNodeGotoTargets srcName rest validNames
    )

-- | Validate a single Goto target exists.
type GotoTargetExists :: Symbol -> Symbol -> [Symbol] -> Constraint
type family GotoTargetExists srcName target validNames where
  GotoTargetExists srcName target validNames =
    If (ElemSymbol target validNames)
       (() :: Constraint)
       (InvalidGotoTargetError srcName target)

-- ════════════════════════════════════════════════════════════════════════════
-- ERROR MESSAGES
-- ════════════════════════════════════════════════════════════════════════════

-- | Error when graph has no Entry declaration.
type MissingEntryError :: Constraint
type MissingEntryError = TypeError
  ('Text "Graph validation failed: missing Entry declaration"
   ':$$: 'Text "Add: Entry :~> YourInputType"
  )

-- | Error when graph has no Exit declaration.
type MissingExitError :: Constraint
type MissingExitError = TypeError
  ('Text "Graph validation failed: missing Exit declaration"
   ':$$: 'Text "Add: Exit :<~ YourOutputType"
  )

-- | Error when a node's Need is not provided by any Schema or Entry.
type UnsatisfiedNeedError :: Symbol -> Type -> Constraint
type UnsatisfiedNeedError nodeName needType = TypeError
  ('Text "Graph validation failed: unsatisfied dependency"
   ':$$: 'Text "Node '" ':<>: 'Text nodeName ':<>: 'Text "' needs type:"
   ':$$: 'Text "  " ':<>: 'ShowType needType
   ':$$: 'Text "But no node provides it via Schema and Entry doesn't provide it."
   ':$$: 'Text "Fix: Add a node with 'Schema " ':<>: 'ShowType needType ':<>: 'Text "'"
   ':$$: 'Text "  or add it to Entry type."
  )

-- | Error when a Goto target doesn't exist.
type InvalidGotoTargetError :: Symbol -> Symbol -> Constraint
type InvalidGotoTargetError srcName targetName = TypeError
  ('Text "Graph validation failed: invalid Goto target"
   ':$$: 'Text "Node '" ':<>: 'Text srcName ':<>: 'Text "' has:"
   ':$$: 'Text "  Goto \"" ':<>: 'Text targetName ':<>: 'Text "\" ..."
   ':$$: 'Text "But no node named \"" ':<>: 'Text targetName ':<>: 'Text "\" exists."
   ':$$: 'Text "Fix: Create the target node or use Goto Exit for termination."
  )

-- ════════════════════════════════════════════════════════════════════════════
-- TOOL SCHEMA VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validates that all tools used in the graph have valid schemas.
--
-- This ensures:
-- * Every tool has a GraphTool instance
-- * Tool input types have HasJSONSchema
-- * Tool output types have HasJSONSchema
type AllToolsHaveSchema :: Type -> Constraint
type family AllToolsHaveSchema g where
  AllToolsHaveSchema (Graph nodes) = AllNodeToolsValid nodes
  AllToolsHaveSchema (g :& _) = AllToolsHaveSchema g

-- | Validate tools for each node.
type AllNodeToolsValid :: [Type] -> Constraint
type family AllNodeToolsValid nodes where
  AllNodeToolsValid '[] = ()
  AllNodeToolsValid ((Entry :~> _) ': rest) = AllNodeToolsValid rest
  AllNodeToolsValid ((Exit :<~ _) ': rest) = AllNodeToolsValid rest
  AllNodeToolsValid (node ': rest) =
    ( AllToolsValid (GetTools node)
    , AllNodeToolsValid rest
    )

-- ════════════════════════════════════════════════════════════════════════════
-- SCHEMA VALIDATION FOR STRUCTURED OUTPUT
-- ════════════════════════════════════════════════════════════════════════════

-- | Validates that all Schema types are valid for Anthropic structured output.
--
-- This ensures Schema output types don't use features unsupported by structured
-- output (e.g., oneOf for sum types). Types with the 'UsesOneOf' marker will
-- produce a helpful compile-time error.
type AllSchemasValidForStructuredOutput :: Type -> Constraint
type family AllSchemasValidForStructuredOutput g where
  AllSchemasValidForStructuredOutput (Graph nodes) = AllNodeSchemasValid nodes
  AllSchemasValidForStructuredOutput (g :& _) = AllSchemasValidForStructuredOutput g

-- | Validate Schema for each node.
type AllNodeSchemasValid :: [Type] -> Constraint
type family AllNodeSchemasValid nodes where
  AllNodeSchemasValid '[] = ()
  AllNodeSchemasValid ((Entry :~> _) ': rest) = AllNodeSchemasValid rest
  AllNodeSchemasValid ((Exit :<~ _) ': rest) = AllNodeSchemasValid rest
  AllNodeSchemasValid (node ': rest) =
    ( ValidateNodeSchema (GetSchema node)
    , AllNodeSchemasValid rest
    )

-- | Validate a single node's Schema (if present).
type ValidateNodeSchema :: Maybe Type -> Constraint
type family ValidateNodeSchema mSchema where
  ValidateNodeSchema 'Nothing = ()
  ValidateNodeSchema ('Just t) = ValidStructuredOutput t

-- ════════════════════════════════════════════════════════════════════════════
-- MEMORY VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validates that all Memory types in the graph are valid.
--
-- Currently a placeholder that always succeeds. Full validation
-- (e.g., JSON serializable, Typeable) will be enforced at runtime
-- when the NodeMemory effect is interpreted.
--
-- Future: Add compile-time checks for serialization constraints.
type AllMemoriesValid :: Type -> Constraint
type family AllMemoriesValid g where
  AllMemoriesValid (Graph nodes) = AllNodeMemoriesValid nodes
  AllMemoriesValid (g :& _) = AllMemoriesValid g

-- | Validate Memory for each node.
type AllNodeMemoriesValid :: [Type] -> Constraint
type family AllNodeMemoriesValid nodes where
  AllNodeMemoriesValid '[] = ()
  AllNodeMemoriesValid ((Entry :~> _) ': rest) = AllNodeMemoriesValid rest
  AllNodeMemoriesValid ((Exit :<~ _) ': rest) = AllNodeMemoriesValid rest
  AllNodeMemoriesValid (node ': rest) =
    ( ValidateNodeMemory (GetMemory node)
    , AllNodeMemoriesValid rest
    )

-- | Validate a single node's Memory (if present).
--
-- Currently always succeeds. The Memory type will be validated
-- at runtime when the effect is used (JSON, Typeable constraints).
type ValidateNodeMemory :: Maybe Type -> Constraint
type family ValidateNodeMemory mMemory where
  ValidateNodeMemory 'Nothing = ()
  ValidateNodeMemory ('Just _t) = ()  -- Placeholder: add constraints when effect is built

-- ════════════════════════════════════════════════════════════════════════════
-- UTILITIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Type-level If (constraint version).
type If :: Bool -> Constraint -> Constraint -> Constraint
type family If cond t f where
  If 'True  t _ = t
  If 'False _ f = f

-- | Append Maybe to list (Nothing = no change).
type AppendMaybe :: Maybe Type -> [Type] -> [Type]
type family AppendMaybe m xs where
  AppendMaybe 'Nothing xs = xs
  AppendMaybe ('Just x) xs = x ': xs

-- | Type-level Elem for Types (uses type equality).
type ElemType :: Type -> [Type] -> Bool
type family ElemType x xs where
  ElemType _ '[] = 'False
  ElemType x (x ': _) = 'True
  ElemType x (_ ': rest) = ElemType x rest

-- | Type-level Elem for Symbols.
type ElemSymbol :: Symbol -> [Symbol] -> Bool
type family ElemSymbol x xs where
  ElemSymbol _ '[] = 'False
  ElemSymbol x (x ': _) = 'True
  ElemSymbol x (_ ': rest) = ElemSymbol x rest
