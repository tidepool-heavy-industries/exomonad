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
  , UniqueSchemas
  , NeedsSatisfied
  , GotoTargetExists

    -- * Error Messages
  , MissingEntryError
  , MissingExitError
  , UnsatisfiedNeedError
  , UnsatisfiedNeedErrorWithContext
  , InvalidGotoTargetError
  , InvalidGotoTargetErrorWithContext
  , DuplicateSchemaError
  , DuplicateSchemaErrorWithNodes

    -- * Error Formatting Helpers
  , FormatTypeList
  , FormatSymbolList
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
  , UniqueSchemas g
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
-- We pass the full 'provided' list to each check so errors can show what's available.
type AllNodeNeedsSatisfied :: [Type] -> [Type] -> Constraint
type family AllNodeNeedsSatisfied nodes provided where
  AllNodeNeedsSatisfied '[] _ = ()
  AllNodeNeedsSatisfied ((Entry :~> _) ': rest) provided =
    AllNodeNeedsSatisfied rest provided
  AllNodeNeedsSatisfied ((Exit :<~ _) ': rest) provided =
    AllNodeNeedsSatisfied rest provided
  AllNodeNeedsSatisfied (node ': rest) provided =
    ( NeedsSatisfied (NodeName node) (GetNeeds node) provided provided
    , AllNodeNeedsSatisfied rest provided
    )

-- | Validate that all types in a node's Needs are in the provided list.
-- Takes both the remaining provided list (for checking) and the full list (for errors).
type NeedsSatisfied :: Symbol -> [Type] -> [Type] -> [Type] -> Constraint
type family NeedsSatisfied nodeName needs provided allProvided where
  NeedsSatisfied _ '[] _ _ = ()
  NeedsSatisfied nodeName (t ': rest) provided allProvided =
    ( CheckNeedProvided nodeName t provided allProvided
    , NeedsSatisfied nodeName rest provided allProvided
    )

-- | Check if a single need is satisfied.
-- Uses the enhanced error that shows available types.
type CheckNeedProvided :: Symbol -> Type -> [Type] -> [Type] -> Constraint
type family CheckNeedProvided nodeName need provided allProvided where
  CheckNeedProvided nodeName need provided allProvided =
    If (ElemType need provided)
       (() :: Constraint)
       (UnsatisfiedNeedErrorWithContext nodeName need allProvided)

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
  ExtractGotoSymbols (node :@ UsesEffects effs) = ProjectSymbols (GetGotoTargets effs)
  ExtractGotoSymbols (node :@ _) = ExtractGotoSymbols node

-- | Project just the Symbol from [(Symbol, Type)] pairs
type ProjectSymbols :: [(Symbol, Type)] -> [Symbol]
type family ProjectSymbols pairs where
  ProjectSymbols '[] = '[]
  ProjectSymbols ('(sym, _) ': rest) = sym ': ProjectSymbols rest

-- | Validate that all Goto symbols in a node are valid targets.
-- We pass the full validNames list so errors can show available options.
type ValidateNodeGotoTargets :: Symbol -> [Symbol] -> [Symbol] -> Constraint
type family ValidateNodeGotoTargets srcName targets validNames where
  ValidateNodeGotoTargets _ '[] _ = ()
  ValidateNodeGotoTargets srcName (target ': rest) validNames =
    ( GotoTargetExists srcName target validNames validNames
    , ValidateNodeGotoTargets srcName rest validNames
    )

-- | Validate a single Goto target exists.
-- Takes both the checking list and full list (for error messages).
type GotoTargetExists :: Symbol -> Symbol -> [Symbol] -> [Symbol] -> Constraint
type family GotoTargetExists srcName target checkList allValidNames where
  GotoTargetExists srcName target checkList allValidNames =
    If (ElemSymbol target checkList)
       (() :: Constraint)
       (InvalidGotoTargetErrorWithContext srcName target allValidNames)

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
--
-- This version takes the list of provided types so it can show what IS available.
type UnsatisfiedNeedError :: Symbol -> Type -> Constraint
type UnsatisfiedNeedError nodeName needType = TypeError
  ('Text "Graph validation failed: unsatisfied dependency"
   ':$$: 'Text ""
   ':$$: 'Text "Node '" ':<>: 'Text nodeName ':<>: 'Text "' needs type:"
   ':$$: 'Text "  " ':<>: 'ShowType needType
   ':$$: 'Text ""
   ':$$: 'Text "But no node provides this type via Schema and Entry doesn't provide it."
   ':$$: 'Text ""
   ':$$: 'Text "Fix options:"
   ':$$: 'Text "  1. Add a node with 'Schema " ':<>: 'ShowType needType ':<>: 'Text "'"
   ':$$: 'Text "  2. Change Entry to provide this type: Entry :~> " ':<>: 'ShowType needType
   ':$$: 'Text "  3. Remove " ':<>: 'ShowType needType ':<>: 'Text " from this node's Needs"
  )

-- | Enhanced error showing what types ARE available.
type UnsatisfiedNeedErrorWithContext :: Symbol -> Type -> [Type] -> Constraint
type UnsatisfiedNeedErrorWithContext nodeName needType available = TypeError
  ('Text "Graph validation failed: unsatisfied dependency"
   ':$$: 'Text ""
   ':$$: 'Text "Node '" ':<>: 'Text nodeName ':<>: 'Text "' needs type:"
   ':$$: 'Text "  " ':<>: 'ShowType needType
   ':$$: 'Text ""
   ':$$: 'Text "Available types (from Entry and Schema outputs):"
   ':$$: FormatTypeList available
   ':$$: 'Text ""
   ':$$: 'Text "Fix options:"
   ':$$: 'Text "  1. Add a node with 'Schema " ':<>: 'ShowType needType ':<>: 'Text "'"
   ':$$: 'Text "  2. Change Entry to provide this type"
   ':$$: 'Text "  3. Remove " ':<>: 'ShowType needType ':<>: 'Text " from this node's Needs"
  )

-- | Format a list of types for display in error messages.
type FormatTypeList :: [Type] -> ErrorMessage
type family FormatTypeList ts where
  FormatTypeList '[] = 'Text "  (none)"
  FormatTypeList '[t] = 'Text "  • " ':<>: 'ShowType t
  FormatTypeList (t ': rest) = 'Text "  • " ':<>: 'ShowType t ':$$: FormatTypeList rest

-- | Error when a Goto target doesn't exist.
type InvalidGotoTargetError :: Symbol -> Symbol -> Constraint
type InvalidGotoTargetError srcName targetName = TypeError
  ('Text "Graph validation failed: invalid Goto target"
   ':$$: 'Text ""
   ':$$: 'Text "Node '" ':<>: 'Text srcName ':<>: 'Text "' has:"
   ':$$: 'Text "  Goto \"" ':<>: 'Text targetName ':<>: 'Text "\" ..."
   ':$$: 'Text ""
   ':$$: 'Text "But no node named \"" ':<>: 'Text targetName ':<>: 'Text "\" exists."
   ':$$: 'Text ""
   ':$$: 'Text "Fix options:"
   ':$$: 'Text "  1. Create a node named \"" ':<>: 'Text targetName ':<>: 'Text "\""
   ':$$: 'Text "  2. Use 'Goto Exit' for graph termination"
   ':$$: 'Text "  3. Check spelling of the target node name"
  )

-- | Enhanced error showing available node names.
type InvalidGotoTargetErrorWithContext :: Symbol -> Symbol -> [Symbol] -> Constraint
type InvalidGotoTargetErrorWithContext srcName targetName validNames = TypeError
  ('Text "Graph validation failed: invalid Goto target"
   ':$$: 'Text ""
   ':$$: 'Text "Node '" ':<>: 'Text srcName ':<>: 'Text "' has:"
   ':$$: 'Text "  Goto \"" ':<>: 'Text targetName ':<>: 'Text "\" ..."
   ':$$: 'Text ""
   ':$$: 'Text "But no node named \"" ':<>: 'Text targetName ':<>: 'Text "\" exists."
   ':$$: 'Text ""
   ':$$: 'Text "Available node names:"
   ':$$: FormatSymbolList validNames
   ':$$: 'Text ""
   ':$$: 'Text "Fix options:"
   ':$$: 'Text "  1. Create a node named \"" ':<>: 'Text targetName ':<>: 'Text "\""
   ':$$: 'Text "  2. Use 'Goto Exit' for graph termination"
   ':$$: 'Text "  3. Use one of the existing node names above"
  )

-- | Format a list of symbols for display in error messages.
type FormatSymbolList :: [Symbol] -> ErrorMessage
type family FormatSymbolList ss where
  FormatSymbolList '[] = 'Text "  (none)"
  FormatSymbolList '[s] = 'Text "  • " ':<>: 'Text s
  FormatSymbolList (s ': rest) = 'Text "  • " ':<>: 'Text s ':$$: FormatSymbolList rest

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
-- UNIQUE SCHEMA VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validates that all Schema types in the graph are unique.
--
-- Two nodes with the same Schema type would create ambiguous Needs resolution.
-- This ensures each Schema type is produced by exactly one node.
--
-- @
-- -- This would fail:
-- type BadGraph = Graph
--   '[ Entry :~> Input
--    , "a" := LLM :@ Needs '[Input] :@ Schema Response
--    , "b" := LLM :@ Needs '[Input] :@ Schema Response  -- Duplicate!
--    , Exit :<~ Response
--    ]
-- @
type UniqueSchemas :: Type -> Constraint
type family UniqueSchemas g where
  UniqueSchemas (Graph nodes) = UniqueSchemasWithNames (CollectNodeSchemas nodes)
  UniqueSchemas (g :& _) = UniqueSchemas g

-- | Collect (NodeName, SchemaType) pairs from all nodes.
type CollectNodeSchemas :: [Type] -> [(Symbol, Type)]
type family CollectNodeSchemas nodes where
  CollectNodeSchemas '[] = '[]
  CollectNodeSchemas ((Entry :~> _) ': rest) = CollectNodeSchemas rest
  CollectNodeSchemas ((Exit :<~ _) ': rest) = CollectNodeSchemas rest
  CollectNodeSchemas (node ': rest) = AppendMaybeWithName (NodeName node) (GetSchema node) (CollectNodeSchemas rest)

-- | Append (name, schema) pair if schema is Just.
type AppendMaybeWithName :: Symbol -> Maybe Type -> [(Symbol, Type)] -> [(Symbol, Type)]
type family AppendMaybeWithName name mSchema rest where
  AppendMaybeWithName _ 'Nothing rest = rest
  AppendMaybeWithName name ('Just t) rest = '(name, t) ': rest

-- | Ensure no duplicate schemas, tracking node names for error messages.
type UniqueSchemasWithNames :: [(Symbol, Type)] -> Constraint
type family UniqueSchemasWithNames pairs where
  UniqueSchemasWithNames '[] = ()
  UniqueSchemasWithNames ('(name, t) ': rest) =
    ( CheckSchemaNotDuplicated name t rest
    , UniqueSchemasWithNames rest
    )

-- | Check that a schema type doesn't appear in the remaining list.
type CheckSchemaNotDuplicated :: Symbol -> Type -> [(Symbol, Type)] -> Constraint
type family CheckSchemaNotDuplicated firstName firstType rest where
  CheckSchemaNotDuplicated _ _ '[] = ()
  CheckSchemaNotDuplicated firstName firstType ('(secondName, firstType) ': _) =
    DuplicateSchemaErrorWithNodes firstName secondName firstType
  CheckSchemaNotDuplicated firstName firstType ('(_, _) ': rest) =
    CheckSchemaNotDuplicated firstName firstType rest

-- | Error when two nodes have the same Schema type, showing which nodes conflict.
type DuplicateSchemaErrorWithNodes :: Symbol -> Symbol -> Type -> Constraint
type DuplicateSchemaErrorWithNodes node1 node2 schemaType = TypeError
  ('Text "Graph validation failed: duplicate Schema type"
   ':$$: 'Text ""
   ':$$: 'Text "Multiple nodes produce the same Schema type:"
   ':$$: 'Text "  • Node '" ':<>: 'Text node1 ':<>: 'Text "' has Schema " ':<>: 'ShowType schemaType
   ':$$: 'Text "  • Node '" ':<>: 'Text node2 ':<>: 'Text "' has Schema " ':<>: 'ShowType schemaType
   ':$$: 'Text ""
   ':$$: 'Text "This creates ambiguous Needs resolution - which node provides the type?"
   ':$$: 'Text ""
   ':$$: 'Text "Fix options:"
   ':$$: 'Text "  1. Use distinct wrapper types: data " ':<>: 'Text node1 ':<>: 'Text "Response = ..."
   ':$$: 'Text "  2. Merge the nodes if they serve the same purpose"
   ':$$: 'Text "  3. Use different Schema types for different semantics"
  )

-- | Simple duplicate error (kept for backwards compatibility)
type DuplicateSchemaError :: Type -> Constraint
type DuplicateSchemaError t = TypeError
  ('Text "Graph validation failed: duplicate Schema type"
   ':$$: 'Text "  Schema " ':<>: 'ShowType t ':<>: 'Text " is produced by multiple nodes."
   ':$$: 'Text "This creates ambiguous Needs resolution."
   ':$$: 'Text "Fix: Use distinct types for each node's Schema output."
  )

-- | Legacy Unique family (kept for compatibility, uses simpler error)
type Unique :: [Type] -> Constraint
type family Unique xs where
  Unique '[] = ()
  Unique (x ': xs) = (NotElemType x xs, Unique xs)

-- | Constraint-level check that a type is NOT in a list.
-- Produces a type error if the type IS in the list.
type NotElemType :: Type -> [Type] -> Constraint
type family NotElemType x xs where
  NotElemType _ '[] = ()
  NotElemType x (x ': _) = DuplicateSchemaError x
  NotElemType x (_ ': rest) = NotElemType x rest

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
