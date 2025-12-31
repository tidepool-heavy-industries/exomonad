{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Structural validation for Graph topology.
--
-- This module provides compile-time validation of graph structure beyond
-- basic type matching:
--
-- 1. __Reachability__: Every node must be reachable from Entry
-- 2. __Exit Coverage__: Every Logic node must have a path to Exit
-- 3. __Orphan Detection__: Warning when Schema output is never consumed
-- 4. __Dead Goto Detection__: Goto targets that can't receive their payload
--
-- = Algorithm Design
--
-- Graph topology is analyzed via fixed-point iteration at the type level:
--
-- * __Reachable nodes__: Start with Entry's type, iteratively add nodes
--   whose Needs are satisfied by available types (Entry + reached Schema outputs)
--
-- * __Exit-reaching nodes__: Start with Exit, work backwards through Goto
--   edges to find all nodes that can reach Exit
--
-- These analyses use bounded iteration (based on node count) to handle
-- cycles safely at compile time.
module Tidepool.Graph.Validate.Structure
  ( -- * Main Structural Constraints
    AllNodesReachable
  , AllLogicNodesReachExit
  , NoOrphanSchemas
  , NoDeadGotos

    -- * Error Messages
  , UnreachableNodeError
  , NoExitPathError
  , OrphanSchemaWarning
  , DeadGotoError

    -- * Internal Type Families (exported for testing)
  , ComputeReachable
  , ComputeExitReaching
  , FindUnreachableNodes
  , FindNoExitPathNodes
  , FindOrphanSchemas
  , FindDeadGotos
  ) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..), Nat, type (-), type (+))

import Tidepool.Graph.Types
import Tidepool.Graph.Edges
import Tidepool.Graph.Goto (Goto)

-- ════════════════════════════════════════════════════════════════════════════
-- REACHABILITY VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validates that all nodes in the graph are reachable from Entry.
--
-- A node is reachable if:
-- * Its Needs can be satisfied by the Entry type, OR
-- * Its Needs can be satisfied by Entry + Schema outputs of other reachable nodes, OR
-- * It is the target of a Goto from a reachable Logic node
--
-- @
-- -- This graph has an unreachable node:
-- type BadGraph = Graph
--   '[ Entry :~> A
--    , "reachable" := LLM :@ Needs '[A] :@ Schema B
--    , "orphan" := LLM :@ Needs '[X] :@ Schema Y  -- X never provided!
--    , Exit :<~ B
--    ]
-- @
type AllNodesReachable :: Type -> Constraint
type family AllNodesReachable g where
  AllNodesReachable (Graph nodes) =
    CheckAllReachable
      (FindUnreachableNodes nodes (GetEntryType (Graph nodes)))
  AllNodesReachable (g :& _) = AllNodesReachable g

-- | Check if unreachable list is empty; error if not.
type CheckAllReachable :: [Symbol] -> Constraint
type family CheckAllReachable unreachable where
  CheckAllReachable '[] = ()
  CheckAllReachable (name ': _) = UnreachableNodeError name

-- | Find all nodes that are NOT reachable from Entry.
--
-- Strategy: Compute the reachable set, then find nodes not in it.
type FindUnreachableNodes :: [Type] -> Type -> [Symbol]
type family FindUnreachableNodes nodes entryType where
  FindUnreachableNodes nodes entryType =
    FilterNotIn
      (CollectAllNodeNames nodes)
      (ComputeReachable nodes entryType (Length nodes))

-- | Collect names of all actual nodes (not Entry/Exit).
type CollectAllNodeNames :: [Type] -> [Symbol]
type family CollectAllNodeNames nodes where
  CollectAllNodeNames '[] = '[]
  CollectAllNodeNames ((Entry :~> _) ': rest) = CollectAllNodeNames rest
  CollectAllNodeNames ((Exit :<~ _) ': rest) = CollectAllNodeNames rest
  CollectAllNodeNames (node ': rest) = NodeName node ': CollectAllNodeNames rest

-- | Compute reachable nodes via fixed-point iteration.
--
-- Parameters:
-- * nodes: all node declarations
-- * entryType: the Entry input type
-- * fuel: iteration bound (decrements each round)
--
-- Returns: list of reachable node names
type ComputeReachable :: [Type] -> Type -> Nat -> [Symbol]
type family ComputeReachable nodes entryType fuel where
  ComputeReachable _ _ 0 = '[]  -- Out of fuel, stop iteration
  ComputeReachable nodes entryType fuel =
    ComputeReachableStep nodes '[entryType] '[] fuel

-- | Single step of reachability computation.
--
-- Parameters:
-- * nodes: all node declarations
-- * available: types currently available (Entry + reached Schema outputs)
-- * reached: nodes already marked reachable
-- * fuel: remaining iterations
type ComputeReachableStep :: [Type] -> [Type] -> [Symbol] -> Nat -> [Symbol]
type family ComputeReachableStep nodes available reached fuel where
  ComputeReachableStep _ _ reached 0 = reached
  ComputeReachableStep nodes available reached fuel =
    ComputeReachableStepContinue
      nodes
      available
      reached
      (FindNewlyReachable nodes available reached)
      fuel

-- | Continue reachability computation with newly reached nodes.
type ComputeReachableStepContinue :: [Type] -> [Type] -> [Symbol] -> [Symbol] -> Nat -> [Symbol]
type family ComputeReachableStepContinue nodes available reached newlyReached fuel where
  ComputeReachableStepContinue _ _ reached '[] _ = reached  -- Fixed point
  ComputeReachableStepContinue nodes available reached newlyReached fuel =
    ComputeReachableStep
      nodes
      (AddSchemaTypes nodes newlyReached available)
      (AppendSymbols reached newlyReached)
      (fuel - 1)

-- | Find nodes that become reachable in this iteration.
--
-- A node is newly reachable if:
-- * It's not already in the reached set
-- * All its Needs are in the available types
type FindNewlyReachable :: [Type] -> [Type] -> [Symbol] -> [Symbol]
type family FindNewlyReachable nodes available reached where
  FindNewlyReachable '[] _ _ = '[]
  FindNewlyReachable ((Entry :~> _) ': rest) available reached =
    FindNewlyReachable rest available reached
  FindNewlyReachable ((Exit :<~ _) ': rest) available reached =
    FindNewlyReachable rest available reached
  FindNewlyReachable (node ': rest) available reached =
    If (And (Not (ElemSymbol (NodeName node) reached))
            (AllIn (GetNeeds node) available))
       (NodeName node ': FindNewlyReachable rest available reached)
       (FindNewlyReachable rest available reached)

-- | Add Schema output types from newly reached nodes to available types.
type AddSchemaTypes :: [Type] -> [Symbol] -> [Type] -> [Type]
type family AddSchemaTypes nodes newlyReached available where
  AddSchemaTypes _ '[] available = available
  AddSchemaTypes nodes (name ': rest) available =
    AddSchemaTypes nodes rest (AddNodeSchema nodes name available)

-- | Add a single node's Schema type to available (if it has one).
type AddNodeSchema :: [Type] -> Symbol -> [Type] -> [Type]
type family AddNodeSchema nodes name available where
  AddNodeSchema nodes name available =
    AppendMaybeType (GetSchemaByName nodes name) available

-- | Get Schema type for a node by name.
type GetSchemaByName :: [Type] -> Symbol -> Maybe Type
type family GetSchemaByName nodes name where
  GetSchemaByName '[] _ = 'Nothing
  GetSchemaByName (node ': rest) name =
    If (NodeName node == name)
       (GetSchema node)
       (GetSchemaByName rest name)

-- ════════════════════════════════════════════════════════════════════════════
-- EXIT COVERAGE VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validates that all Logic nodes have a path to Exit.
--
-- A Logic node reaches Exit if:
-- * It has @Goto Exit@ in its effect stack, OR
-- * It has @Goto "name"@ where "name" reaches Exit
--
-- LLM nodes don't need this check - they transition implicitly via Schema.
--
-- @
-- -- This graph has a Logic node that can't reach Exit:
-- type BadGraph = Graph
--   '[ Entry :~> A
--    , "loop" := Logic :@ Needs '[A] :@ UsesEffects '[Goto "loop" A]  -- Infinite loop!
--    , Exit :<~ B
--    ]
-- @
type AllLogicNodesReachExit :: Type -> Constraint
type family AllLogicNodesReachExit g where
  AllLogicNodesReachExit (Graph nodes) =
    CheckAllReachExit (FindNoExitPathNodes nodes)
  AllLogicNodesReachExit (g :& _) = AllLogicNodesReachExit g

-- | Check if no-exit list is empty; error if not.
type CheckAllReachExit :: [Symbol] -> Constraint
type family CheckAllReachExit noExit where
  CheckAllReachExit '[] = ()
  CheckAllReachExit (name ': _) = NoExitPathError name

-- | Find Logic nodes that cannot reach Exit.
type FindNoExitPathNodes :: [Type] -> [Symbol]
type family FindNoExitPathNodes nodes where
  FindNoExitPathNodes nodes =
    FilterNotIn
      (CollectLogicNodeNames nodes)
      (ComputeExitReaching nodes (Length nodes))

-- | Collect names of Logic nodes only.
type CollectLogicNodeNames :: [Type] -> [Symbol]
type family CollectLogicNodeNames nodes where
  CollectLogicNodeNames '[] = '[]
  CollectLogicNodeNames ((Entry :~> _) ': rest) = CollectLogicNodeNames rest
  CollectLogicNodeNames ((Exit :<~ _) ': rest) = CollectLogicNodeNames rest
  CollectLogicNodeNames (node ': rest) =
    If (IsLogicNode node)
       (NodeName node ': CollectLogicNodeNames rest)
       (CollectLogicNodeNames rest)

-- | Check if a node is a Logic node.
type IsLogicNode :: Type -> Bool
type family IsLogicNode node where
  IsLogicNode node = IsLogicKind (GetNodeKind node)

-- | Check if NodeKind is Logic.
type IsLogicKind :: NodeKind -> Bool
type family IsLogicKind k where
  IsLogicKind 'Logic = 'True
  IsLogicKind 'LLM = 'False

-- | Compute nodes that can reach Exit via fixed-point iteration.
--
-- Works backwards: start with nodes that have Goto Exit, then add
-- nodes that Goto those nodes.
type ComputeExitReaching :: [Type] -> Nat -> [Symbol]
type family ComputeExitReaching nodes fuel where
  ComputeExitReaching _ 0 = '[]
  ComputeExitReaching nodes fuel =
    ComputeExitReachingStep nodes (NodesWithGotoExit nodes) fuel

-- | Single step of exit-reaching computation.
type ComputeExitReachingStep :: [Type] -> [Symbol] -> Nat -> [Symbol]
type family ComputeExitReachingStep nodes reaching fuel where
  ComputeExitReachingStep _ reaching 0 = reaching
  ComputeExitReachingStep nodes reaching fuel =
    ComputeExitReachingStepContinue
      nodes
      reaching
      (FindNewlyExitReaching nodes reaching)
      fuel

-- | Continue exit-reaching computation with newly reaching nodes.
type ComputeExitReachingStepContinue :: [Type] -> [Symbol] -> [Symbol] -> Nat -> [Symbol]
type family ComputeExitReachingStepContinue nodes reaching newlyReaching fuel where
  ComputeExitReachingStepContinue _ reaching '[] _ = reaching  -- Fixed point
  ComputeExitReachingStepContinue nodes reaching newlyReaching fuel =
    ComputeExitReachingStep
      nodes
      (AppendSymbols reaching newlyReaching)
      (fuel - 1)

-- | Find nodes with direct Goto Exit.
type NodesWithGotoExit :: [Type] -> [Symbol]
type family NodesWithGotoExit nodes where
  NodesWithGotoExit '[] = '[]
  NodesWithGotoExit ((Entry :~> _) ': rest) = NodesWithGotoExit rest
  NodesWithGotoExit ((Exit :<~ _) ': rest) = NodesWithGotoExit rest
  NodesWithGotoExit (node ': rest) =
    If (NodeHasGotoExit node)
       (NodeName node ': NodesWithGotoExit rest)
       (NodesWithGotoExit rest)

-- | Check if a node has Goto Exit in its effect stack.
--
-- We pattern match directly on the node structure to extract UsesEffects
-- and check for Goto Exit, avoiding kind inference issues.
type NodeHasGotoExit :: Type -> Bool
type family NodeHasGotoExit node where
  NodeHasGotoExit (_ := _) = 'False
  NodeHasGotoExit (node :@ UsesEffects effs) = HasGotoExit effs
  NodeHasGotoExit (node :@ _) = NodeHasGotoExit node

-- | Find nodes that become exit-reaching in this iteration.
type FindNewlyExitReaching :: [Type] -> [Symbol] -> [Symbol]
type family FindNewlyExitReaching nodes reaching where
  FindNewlyExitReaching '[] _ = '[]
  FindNewlyExitReaching ((Entry :~> _) ': rest) reaching =
    FindNewlyExitReaching rest reaching
  FindNewlyExitReaching ((Exit :<~ _) ': rest) reaching =
    FindNewlyExitReaching rest reaching
  FindNewlyExitReaching (node ': rest) reaching =
    If (And (Not (ElemSymbol (NodeName node) reaching))
            (HasGotoToAny node reaching))
       (NodeName node ': FindNewlyExitReaching rest reaching)
       (FindNewlyExitReaching rest reaching)

-- | Check if node has Goto to any of the target names.
type HasGotoToAny :: Type -> [Symbol] -> Bool
type family HasGotoToAny node targets where
  HasGotoToAny node targets =
    AnyElemSymbol (GetNodeGotoTargetNames node) targets

-- | Get Goto target names from a node.
--
-- Pattern matches directly on node structure to avoid kind inference issues.
type GetNodeGotoTargetNames :: Type -> [Symbol]
type family GetNodeGotoTargetNames node where
  GetNodeGotoTargetNames (_ := _) = '[]
  GetNodeGotoTargetNames (node :@ UsesEffects effs) = ProjectSymbolsFromPairs (GetGotoTargets effs)
  GetNodeGotoTargetNames (node :@ _) = GetNodeGotoTargetNames node

-- | Project symbols from (Symbol, Type) pairs.
type ProjectSymbolsFromPairs :: [(Symbol, Type)] -> [Symbol]
type family ProjectSymbolsFromPairs pairs where
  ProjectSymbolsFromPairs '[] = '[]
  ProjectSymbolsFromPairs ('(sym, _) ': rest) = sym ': ProjectSymbolsFromPairs rest

-- ════════════════════════════════════════════════════════════════════════════
-- ORPHAN SCHEMA DETECTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Warns when a Schema output is never consumed by any Needs.
--
-- An orphan Schema is one where:
-- * The type is not needed by any node, AND
-- * The type is not the Exit type
--
-- This is a warning, not an error - orphan schemas may be intentional
-- (e.g., for logging side effects).
--
-- @
-- type GraphWithOrphan = Graph
--   '[ Entry :~> A
--    , "produce" := LLM :@ Needs '[A] :@ Schema B  -- B is orphaned!
--    , "other" := LLM :@ Needs '[A] :@ Schema C
--    , Exit :<~ C
--    ]
-- @
type NoOrphanSchemas :: Type -> Constraint
type family NoOrphanSchemas g where
  NoOrphanSchemas (Graph nodes) =
    CheckNoOrphans (FindOrphanSchemas nodes (GetExitType (Graph nodes)))
  NoOrphanSchemas (g :& _) = NoOrphanSchemas g

-- | Check if orphan list is empty; warn if not.
type CheckNoOrphans :: [(Symbol, Type)] -> Constraint
type family CheckNoOrphans orphans where
  CheckNoOrphans '[] = ()
  CheckNoOrphans ('(name, t) ': _) = OrphanSchemaWarning name t

-- | Find Schema types that are never consumed.
type FindOrphanSchemas :: [Type] -> Type -> [(Symbol, Type)]
type family FindOrphanSchemas nodes exitType where
  FindOrphanSchemas nodes exitType =
    FilterOrphanSchemas
      (CollectSchemasWithNames nodes)
      (CollectAllNeeds nodes)
      exitType

-- | Collect (nodeName, schemaType) pairs.
type CollectSchemasWithNames :: [Type] -> [(Symbol, Type)]
type family CollectSchemasWithNames nodes where
  CollectSchemasWithNames '[] = '[]
  CollectSchemasWithNames ((Entry :~> _) ': rest) = CollectSchemasWithNames rest
  CollectSchemasWithNames ((Exit :<~ _) ': rest) = CollectSchemasWithNames rest
  CollectSchemasWithNames (node ': rest) =
    AppendMaybePair
      (MakeSchemaPair (NodeName node) (GetSchema node))
      (CollectSchemasWithNames rest)

-- | Make a (name, type) pair if Schema exists.
type MakeSchemaPair :: Symbol -> Maybe Type -> Maybe (Symbol, Type)
type family MakeSchemaPair name mSchema where
  MakeSchemaPair _ 'Nothing = 'Nothing
  MakeSchemaPair name ('Just t) = 'Just '(name, t)

-- | Collect all Needs types from all nodes.
type CollectAllNeeds :: [Type] -> [Type]
type family CollectAllNeeds nodes where
  CollectAllNeeds '[] = '[]
  CollectAllNeeds ((Entry :~> _) ': rest) = CollectAllNeeds rest
  CollectAllNeeds ((Exit :<~ _) ': rest) = CollectAllNeeds rest
  CollectAllNeeds (node ': rest) =
    AppendTypes (GetNeeds node) (CollectAllNeeds rest)

-- | Filter to only orphaned schemas.
type FilterOrphanSchemas :: [(Symbol, Type)] -> [Type] -> Type -> [(Symbol, Type)]
type family FilterOrphanSchemas schemas allNeeds exitType where
  FilterOrphanSchemas '[] _ _ = '[]
  FilterOrphanSchemas ('(name, t) ': rest) allNeeds exitType =
    If (Or (ElemType t allNeeds) (TypeEq t exitType))
       (FilterOrphanSchemas rest allNeeds exitType)
       ('(name, t) ': FilterOrphanSchemas rest allNeeds exitType)

-- ════════════════════════════════════════════════════════════════════════════
-- DEAD GOTO DETECTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validates that all Goto targets can receive their payload type.
--
-- A Goto is "dead" if:
-- * The target node Needs a type that is not provided by:
--   - The Goto payload type
--   - Entry
--   - Schema outputs from reachable nodes
--
-- @
-- type BadGraph = Graph
--   '[ Entry :~> A
--    , "router" := Logic :@ Needs '[A] :@ UsesEffects '[Goto "target" B]
--    , "target" := LLM :@ Needs '[C] :@ Schema D  -- Needs C, but gets B!
--    , Exit :<~ D
--    ]
-- @
type NoDeadGotos :: Type -> Constraint
type family NoDeadGotos g where
  NoDeadGotos (Graph nodes) =
    CheckNoDeadGotos (FindDeadGotos nodes (GetEntryType (Graph nodes)))
  NoDeadGotos (g :& _) = NoDeadGotos g

-- | Check if dead goto list is empty; error if not.
type CheckNoDeadGotos :: [(Symbol, Symbol, Type)] -> Constraint
type family CheckNoDeadGotos deadGotos where
  CheckNoDeadGotos '[] = ()
  CheckNoDeadGotos ('(src, target, payload) ': _) =
    DeadGotoError src target payload

-- | Find all dead Goto declarations.
--
-- Returns list of (source node, target node, payload type).
type FindDeadGotos :: [Type] -> Type -> [(Symbol, Symbol, Type)]
type family FindDeadGotos nodes entryType where
  FindDeadGotos nodes entryType =
    FindDeadGotosInNodes nodes nodes entryType

-- | Check each node's Gotos for deadness.
type FindDeadGotosInNodes :: [Type] -> [Type] -> Type -> [(Symbol, Symbol, Type)]
type family FindDeadGotosInNodes checking allNodes entryType where
  FindDeadGotosInNodes '[] _ _ = '[]
  FindDeadGotosInNodes ((Entry :~> _) ': rest) allNodes entryType =
    FindDeadGotosInNodes rest allNodes entryType
  FindDeadGotosInNodes ((Exit :<~ _) ': rest) allNodes entryType =
    FindDeadGotosInNodes rest allNodes entryType
  FindDeadGotosInNodes (node ': rest) allNodes entryType =
    AppendTriples
      (CheckNodeGotos (NodeName node) (GetNodeGotoTargetsWithPayloads node) allNodes entryType)
      (FindDeadGotosInNodes rest allNodes entryType)

-- | Get (target, payload) pairs from a node's effects.
--
-- Pattern matches directly on node structure to avoid kind inference issues.
type GetNodeGotoTargetsWithPayloads :: Type -> [(Symbol, Type)]
type family GetNodeGotoTargetsWithPayloads node where
  GetNodeGotoTargetsWithPayloads (_ := _) = '[]
  GetNodeGotoTargetsWithPayloads (node :@ UsesEffects effs) = GetGotoTargets effs
  GetNodeGotoTargetsWithPayloads (node :@ _) = GetNodeGotoTargetsWithPayloads node

-- | Check each Goto in a node for deadness.
type CheckNodeGotos :: Symbol -> [(Symbol, Type)] -> [Type] -> Type -> [(Symbol, Symbol, Type)]
type family CheckNodeGotos srcName gotos allNodes entryType where
  CheckNodeGotos _ '[] _ _ = '[]
  CheckNodeGotos srcName ('(target, payload) ': rest) allNodes entryType =
    AppendTriples
      (CheckSingleGoto srcName target payload allNodes entryType)
      (CheckNodeGotos srcName rest allNodes entryType)

-- | Check if a single Goto is dead.
--
-- A Goto "src" -> "target" with payload P is dead if "target" Needs
-- something that P cannot satisfy (considering Entry and available schemas).
type CheckSingleGoto :: Symbol -> Symbol -> Type -> [Type] -> Type -> [(Symbol, Symbol, Type)]
type family CheckSingleGoto srcName targetName payload allNodes entryType where
  CheckSingleGoto srcName targetName payload allNodes entryType =
    CheckSingleGotoWithTarget
      srcName
      targetName
      payload
      (GetNeedsFromMaybe (FindNodeByName allNodes targetName))
      (payload ': entryType ': CollectSchemaTypesFromNodes allNodes)

-- | Continue dead goto check with resolved target needs.
type CheckSingleGotoWithTarget :: Symbol -> Symbol -> Type -> [Type] -> [Type] -> [(Symbol, Symbol, Type)]
type family CheckSingleGotoWithTarget srcName targetName payload targetNeeds available where
  CheckSingleGotoWithTarget srcName targetName payload targetNeeds available =
    If (AllIn targetNeeds available)
       '[]  -- Not dead: all needs can be satisfied
       '[ '(srcName, targetName, payload) ]

-- | Get Needs from Maybe node.
type GetNeedsFromMaybe :: Maybe Type -> [Type]
type family GetNeedsFromMaybe mNode where
  GetNeedsFromMaybe 'Nothing = '[]
  GetNeedsFromMaybe ('Just node) = GetNeeds node

-- | Collect Schema types from nodes.
type CollectSchemaTypesFromNodes :: [Type] -> [Type]
type family CollectSchemaTypesFromNodes nodes where
  CollectSchemaTypesFromNodes '[] = '[]
  CollectSchemaTypesFromNodes ((Entry :~> _) ': rest) = CollectSchemaTypesFromNodes rest
  CollectSchemaTypesFromNodes ((Exit :<~ _) ': rest) = CollectSchemaTypesFromNodes rest
  CollectSchemaTypesFromNodes (node ': rest) =
    AppendMaybeType (GetSchema node) (CollectSchemaTypesFromNodes rest)

-- ════════════════════════════════════════════════════════════════════════════
-- ERROR MESSAGES
-- ════════════════════════════════════════════════════════════════════════════

-- | Error when a node is not reachable from Entry.
type UnreachableNodeError :: Symbol -> Constraint
type UnreachableNodeError name = TypeError
  ('Text "Graph validation failed: unreachable node"
   ':$$: 'Text "Node '" ':<>: 'Text name ':<>: 'Text "' cannot be reached from Entry."
   ':$$: 'Text ""
   ':$$: 'Text "A node is reachable if:"
   ':$$: 'Text "  • Its Needs are satisfied by Entry"
   ':$$: 'Text "  • Its Needs are satisfied by Schema outputs of reachable nodes"
   ':$$: 'Text "  • It is the target of a Goto from a reachable Logic node"
   ':$$: 'Text ""
   ':$$: 'Text "Fix: Ensure some reachable node provides what this node Needs,"
   ':$$: 'Text "     or add a Goto edge from a reachable Logic node."
  )

-- | Error when a Logic node cannot reach Exit.
type NoExitPathError :: Symbol -> Constraint
type NoExitPathError name = TypeError
  ('Text "Graph validation failed: Logic node cannot reach Exit"
   ':$$: 'Text "Node '" ':<>: 'Text name ':<>: 'Text "' has no path to Exit."
   ':$$: 'Text ""
   ':$$: 'Text "This creates a potential infinite loop or dead end."
   ':$$: 'Text ""
   ':$$: 'Text "A Logic node reaches Exit if:"
   ':$$: 'Text "  • It has Goto Exit in its UsesEffects, OR"
   ':$$: 'Text "  • It has Goto to another node that reaches Exit"
   ':$$: 'Text ""
   ':$$: 'Text "Fix: Add 'Goto Exit result' to the UsesEffects,"
   ':$$: 'Text "     or add Goto to a node that reaches Exit."
  )

-- | Warning when a Schema is never consumed.
type OrphanSchemaWarning :: Symbol -> Type -> Constraint
type OrphanSchemaWarning name t = TypeError
  ('Text "Graph validation warning: orphan Schema"
   ':$$: 'Text "Node '" ':<>: 'Text name ':<>: 'Text "' produces:"
   ':$$: 'Text "  Schema " ':<>: 'ShowType t
   ':$$: 'Text "But no node Needs this type, and it's not the Exit type."
   ':$$: 'Text ""
   ':$$: 'Text "This Schema output will be computed but never used."
   ':$$: 'Text ""
   ':$$: 'Text "Fix: Add a node that Needs " ':<>: 'ShowType t ':<>: 'Text ","
   ':$$: 'Text "     use it as the Exit type, or remove the Schema annotation."
  )

-- | Error when a Goto target cannot receive its payload.
type DeadGotoError :: Symbol -> Symbol -> Type -> Constraint
type DeadGotoError srcName targetName payload = TypeError
  ('Text "Graph validation failed: dead Goto"
   ':$$: 'Text "Node '" ':<>: 'Text srcName ':<>: 'Text "' has:"
   ':$$: 'Text "  Goto \"" ':<>: 'Text targetName ':<>: 'Text "\" " ':<>: 'ShowType payload
   ':$$: 'Text ""
   ':$$: 'Text "But node '" ':<>: 'Text targetName ':<>: 'Text "' Needs types that won't be available."
   ':$$: 'Text "The Goto payload type " ':<>: 'ShowType payload
   ':$$: 'Text "does not satisfy the target's requirements."
   ':$$: 'Text ""
   ':$$: 'Text "Fix: Ensure the Goto payload matches what the target Needs,"
   ':$$: 'Text "     or update the target's Needs to accept the payload type."
  )

-- ════════════════════════════════════════════════════════════════════════════
-- TYPE-LEVEL UTILITIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Type-level If.
type If :: Bool -> k -> k -> k
type family If cond t f where
  If 'True  t _ = t
  If 'False _ f = f

-- | Type-level And.
type And :: Bool -> Bool -> Bool
type family And a b where
  And 'True 'True = 'True
  And _ _ = 'False

-- | Type-level Or.
type Or :: Bool -> Bool -> Bool
type family Or a b where
  Or 'True _ = 'True
  Or _ 'True = 'True
  Or 'False 'False = 'False

-- | Type-level Not.
type Not :: Bool -> Bool
type family Not b where
  Not 'True = 'False
  Not 'False = 'True

-- | Check if symbol list is empty.
type NullSymbols :: [Symbol] -> Bool
type family NullSymbols xs where
  NullSymbols '[] = 'True
  NullSymbols (_ ': _) = 'False

-- | Type-level Elem for Symbols.
type ElemSymbol :: Symbol -> [Symbol] -> Bool
type family ElemSymbol x xs where
  ElemSymbol _ '[] = 'False
  ElemSymbol x (x ': _) = 'True
  ElemSymbol x (_ ': rest) = ElemSymbol x rest

-- | Type-level Elem for Types.
type ElemType :: Type -> [Type] -> Bool
type family ElemType x xs where
  ElemType _ '[] = 'False
  ElemType x (x ': _) = 'True
  ElemType x (_ ': rest) = ElemType x rest

-- | Check if all types in first list are in second list.
type AllIn :: [Type] -> [Type] -> Bool
type family AllIn xs ys where
  AllIn '[] _ = 'True
  AllIn (x ': rest) ys = And (ElemType x ys) (AllIn rest ys)

-- | Check if any symbol from first list is in second list.
type AnyElemSymbol :: [Symbol] -> [Symbol] -> Bool
type family AnyElemSymbol xs ys where
  AnyElemSymbol '[] _ = 'False
  AnyElemSymbol (x ': rest) ys = Or (ElemSymbol x ys) (AnyElemSymbol rest ys)

-- | Filter symbols not in the given list.
type FilterNotIn :: [Symbol] -> [Symbol] -> [Symbol]
type family FilterNotIn xs ys where
  FilterNotIn '[] _ = '[]
  FilterNotIn (x ': rest) ys =
    If (ElemSymbol x ys)
       (FilterNotIn rest ys)
       (x ': FilterNotIn rest ys)

-- | Append two symbol lists.
type AppendSymbols :: [Symbol] -> [Symbol] -> [Symbol]
type family AppendSymbols xs ys where
  AppendSymbols '[] ys = ys
  AppendSymbols (x ': rest) ys = x ': AppendSymbols rest ys

-- | Append two type lists.
type AppendTypes :: [Type] -> [Type] -> [Type]
type family AppendTypes xs ys where
  AppendTypes '[] ys = ys
  AppendTypes (x ': rest) ys = x ': AppendTypes rest ys

-- | Append Maybe type to list.
type AppendMaybeType :: Maybe Type -> [Type] -> [Type]
type family AppendMaybeType m xs where
  AppendMaybeType 'Nothing xs = xs
  AppendMaybeType ('Just x) xs = x ': xs

-- | Append Maybe pair to list.
type AppendMaybePair :: Maybe (Symbol, Type) -> [(Symbol, Type)] -> [(Symbol, Type)]
type family AppendMaybePair m xs where
  AppendMaybePair 'Nothing xs = xs
  AppendMaybePair ('Just p) xs = p ': xs

-- | Append two triple lists.
type AppendTriples :: [(Symbol, Symbol, Type)] -> [(Symbol, Symbol, Type)] -> [(Symbol, Symbol, Type)]
type family AppendTriples xs ys where
  AppendTriples '[] ys = ys
  AppendTriples (x ': rest) ys = x ': AppendTriples rest ys

-- | Type equality.
type TypeEq :: Type -> Type -> Bool
type family TypeEq a b where
  TypeEq a a = 'True
  TypeEq _ _ = 'False

-- | Symbol equality (explicit, since we can't use == for kind Symbol in all contexts).
type (==) :: Symbol -> Symbol -> Bool
type family a == b where
  a == a = 'True
  _ == _ = 'False

-- | Length of a type-level list.
type Length :: [k] -> Nat
type family Length xs where
  Length '[] = 0
  Length (_ ': rest) = 1 + Length rest
