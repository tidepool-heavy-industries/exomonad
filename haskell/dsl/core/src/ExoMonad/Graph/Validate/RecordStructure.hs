-- | Structural validation for record-based (Servant-style) graphs.
--
-- This module provides compile-time validation of graph topology for
-- graphs defined using the record syntax:
--
-- @
-- data MyGraph mode = MyGraph
--   { entry    :: mode :- EntryNode Message
--   , classify :: mode :- LLMNode :@ Input Message :@ Schema Intent
--   , exit     :: mode :- ExitNode Response
--   }
-- @
--
-- = Validation Rules
--
-- 1. __Reachability__: Every node must be reachable from EntryNode
-- 2. __Exit Coverage__: Every Logic node must have a path to Exit
-- 3. __No Dead Gotos__: Goto targets must be able to receive their payload
--
-- These validations work on the Generic representation of graph records.
module ExoMonad.Graph.Validate.RecordStructure
  ( -- * Main Structural Constraints
    AllFieldsReachable,
    AllLogicFieldsReachExit,
    NoDeadGotosRecord,
    AllLogicNodesHaveGoto,
    NoGotoSelfOnly,

    -- * Error Messages
    UnreachableFieldError,
    NoExitPathFieldError,
    DeadGotoFieldError,
    GotoTypeMismatchError,
    LogicNodeNoGotoError,
    GotoSelfOnlyError,

    -- * Error Formatting
    FormatTypeList,

    -- * Internal Type Families (exported for testing)
    ComputeReachableFields,
    ComputeExitReachingFields,
    FindUnreachableFields,
    FindNoExitPathFields,
  )
where

import Data.Kind (Constraint, Type)
import ExoMonad.Graph.Edges (GetInput, GetSchema)
import ExoMonad.Graph.Generic.Core (AsGraph, EntryNode, LLMNode, LogicNode)
import ExoMonad.Graph.Goto (Goto)
import ExoMonad.Graph.Types (Arrive, Self, UsesEffects, type (:@))
import ExoMonad.Graph.Types qualified as Types (Exit)
import GHC.Generics (C, D, Generic (..), K1 (..), M1 (..), Meta (..), Rep, S, (:*:) (..))
import GHC.TypeLits (ErrorMessage (..), Nat, Symbol, TypeError, type (+), type (-))

-- ════════════════════════════════════════════════════════════════════════════
-- FIELD EXTRACTION TYPE FAMILIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract field names from a Generic representation.
type FieldNames :: (Type -> Type) -> [Symbol]
type family FieldNames f where
  FieldNames (M1 D _ f) = FieldNames f
  FieldNames (M1 C _ f) = FieldNames f
  FieldNames (M1 S ('MetaSel ('Just name) _ _ _) _) = '[name]
  FieldNames (M1 S ('MetaSel 'Nothing _ _ _) _) = '[]
  FieldNames (l :*: r) = Append (FieldNames l) (FieldNames r)
  FieldNames (K1 _ _) = '[]

-- | Pair field names with their node definitions.
type FieldsWithNames :: (Type -> Type) -> [(Symbol, Type)]
type family FieldsWithNames f where
  FieldsWithNames (M1 D _ f) = FieldsWithNames f
  FieldsWithNames (M1 C _ f) = FieldsWithNames f
  FieldsWithNames (M1 S ('MetaSel ('Just name) _ _ _) (K1 _ def)) = '[ '(name, def)]
  FieldsWithNames (M1 S ('MetaSel 'Nothing _ _ _) _) = '[]
  FieldsWithNames (l :*: r) = Append (FieldsWithNames l) (FieldsWithNames r)
  FieldsWithNames _ = '[]

-- | Get fields with names from a graph type.
type FieldsWithNamesOf :: (Type -> Type) -> [(Symbol, Type)]
type FieldsWithNamesOf graph = FieldsWithNames (Rep (graph AsGraph))

-- | Extract EntryNode type from a graph record.
type GetEntryType :: (Type -> Type) -> Maybe Type
type family GetEntryType f where
  GetEntryType (M1 D _ f) = GetEntryType f
  GetEntryType (M1 C _ f) = GetEntryType f
  GetEntryType (M1 S _ (K1 _ (EntryNode a))) = 'Just a
  GetEntryType (M1 S _ _) = 'Nothing
  GetEntryType (l :*: r) = OrMaybe (GetEntryType l) (GetEntryType r)
  GetEntryType _ = 'Nothing

-- | Append two type-level lists.
type Append :: [k] -> [k] -> [k]
type family Append xs ys where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

-- | Return first Just, or Nothing if both Nothing.
type OrMaybe :: Maybe k -> Maybe k -> Maybe k
type family OrMaybe a b where
  OrMaybe ('Just x) _ = 'Just x
  OrMaybe 'Nothing b = b

-- ════════════════════════════════════════════════════════════════════════════
-- REACHABILITY VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validates that all nodes in a graph record are reachable from EntryNode.
--
-- A node is reachable if:
-- * Its Input type can be satisfied by the EntryNode type, OR
-- * Its Input type can be satisfied by EntryNode + Schema outputs of reachable nodes, OR
-- * It is the target of a Goto from a reachable Logic node
type AllFieldsReachable :: (Type -> Type) -> Constraint
type family AllFieldsReachable graph where
  AllFieldsReachable graph =
    CheckAllReachableFields
      ( FindUnreachableFields
          (FieldsWithNamesOf graph)
          (GetRecordEntryType graph)
      )

-- | Get EntryNode type from a graph record (unwrap Maybe).
type GetRecordEntryType :: (Type -> Type) -> Type
type family GetRecordEntryType graph where
  GetRecordEntryType graph = FromJust (GetEntryType (Rep (graph AsGraph)))

-- | Unwrap Maybe (error if Nothing).
type FromJust :: Maybe k -> k
type family FromJust m where
  FromJust ('Just x) = x
  FromJust 'Nothing = TypeError ('Text "Internal error: EntryNode type not found")

-- | Check if unreachable list is empty.
type CheckAllReachableFields :: [Symbol] -> Constraint
type family CheckAllReachableFields unreachable where
  CheckAllReachableFields '[] = ()
  CheckAllReachableFields (name ': _) = UnreachableFieldError name

-- | Find unreachable fields.
type FindUnreachableFields :: [(Symbol, Type)] -> Type -> [Symbol]
type family FindUnreachableFields fields entryType where
  FindUnreachableFields fields entryType =
    FilterNotInSymbols
      (CollectNodeFieldNames fields)
      (ComputeReachableFields fields entryType (LengthPairs fields))

-- | Collect field names that are nodes (not EntryNode/Exit).
type CollectNodeFieldNames :: [(Symbol, Type)] -> [Symbol]
type family CollectNodeFieldNames fields where
  CollectNodeFieldNames '[] = '[]
  CollectNodeFieldNames ('(name, def) ': rest) =
    If
      (IsNodeDef def)
      (name ': CollectNodeFieldNames rest)
      (CollectNodeFieldNames rest)

-- | Check if a definition is a node (LLMNode or LogicNode with annotations).
type IsNodeDef :: Type -> Bool
type family IsNodeDef def where
  IsNodeDef (node :@ _) = IsNodeDef node
  IsNodeDef (LLMNode _subtype) = 'True -- LLMNode now has subtype parameter
  IsNodeDef LogicNode = 'True
  IsNodeDef _ = 'False

-- | Compute reachable fields via fixed-point iteration.
type ComputeReachableFields :: [(Symbol, Type)] -> Type -> Nat -> [Symbol]
type family ComputeReachableFields fields entryType fuel where
  ComputeReachableFields _ _ 0 = '[]
  ComputeReachableFields fields entryType fuel =
    ComputeReachableFieldsStep fields '[entryType] '[] fuel

-- | Single step of field reachability.
type ComputeReachableFieldsStep :: [(Symbol, Type)] -> [Type] -> [Symbol] -> Nat -> [Symbol]
type family ComputeReachableFieldsStep fields available reached fuel where
  ComputeReachableFieldsStep _ _ reached 0 = reached
  ComputeReachableFieldsStep fields available reached fuel =
    ComputeReachableFieldsContinue
      fields
      available
      reached
      (FindNewlyReachableFields fields available reached)
      fuel

-- | Continue with newly reached fields.
type ComputeReachableFieldsContinue :: [(Symbol, Type)] -> [Type] -> [Symbol] -> [Symbol] -> Nat -> [Symbol]
type family ComputeReachableFieldsContinue fields available reached newlyReached fuel where
  ComputeReachableFieldsContinue _ _ reached '[] _ = reached
  ComputeReachableFieldsContinue fields available reached newlyReached fuel =
    ComputeReachableFieldsStep
      fields
      (AddFieldSchemaTypes fields newlyReached available)
      (AppendSymbols reached newlyReached)
      (fuel - 1)

-- | Find fields that become reachable.
type FindNewlyReachableFields :: [(Symbol, Type)] -> [Type] -> [Symbol] -> [Symbol]
type family FindNewlyReachableFields fields available reached where
  FindNewlyReachableFields '[] _ _ = '[]
  FindNewlyReachableFields ('(name, def) ': rest) available reached =
    If
      ( And
          (Not (ElemSymbol name reached))
          (And (IsNodeDef def) (InputSatisfied (GetInput def) available))
      )
      (name ': FindNewlyReachableFields rest available reached)
      (FindNewlyReachableFields rest available reached)

-- | Check if the Input type is satisfied by available types.
-- Handles tuples by checking if all components are available.
type InputSatisfied :: Maybe Type -> [Type] -> Bool
type family InputSatisfied mInput available where
  InputSatisfied 'Nothing _ = 'True -- No Input means always satisfied
  InputSatisfied ('Just (a, b)) available = And (ElemType a available) (ElemType b available)
  InputSatisfied ('Just t) available = ElemType t available

-- | Add Schema types from newly reached fields.
type AddFieldSchemaTypes :: [(Symbol, Type)] -> [Symbol] -> [Type] -> [Type]
type family AddFieldSchemaTypes fields newlyReached available where
  AddFieldSchemaTypes _ '[] available = available
  AddFieldSchemaTypes fields (name ': rest) available =
    AddFieldSchemaTypes
      fields
      rest
      (AppendMaybeType (GetFieldSchema fields name) available)

-- | Get Schema type for a field by name.
type GetFieldSchema :: [(Symbol, Type)] -> Symbol -> Maybe Type
type family GetFieldSchema fields name where
  GetFieldSchema '[] _ = 'Nothing
  GetFieldSchema ('(n, def) ': rest) name =
    If
      (n == name)
      (GetSchema def)
      (GetFieldSchema rest name)

-- ════════════════════════════════════════════════════════════════════════════
-- EXIT COVERAGE VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validates that all Logic nodes can reach Exit.
type AllLogicFieldsReachExit :: (Type -> Type) -> Constraint
type family AllLogicFieldsReachExit graph where
  AllLogicFieldsReachExit graph =
    CheckAllReachExit
      (FindNoExitPathFields (FieldsWithNamesOf graph))

-- | Check if no-exit list is empty.
type CheckAllReachExit :: [Symbol] -> Constraint
type family CheckAllReachExit noExit where
  CheckAllReachExit '[] = ()
  CheckAllReachExit (name ': _) = NoExitPathFieldError name

-- | Find Logic fields that can't reach Exit.
type FindNoExitPathFields :: [(Symbol, Type)] -> [Symbol]
type family FindNoExitPathFields fields where
  FindNoExitPathFields fields =
    FilterNotInSymbols
      (CollectLogicFieldNames fields)
      (ComputeExitReachingFields fields (LengthPairs fields))

-- | Collect names of Logic node fields.
type CollectLogicFieldNames :: [(Symbol, Type)] -> [Symbol]
type family CollectLogicFieldNames fields where
  CollectLogicFieldNames '[] = '[]
  CollectLogicFieldNames ('(name, def) ': rest) =
    If
      (IsLogicDef def)
      (name ': CollectLogicFieldNames rest)
      (CollectLogicFieldNames rest)

-- | Check if a definition is a Logic node.
type IsLogicDef :: Type -> Bool
type family IsLogicDef def where
  IsLogicDef (node :@ _) = IsLogicDef node
  IsLogicDef LogicNode = 'True
  IsLogicDef _ = 'False

-- | Compute fields that can reach Exit.
type ComputeExitReachingFields :: [(Symbol, Type)] -> Nat -> [Symbol]
type family ComputeExitReachingFields fields fuel where
  ComputeExitReachingFields _ 0 = '[]
  ComputeExitReachingFields fields fuel =
    ComputeExitReachingStep fields (FieldsWithGotoExit fields) fuel

-- | Single step of exit-reaching computation.
type ComputeExitReachingStep :: [(Symbol, Type)] -> [Symbol] -> Nat -> [Symbol]
type family ComputeExitReachingStep fields reaching fuel where
  ComputeExitReachingStep _ reaching 0 = reaching
  ComputeExitReachingStep fields reaching fuel =
    ComputeExitReachingContinue
      fields
      reaching
      (FindNewlyExitReachingFields fields reaching)
      fuel

-- | Continue exit-reaching computation.
type ComputeExitReachingContinue :: [(Symbol, Type)] -> [Symbol] -> [Symbol] -> Nat -> [Symbol]
type family ComputeExitReachingContinue fields reaching newlyReaching fuel where
  ComputeExitReachingContinue _ reaching '[] _ = reaching
  ComputeExitReachingContinue fields reaching newlyReaching fuel =
    ComputeExitReachingStep
      fields
      (AppendSymbols reaching newlyReaching)
      (fuel - 1)

-- | Find fields with direct Goto Exit.
type FieldsWithGotoExit :: [(Symbol, Type)] -> [Symbol]
type family FieldsWithGotoExit fields where
  FieldsWithGotoExit '[] = '[]
  FieldsWithGotoExit ('(name, def) ': rest) =
    If
      (FieldHasGotoExit def)
      (name ': FieldsWithGotoExit rest)
      (FieldsWithGotoExit rest)

-- | Check if a field definition has Goto Exit.
type FieldHasGotoExit :: Type -> Bool
type family FieldHasGotoExit def where
  FieldHasGotoExit def = HasGotoExitInMaybeEffects (GetUsesEffectsFixed def)

-- | Get UsesEffects with fixed kind.
type GetUsesEffectsFixed :: Type -> Maybe [Effect]
type family GetUsesEffectsFixed def where
  GetUsesEffectsFixed (_ :@ UsesEffects effs) = 'Just effs
  GetUsesEffectsFixed (node :@ _) = GetUsesEffectsFixed node
  GetUsesEffectsFixed _ = 'Nothing

-- | Effect kind alias (freer-simple effects have kind Type -> Type).
type Effect = Type -> Type

-- | Check for Goto Exit in Maybe effect list.
type HasGotoExitInMaybeEffects :: Maybe [Effect] -> Bool
type family HasGotoExitInMaybeEffects mEffs where
  HasGotoExitInMaybeEffects 'Nothing = 'False
  HasGotoExitInMaybeEffects ('Just effs) = HasGotoExitInEffects effs

-- | Check effect list for Goto Exit.
type HasGotoExitInEffects :: [Effect] -> Bool
type family HasGotoExitInEffects effs where
  HasGotoExitInEffects '[] = 'False
  HasGotoExitInEffects (Goto Types.Exit _ ': _) = 'True
  HasGotoExitInEffects (_ ': rest) = HasGotoExitInEffects rest

-- | Find fields that become exit-reaching.
type FindNewlyExitReachingFields :: [(Symbol, Type)] -> [Symbol] -> [Symbol]
type family FindNewlyExitReachingFields fields reaching where
  FindNewlyExitReachingFields '[] _ = '[]
  FindNewlyExitReachingFields ('(name, def) ': rest) reaching =
    If
      ( And
          (Not (ElemSymbol name reaching))
          (FieldHasGotoToAny def reaching)
      )
      (name ': FindNewlyExitReachingFields rest reaching)
      (FindNewlyExitReachingFields rest reaching)

-- | Check if field has Goto to any of the targets.
type FieldHasGotoToAny :: Type -> [Symbol] -> Bool
type family FieldHasGotoToAny def targets where
  FieldHasGotoToAny def targets =
    AnyElemSymbol (GetFieldGotoTargetNames def) targets

-- | Get Goto target names from a field definition.
type GetFieldGotoTargetNames :: Type -> [Symbol]
type family GetFieldGotoTargetNames def where
  GetFieldGotoTargetNames def =
    ProjectSymbolsFromMaybeEffects (GetUsesEffectsFixed def)

-- | Project symbols from Maybe effect list.
type ProjectSymbolsFromMaybeEffects :: Maybe [Effect] -> [Symbol]
type family ProjectSymbolsFromMaybeEffects mEffs where
  ProjectSymbolsFromMaybeEffects 'Nothing = '[]
  ProjectSymbolsFromMaybeEffects ('Just effs) = ProjectGotoSymbols effs

-- | Project Goto target symbols from effect list.
type ProjectGotoSymbols :: [Effect] -> [Symbol]
type family ProjectGotoSymbols effs where
  ProjectGotoSymbols '[] = '[]
  ProjectGotoSymbols (Goto (name :: Symbol) _ ': rest) = name ': ProjectGotoSymbols rest
  ProjectGotoSymbols (Goto Types.Exit _ ': rest) = ProjectGotoSymbols rest -- Skip Exit
  ProjectGotoSymbols (_ ': rest) = ProjectGotoSymbols rest

-- ════════════════════════════════════════════════════════════════════════════
-- DEAD GOTO VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validates that all Goto targets can receive their payload.
type NoDeadGotosRecord :: (Type -> Type) -> Constraint
type family NoDeadGotosRecord graph where
  NoDeadGotosRecord graph =
    CheckNoDeadGotosRecord
      ( FindDeadGotosInFields
          (FieldsWithNamesOf graph)
          (GetRecordEntryType graph)
      )

-- | Check if dead goto list is empty.
-- The tuple now includes target's Input for better error messages.
type CheckNoDeadGotosRecord :: [(Symbol, Symbol, Type, Maybe Type)] -> Constraint
type family CheckNoDeadGotosRecord deadGotos where
  CheckNoDeadGotosRecord '[] = ()
  CheckNoDeadGotosRecord ('(src, target, payload, targetInput) ': _) =
    GotoTypeMismatchError src target payload targetInput

-- | Find dead Gotos in all fields.
-- Returns 4-tuples: (srcName, targetName, payload, targetInput)
type FindDeadGotosInFields :: [(Symbol, Type)] -> Type -> [(Symbol, Symbol, Type, Maybe Type)]
type family FindDeadGotosInFields fields entryType where
  FindDeadGotosInFields fields entryType =
    FindDeadGotosHelper fields fields entryType

-- | Helper that keeps track of all fields while iterating.
type FindDeadGotosHelper :: [(Symbol, Type)] -> [(Symbol, Type)] -> Type -> [(Symbol, Symbol, Type, Maybe Type)]
type family FindDeadGotosHelper remaining allFields entryType where
  FindDeadGotosHelper '[] _ _ = '[]
  FindDeadGotosHelper ('(name, def) ': rest) allFields entryType =
    AppendQuads
      (CheckFieldGotos name def allFields entryType)
      (FindDeadGotosHelper rest allFields entryType)

-- | Check Gotos in a single field.
type CheckFieldGotos :: Symbol -> Type -> [(Symbol, Type)] -> Type -> [(Symbol, Symbol, Type, Maybe Type)]
type family CheckFieldGotos srcName def allFields entryType where
  CheckFieldGotos srcName def allFields entryType =
    CheckGotoList srcName (GetFieldGotoTargetsWithPayloads def) allFields entryType

-- | Get (target, payload) pairs from a field definition.
type GetFieldGotoTargetsWithPayloads :: Type -> [(Symbol, Type)]
type family GetFieldGotoTargetsWithPayloads def where
  GetFieldGotoTargetsWithPayloads def =
    GetGotoTargetsFromMaybeEffects (GetUsesEffectsFixed def)

-- | Get Goto targets from Maybe effect list.
type GetGotoTargetsFromMaybeEffects :: Maybe [Effect] -> [(Symbol, Type)]
type family GetGotoTargetsFromMaybeEffects mEffs where
  GetGotoTargetsFromMaybeEffects 'Nothing = '[]
  GetGotoTargetsFromMaybeEffects ('Just effs) = GetGotoTargetsFromEffects effs

-- | Get Goto targets from effect list.
type GetGotoTargetsFromEffects :: [Effect] -> [(Symbol, Type)]
type family GetGotoTargetsFromEffects effs where
  GetGotoTargetsFromEffects '[] = '[]
  GetGotoTargetsFromEffects (Goto (name :: Symbol) payload ': rest) =
    '(name, payload) ': GetGotoTargetsFromEffects rest
  GetGotoTargetsFromEffects (Goto Types.Exit _ ': rest) = GetGotoTargetsFromEffects rest
  GetGotoTargetsFromEffects (_ ': rest) = GetGotoTargetsFromEffects rest

-- | Check a list of Gotos.
type CheckGotoList :: Symbol -> [(Symbol, Type)] -> [(Symbol, Type)] -> Type -> [(Symbol, Symbol, Type, Maybe Type)]
type family CheckGotoList srcName gotos allFields entryType where
  CheckGotoList _ '[] _ _ = '[]
  CheckGotoList srcName ('(target, payload) ': rest) allFields entryType =
    AppendQuads
      (CheckSingleFieldGoto srcName target payload allFields entryType)
      (CheckGotoList srcName rest allFields entryType)

-- | Check a single Goto.
type CheckSingleFieldGoto :: Symbol -> Symbol -> Type -> [(Symbol, Type)] -> Type -> [(Symbol, Symbol, Type, Maybe Type)]
type family CheckSingleFieldGoto srcName targetName payload allFields entryType where
  CheckSingleFieldGoto srcName targetName payload allFields entryType =
    CheckGotoWithTarget
      srcName
      targetName
      payload
      (GetFieldInput allFields targetName)
      (payload ': entryType ': CollectAllFieldSchemas allFields)

-- | Get Input type for a field by name.
type GetFieldInput :: [(Symbol, Type)] -> Symbol -> Maybe Type
type family GetFieldInput fields name where
  GetFieldInput '[] _ = 'Nothing
  GetFieldInput ('(n, def) ': rest) name =
    If
      (n == name)
      (GetInput def)
      (GetFieldInput rest name)

-- | Collect all Schema types from fields.
type CollectAllFieldSchemas :: [(Symbol, Type)] -> [Type]
type family CollectAllFieldSchemas fields where
  CollectAllFieldSchemas '[] = '[]
  CollectAllFieldSchemas ('(_, def) ': rest) =
    AppendMaybeType (GetSchema def) (CollectAllFieldSchemas rest)

-- | Check Goto with resolved target input.
-- Returns 4-tuple including targetInput for better error messages.
type CheckGotoWithTarget :: Symbol -> Symbol -> Type -> Maybe Type -> [Type] -> [(Symbol, Symbol, Type, Maybe Type)]
type family CheckGotoWithTarget srcName targetName payload targetInput available where
  CheckGotoWithTarget srcName targetName payload targetInput available =
    If
      (InputSatisfied targetInput available)
      '[]
      '[ '(srcName, targetName, payload, targetInput)]

-- ════════════════════════════════════════════════════════════════════════════
-- ERROR MESSAGES
-- ════════════════════════════════════════════════════════════════════════════

-- | Error when a field is unreachable.
type UnreachableFieldError :: Symbol -> Constraint
type UnreachableFieldError name =
  TypeError
    ( 'Text "Graph validation failed: unreachable node"
        ':$$: 'Text "Field '"
        ':<>: 'Text name
        ':<>: 'Text "' cannot be reached from EntryNode."
        ':$$: 'Text ""
        ':$$: 'Text "A node is reachable if:"
        ':$$: 'Text "  - Its Input type matches the EntryNode type"
        ':$$: 'Text "  - Its Input type matches a Schema output from a reachable node"
        ':$$: 'Text "  - It is the target of a Goto from a reachable Logic node"
        ':$$: 'Text ""
        ':$$: 'Text "Fix: Ensure some reachable node provides this node's Input type."
    )

-- | Error when a Logic field can't reach Exit.
type NoExitPathFieldError :: Symbol -> Constraint
type NoExitPathFieldError name =
  TypeError
    ( 'Text "Graph validation failed: Logic node cannot reach Exit"
        ':$$: 'Text "Field '"
        ':<>: 'Text name
        ':<>: 'Text "' has no path to Exit."
        ':$$: 'Text ""
        ':$$: 'Text "This creates a potential infinite loop or dead end."
        ':$$: 'Text ""
        ':$$: 'Text "Fix: Add 'Goto Exit result' to UsesEffects,"
        ':$$: 'Text "     or add Goto to a node that reaches Exit."
    )

-- | Error when a Goto payload type doesn't match target's Input.
--
-- This improved error shows both what was sent and what was expected,
-- making type mismatches much easier to debug.
type GotoTypeMismatchError :: Symbol -> Symbol -> Type -> Maybe Type -> Constraint
type GotoTypeMismatchError srcName targetName payload targetInput =
  TypeError
    ( 'Text "Graph validation failed: Goto payload type mismatch"
        ':$$: 'Text ""
        ':$$: 'Text "Node '"
        ':<>: 'Text srcName
        ':<>: 'Text "' sends:"
        ':$$: 'Text "  Goto \""
        ':<>: 'Text targetName
        ':<>: 'Text "\" "
        ':<>: 'ShowType payload
        ':$$: 'Text ""
        ':$$: 'Text "But target '"
        ':<>: 'Text targetName
        ':<>: 'Text "' expects:"
        ':$$: FormatMaybeType targetInput
        ':$$: 'Text ""
        ':$$: 'Text "The Goto payload must match the target's Input type."
        ':$$: 'Text "Fix: Change the payload type or adjust the target's Input."
    )

-- | Legacy alias for backwards compatibility with Validate.hs exports.
type DeadGotoFieldError :: Symbol -> Symbol -> Type -> Constraint
type DeadGotoFieldError srcName targetName payload =
  GotoTypeMismatchError srcName targetName payload 'Nothing

-- ════════════════════════════════════════════════════════════════════════════
-- LOGIC NODE TRANSITION VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validates that all Logic nodes have at least one Goto effect.
--
-- A Logic node with UsesEffects but no Goto cannot transition and will deadlock.
type AllLogicNodesHaveGoto :: (Type -> Type) -> Constraint
type family AllLogicNodesHaveGoto graph where
  AllLogicNodesHaveGoto graph =
    CheckLogicNodesHaveGoto (FieldsWithNamesOf graph)

-- | Check each field for Logic-without-Goto violation.
type CheckLogicNodesHaveGoto :: [(Symbol, Type)] -> Constraint
type family CheckLogicNodesHaveGoto fields where
  CheckLogicNodesHaveGoto '[] = ()
  CheckLogicNodesHaveGoto ('(name, def) ': rest) =
    (CheckSingleLogicNodeHasGoto name def, CheckLogicNodesHaveGoto rest)

-- | Check a single node.
type CheckSingleLogicNodeHasGoto :: Symbol -> Type -> Constraint
type family CheckSingleLogicNodeHasGoto name def where
  CheckSingleLogicNodeHasGoto name def =
    If
      (And (IsLogicDef def) (Not (HasAnyGoto def)))
      (LogicNodeNoGotoError name)
      (() :: Constraint)

-- | Check if a node definition has any Goto (including Exit, Self, or named).
type HasAnyGoto :: Type -> Bool
type family HasAnyGoto def where
  HasAnyGoto def = HasAnyGotoInMaybeEffects (GetUsesEffectsFixed def)

-- | Check Maybe effect list for any Goto.
type HasAnyGotoInMaybeEffects :: Maybe [Effect] -> Bool
type family HasAnyGotoInMaybeEffects mEffs where
  HasAnyGotoInMaybeEffects 'Nothing = 'False
  HasAnyGotoInMaybeEffects ('Just effs) = HasAnyGotoInEffects effs

-- | Check effect list for any Goto or Arrive.
-- Both Goto and Arrive are valid exit paths for nodes.
type HasAnyGotoInEffects :: [Effect] -> Bool
type family HasAnyGotoInEffects effs where
  HasAnyGotoInEffects '[] = 'False
  HasAnyGotoInEffects (Goto _ _ ': _) = 'True
  HasAnyGotoInEffects (Arrive _ _ ': _) = 'True -- Arrive is also a valid exit path
  HasAnyGotoInEffects (_ ': rest) = HasAnyGotoInEffects rest

-- | Error when a Logic node has no Goto effects.
type LogicNodeNoGotoError :: Symbol -> Constraint
type LogicNodeNoGotoError name =
  TypeError
    ( 'Text "Graph validation failed: Logic node cannot transition"
        ':$$: 'Text ""
        ':$$: 'Text "Node '"
        ':<>: 'Text name
        ':<>: 'Text "' is a Logic node but has no Goto effects."
        ':$$: 'Text "Without a Goto, the node cannot transition and will deadlock at runtime."
        ':$$: 'Text ""
        ':$$: 'Text "Fix: Add Goto to UsesEffects:"
        ':$$: 'Text "  LogicNode :@ UsesEffects '[Goto \"nextNode\" Payload]"
        ':$$: 'Text "  LogicNode :@ UsesEffects '[Goto Exit Result]"
    )

-- ════════════════════════════════════════════════════════════════════════════
-- GOTO SELF ONLY VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validates that nodes with Goto Self also have another exit path.
--
-- A node with only Goto Self creates an infinite loop with no escape.
type NoGotoSelfOnly :: (Type -> Type) -> Constraint
type family NoGotoSelfOnly graph where
  NoGotoSelfOnly graph = CheckNoGotoSelfOnly (FieldsWithNamesOf graph)

-- | Check each field for Goto-Self-only violation.
type CheckNoGotoSelfOnly :: [(Symbol, Type)] -> Constraint
type family CheckNoGotoSelfOnly fields where
  CheckNoGotoSelfOnly '[] = ()
  CheckNoGotoSelfOnly ('(name, def) ': rest) =
    (CheckSingleNodeGotoSelfOnly name def, CheckNoGotoSelfOnly rest)

-- | Check a single node for Goto Self only.
type CheckSingleNodeGotoSelfOnly :: Symbol -> Type -> Constraint
type family CheckSingleNodeGotoSelfOnly name def where
  CheckSingleNodeGotoSelfOnly name def =
    If
      (And (HasGotoSelf def) (Not (HasNonSelfGoto def)))
      (GotoSelfOnlyError name)
      (() :: Constraint)

-- | Check if node has Goto Self.
type HasGotoSelf :: Type -> Bool
type family HasGotoSelf def where
  HasGotoSelf def = HasGotoSelfInMaybeEffects (GetUsesEffectsFixed def)

type HasGotoSelfInMaybeEffects :: Maybe [Effect] -> Bool
type family HasGotoSelfInMaybeEffects mEffs where
  HasGotoSelfInMaybeEffects 'Nothing = 'False
  HasGotoSelfInMaybeEffects ('Just effs) = HasGotoSelfInEffects effs

type HasGotoSelfInEffects :: [Effect] -> Bool
type family HasGotoSelfInEffects effs where
  HasGotoSelfInEffects '[] = 'False
  HasGotoSelfInEffects (Goto Self _ ': _) = 'True
  HasGotoSelfInEffects (_ ': rest) = HasGotoSelfInEffects rest

-- | Check if node has any non-Self Goto (named target or Exit).
type HasNonSelfGoto :: Type -> Bool
type family HasNonSelfGoto def where
  HasNonSelfGoto def = HasNonSelfGotoInMaybeEffects (GetUsesEffectsFixed def)

type HasNonSelfGotoInMaybeEffects :: Maybe [Effect] -> Bool
type family HasNonSelfGotoInMaybeEffects mEffs where
  HasNonSelfGotoInMaybeEffects 'Nothing = 'False
  HasNonSelfGotoInMaybeEffects ('Just effs) = HasNonSelfGotoInEffects effs

type HasNonSelfGotoInEffects :: [Effect] -> Bool
type family HasNonSelfGotoInEffects effs where
  HasNonSelfGotoInEffects '[] = 'False
  HasNonSelfGotoInEffects (Goto Self _ ': rest) = HasNonSelfGotoInEffects rest
  HasNonSelfGotoInEffects (Goto _ _ ': _) = 'True -- Named or Exit
  HasNonSelfGotoInEffects (Arrive _ _ ': _) = 'True -- Arrive is also an exit path (for ForkNode workers)
  HasNonSelfGotoInEffects (_ ': rest) = HasNonSelfGotoInEffects rest

-- | Error when a node only has Goto Self.
type GotoSelfOnlyError :: Symbol -> Constraint
type GotoSelfOnlyError name =
  TypeError
    ( 'Text "Graph validation failed: infinite loop detected"
        ':$$: 'Text ""
        ':$$: 'Text "Node '"
        ':<>: 'Text name
        ':<>: 'Text "' only has Goto Self with no other exit."
        ':$$: 'Text "This creates an infinite loop - the node can never terminate."
        ':$$: 'Text ""
        ':$$: 'Text "Fix: Add an exit path:"
        ':$$: 'Text "  UsesEffects '[Goto Self Payload, Goto Exit Result]"
        ':$$: 'Text "  UsesEffects '[Goto Self Payload, Goto \"nextNode\" Payload]"
    )

-- ════════════════════════════════════════════════════════════════════════════
-- TYPE-LEVEL UTILITIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Format a list of types for display in error messages.
type FormatTypeList :: [Type] -> ErrorMessage
type family FormatTypeList ts where
  FormatTypeList '[] = 'Text "  (none)"
  FormatTypeList '[t] = 'Text "  • " ':<>: 'ShowType t
  FormatTypeList (t ': rest) = 'Text "  • " ':<>: 'ShowType t ':$$: FormatTypeList rest

-- | Format a Maybe Type for display in error messages.
type FormatMaybeType :: Maybe Type -> ErrorMessage
type family FormatMaybeType mt where
  FormatMaybeType 'Nothing = 'Text "  (no Input - accepts any type)"
  FormatMaybeType ('Just t) = 'Text "  Input " ':<>: 'ShowType t

-- | Type-level If.
type If :: Bool -> k -> k -> k
type family If cond t f where
  If 'True t _ = t
  If 'False _ f = f

-- | Type-level And.
type And :: Bool -> Bool -> Bool
type family And a b where
  And 'True 'True = 'True
  And _ _ = 'False

-- | Type-level Not.
type Not :: Bool -> Bool
type family Not b where
  Not 'True = 'False
  Not 'False = 'True

-- | Symbol equality.
type (==) :: Symbol -> Symbol -> Bool
type family a == b where
  a == a = 'True
  _ == _ = 'False

-- | Symbol membership.
type ElemSymbol :: Symbol -> [Symbol] -> Bool
type family ElemSymbol x xs where
  ElemSymbol _ '[] = 'False
  ElemSymbol x (x ': _) = 'True
  ElemSymbol x (_ ': rest) = ElemSymbol x rest

-- | Type membership.
type ElemType :: Type -> [Type] -> Bool
type family ElemType x xs where
  ElemType _ '[] = 'False
  ElemType x (x ': _) = 'True
  ElemType x (_ ': rest) = ElemType x rest

-- | All types in first list are in second.
type AllIn :: [Type] -> [Type] -> Bool
type family AllIn xs ys where
  AllIn '[] _ = 'True
  AllIn (x ': rest) ys = And (ElemType x ys) (AllIn rest ys)

-- | Any symbol in first list is in second.
type AnyElemSymbol :: [Symbol] -> [Symbol] -> Bool
type family AnyElemSymbol xs ys where
  AnyElemSymbol '[] _ = 'False
  AnyElemSymbol (x ': rest) ys =
    If (ElemSymbol x ys) 'True (AnyElemSymbol rest ys)

-- | Filter symbols not in list.
type FilterNotInSymbols :: [Symbol] -> [Symbol] -> [Symbol]
type family FilterNotInSymbols xs ys where
  FilterNotInSymbols '[] _ = '[]
  FilterNotInSymbols (x ': rest) ys =
    If
      (ElemSymbol x ys)
      (FilterNotInSymbols rest ys)
      (x ': FilterNotInSymbols rest ys)

-- | Append symbol lists.
type AppendSymbols :: [Symbol] -> [Symbol] -> [Symbol]
type family AppendSymbols xs ys where
  AppendSymbols '[] ys = ys
  AppendSymbols (x ': rest) ys = x ': AppendSymbols rest ys

-- | Append Maybe type to list.
type AppendMaybeType :: Maybe Type -> [Type] -> [Type]
type family AppendMaybeType m xs where
  AppendMaybeType 'Nothing xs = xs
  AppendMaybeType ('Just x) xs = x ': xs

-- | Append 4-tuple lists (for dead goto tracking).
type AppendQuads :: [(Symbol, Symbol, Type, Maybe Type)] -> [(Symbol, Symbol, Type, Maybe Type)] -> [(Symbol, Symbol, Type, Maybe Type)]
type family AppendQuads xs ys where
  AppendQuads '[] ys = ys
  AppendQuads (x ': rest) ys = x ': AppendQuads rest ys

-- | Length of pair list.
type LengthPairs :: [(Symbol, Type)] -> Nat
type family LengthPairs xs where
  LengthPairs '[] = 0
  LengthPairs (_ ': rest) = 1 + LengthPairs rest
