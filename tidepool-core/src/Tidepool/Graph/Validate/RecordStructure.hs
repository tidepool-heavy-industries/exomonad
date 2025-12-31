{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Structural validation for record-based (Servant-style) graphs.
--
-- This module provides compile-time validation of graph topology for
-- graphs defined using the record syntax:
--
-- @
-- data MyGraph mode = MyGraph
--   { entry    :: mode :- Entry Message
--   , classify :: mode :- LLMNode :@ Needs '[Message] :@ Schema Intent
--   , exit     :: mode :- Exit Response
--   }
-- @
--
-- = Validation Rules
--
-- 1. __Reachability__: Every node must be reachable from Entry
-- 2. __Exit Coverage__: Every Logic node must have a path to Exit
-- 3. __No Dead Gotos__: Goto targets must be able to receive their payload
--
-- These validations work on the Generic representation of graph records.
module Tidepool.Graph.Validate.RecordStructure
  ( -- * Main Structural Constraints
    AllFieldsReachable
  , AllLogicFieldsReachExit
  , NoDeadGotosRecord

    -- * Error Messages
  , UnreachableFieldError
  , NoExitPathFieldError
  , DeadGotoFieldError

    -- * Internal Type Families (exported for testing)
  , ComputeReachableFields
  , ComputeExitReachingFields
  , FindUnreachableFields
  , FindNoExitPathFields
  ) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..), Nat, type (-), type (+))
import GHC.Generics (Generic(..), K1(..), M1(..), (:*:)(..), Meta(..), S, D, C, Rep)

import Tidepool.Graph.Types (type (:@), Needs, Schema, UsesEffects)
import qualified Tidepool.Graph.Types as Types (Exit)
import Tidepool.Graph.Edges (GetNeeds, GetSchema)
import Tidepool.Graph.Goto (Goto)
import Tidepool.Graph.Generic.Core (GraphMode(..), AsGraph, LLMNode, LogicNode, Entry, Exit)

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
  FieldsWithNames (M1 S ('MetaSel ('Just name) _ _ _) (K1 _ def)) = '[ '(name, def) ]
  FieldsWithNames (M1 S ('MetaSel 'Nothing _ _ _) _) = '[]
  FieldsWithNames (l :*: r) = Append (FieldsWithNames l) (FieldsWithNames r)
  FieldsWithNames _ = '[]

-- | Get field names from a graph type.
type FieldNamesOf :: (Type -> Type) -> [Symbol]
type FieldNamesOf graph = FieldNames (Rep (graph AsGraph))

-- | Get fields with names from a graph type.
type FieldsWithNamesOf :: (Type -> Type) -> [(Symbol, Type)]
type FieldsWithNamesOf graph = FieldsWithNames (Rep (graph AsGraph))

-- | Extract Entry type from a graph record.
type GetEntryType :: (Type -> Type) -> Maybe Type
type family GetEntryType f where
  GetEntryType (M1 D _ f) = GetEntryType f
  GetEntryType (M1 C _ f) = GetEntryType f
  GetEntryType (M1 S _ (K1 _ (Entry a))) = 'Just a
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

-- | Validates that all nodes in a graph record are reachable from Entry.
--
-- A node is reachable if:
-- * Its Needs can be satisfied by the Entry type, OR
-- * Its Needs can be satisfied by Entry + Schema outputs of reachable nodes, OR
-- * It is the target of a Goto from a reachable Logic node
type AllFieldsReachable :: (Type -> Type) -> Constraint
type family AllFieldsReachable graph where
  AllFieldsReachable graph =
    CheckAllReachableFields
      (FindUnreachableFields
        (FieldsWithNamesOf graph)
        (GetRecordEntryType graph))

-- | Get Entry type from a graph record (unwrap Maybe).
type GetRecordEntryType :: (Type -> Type) -> Type
type family GetRecordEntryType graph where
  GetRecordEntryType graph = FromJust (GetEntryType (Rep (graph AsGraph)))

-- | Unwrap Maybe (error if Nothing).
type FromJust :: Maybe k -> k
type family FromJust m where
  FromJust ('Just x) = x
  FromJust 'Nothing = TypeError ('Text "Internal error: Entry type not found")

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

-- | Collect field names that are nodes (not Entry/Exit).
type CollectNodeFieldNames :: [(Symbol, Type)] -> [Symbol]
type family CollectNodeFieldNames fields where
  CollectNodeFieldNames '[] = '[]
  CollectNodeFieldNames ('(name, def) ': rest) =
    If (IsNodeDef def)
       (name ': CollectNodeFieldNames rest)
       (CollectNodeFieldNames rest)

-- | Check if a definition is a node (LLMNode or LogicNode with annotations).
type IsNodeDef :: Type -> Bool
type family IsNodeDef def where
  IsNodeDef (node :@ _) = IsNodeDef node
  IsNodeDef LLMNode = 'True
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
    If (And (Not (ElemSymbol name reached))
            (And (IsNodeDef def) (AllIn (GetNeeds def) available)))
       (name ': FindNewlyReachableFields rest available reached)
       (FindNewlyReachableFields rest available reached)

-- | Add Schema types from newly reached fields.
type AddFieldSchemaTypes :: [(Symbol, Type)] -> [Symbol] -> [Type] -> [Type]
type family AddFieldSchemaTypes fields newlyReached available where
  AddFieldSchemaTypes _ '[] available = available
  AddFieldSchemaTypes fields (name ': rest) available =
    AddFieldSchemaTypes fields rest
      (AppendMaybeType (GetFieldSchema fields name) available)

-- | Get Schema type for a field by name.
type GetFieldSchema :: [(Symbol, Type)] -> Symbol -> Maybe Type
type family GetFieldSchema fields name where
  GetFieldSchema '[] _ = 'Nothing
  GetFieldSchema ('(n, def) ': rest) name =
    If (n == name)
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
    If (IsLogicDef def)
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
    If (FieldHasGotoExit def)
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

-- | Effect kind alias.
type Effect = (Type -> Type) -> Type -> Type

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
    If (And (Not (ElemSymbol name reaching))
            (FieldHasGotoToAny def reaching))
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
  ProjectGotoSymbols (Goto Types.Exit _ ': rest) = ProjectGotoSymbols rest  -- Skip Exit
  ProjectGotoSymbols (_ ': rest) = ProjectGotoSymbols rest

-- ════════════════════════════════════════════════════════════════════════════
-- DEAD GOTO VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validates that all Goto targets can receive their payload.
type NoDeadGotosRecord :: (Type -> Type) -> Constraint
type family NoDeadGotosRecord graph where
  NoDeadGotosRecord graph =
    CheckNoDeadGotosRecord
      (FindDeadGotosInFields
        (FieldsWithNamesOf graph)
        (GetRecordEntryType graph))

-- | Check if dead goto list is empty.
type CheckNoDeadGotosRecord :: [(Symbol, Symbol, Type)] -> Constraint
type family CheckNoDeadGotosRecord deadGotos where
  CheckNoDeadGotosRecord '[] = ()
  CheckNoDeadGotosRecord ('(src, target, payload) ': _) =
    DeadGotoFieldError src target payload

-- | Find dead Gotos in all fields.
type FindDeadGotosInFields :: [(Symbol, Type)] -> Type -> [(Symbol, Symbol, Type)]
type family FindDeadGotosInFields fields entryType where
  FindDeadGotosInFields fields entryType =
    FindDeadGotosHelper fields fields entryType

-- | Helper that keeps track of all fields while iterating.
type FindDeadGotosHelper :: [(Symbol, Type)] -> [(Symbol, Type)] -> Type -> [(Symbol, Symbol, Type)]
type family FindDeadGotosHelper remaining allFields entryType where
  FindDeadGotosHelper '[] _ _ = '[]
  FindDeadGotosHelper ('(name, def) ': rest) allFields entryType =
    AppendTriples
      (CheckFieldGotos name def allFields entryType)
      (FindDeadGotosHelper rest allFields entryType)

-- | Check Gotos in a single field.
type CheckFieldGotos :: Symbol -> Type -> [(Symbol, Type)] -> Type -> [(Symbol, Symbol, Type)]
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
type CheckGotoList :: Symbol -> [(Symbol, Type)] -> [(Symbol, Type)] -> Type -> [(Symbol, Symbol, Type)]
type family CheckGotoList srcName gotos allFields entryType where
  CheckGotoList _ '[] _ _ = '[]
  CheckGotoList srcName ('(target, payload) ': rest) allFields entryType =
    AppendTriples
      (CheckSingleFieldGoto srcName target payload allFields entryType)
      (CheckGotoList srcName rest allFields entryType)

-- | Check a single Goto.
type CheckSingleFieldGoto :: Symbol -> Symbol -> Type -> [(Symbol, Type)] -> Type -> [(Symbol, Symbol, Type)]
type family CheckSingleFieldGoto srcName targetName payload allFields entryType where
  CheckSingleFieldGoto srcName targetName payload allFields entryType =
    CheckGotoWithTarget
      srcName
      targetName
      payload
      (GetFieldNeeds allFields targetName)
      (payload ': entryType ': CollectAllFieldSchemas allFields)

-- | Get Needs for a field by name.
type GetFieldNeeds :: [(Symbol, Type)] -> Symbol -> [Type]
type family GetFieldNeeds fields name where
  GetFieldNeeds '[] _ = '[]
  GetFieldNeeds ('(n, def) ': rest) name =
    If (n == name)
       (GetNeeds def)
       (GetFieldNeeds rest name)

-- | Collect all Schema types from fields.
type CollectAllFieldSchemas :: [(Symbol, Type)] -> [Type]
type family CollectAllFieldSchemas fields where
  CollectAllFieldSchemas '[] = '[]
  CollectAllFieldSchemas ('(_, def) ': rest) =
    AppendMaybeType (GetSchema def) (CollectAllFieldSchemas rest)

-- | Check Goto with resolved target needs.
type CheckGotoWithTarget :: Symbol -> Symbol -> Type -> [Type] -> [Type] -> [(Symbol, Symbol, Type)]
type family CheckGotoWithTarget srcName targetName payload targetNeeds available where
  CheckGotoWithTarget srcName targetName payload targetNeeds available =
    If (AllIn targetNeeds available)
       '[]
       '[ '(srcName, targetName, payload) ]

-- ════════════════════════════════════════════════════════════════════════════
-- ERROR MESSAGES
-- ════════════════════════════════════════════════════════════════════════════

-- | Error when a field is unreachable.
type UnreachableFieldError :: Symbol -> Constraint
type UnreachableFieldError name = TypeError
  ('Text "Graph validation failed: unreachable node"
   ':$$: 'Text "Field '" ':<>: 'Text name ':<>: 'Text "' cannot be reached from Entry."
   ':$$: 'Text ""
   ':$$: 'Text "A node is reachable if:"
   ':$$: 'Text "  - Its Needs are satisfied by Entry"
   ':$$: 'Text "  - Its Needs are satisfied by Schema outputs of reachable nodes"
   ':$$: 'Text "  - It is the target of a Goto from a reachable Logic node"
   ':$$: 'Text ""
   ':$$: 'Text "Fix: Ensure some reachable node provides what this node Needs."
  )

-- | Error when a Logic field can't reach Exit.
type NoExitPathFieldError :: Symbol -> Constraint
type NoExitPathFieldError name = TypeError
  ('Text "Graph validation failed: Logic node cannot reach Exit"
   ':$$: 'Text "Field '" ':<>: 'Text name ':<>: 'Text "' has no path to Exit."
   ':$$: 'Text ""
   ':$$: 'Text "This creates a potential infinite loop or dead end."
   ':$$: 'Text ""
   ':$$: 'Text "Fix: Add 'Goto Exit result' to UsesEffects,"
   ':$$: 'Text "     or add Goto to a node that reaches Exit."
  )

-- | Error when a Goto target can't receive its payload.
type DeadGotoFieldError :: Symbol -> Symbol -> Type -> Constraint
type DeadGotoFieldError srcName targetName payload = TypeError
  ('Text "Graph validation failed: dead Goto"
   ':$$: 'Text "Field '" ':<>: 'Text srcName ':<>: 'Text "' has:"
   ':$$: 'Text "  Goto \"" ':<>: 'Text targetName ':<>: 'Text "\" " ':<>: 'ShowType payload
   ':$$: 'Text ""
   ':$$: 'Text "But field '" ':<>: 'Text targetName ':<>: 'Text "' Needs types that won't be available."
   ':$$: 'Text ""
   ':$$: 'Text "Fix: Ensure the Goto payload matches what the target Needs."
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
    If (ElemSymbol x ys)
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

-- | Append triple lists.
type AppendTriples :: [(Symbol, Symbol, Type)] -> [(Symbol, Symbol, Type)] -> [(Symbol, Symbol, Type)]
type family AppendTriples xs ys where
  AppendTriples '[] ys = ys
  AppendTriples (x ': rest) ys = x ': AppendTriples rest ys

-- | Length of pair list.
type LengthPairs :: [(Symbol, Type)] -> Nat
type family LengthPairs xs where
  LengthPairs '[] = 0
  LengthPairs (_ ': rest) = 1 + LengthPairs rest
