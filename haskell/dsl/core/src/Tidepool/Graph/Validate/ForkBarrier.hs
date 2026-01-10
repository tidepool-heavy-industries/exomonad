{-# LANGUAGE UndecidableInstances #-}

-- | Compile-time validation for ForkNode/BarrierNode patterns.
--
-- This module validates that parallel fan-out/fan-in patterns are correctly
-- wired at compile time:
--
-- 1. ForkNode's Spawn targets reference existing worker fields
-- 2. Each spawned worker has Arrive effect pointing to the correct barrier
-- 3. BarrierNode's Awaits sources match what workers send via Arrive
--
-- = Usage Pattern
--
-- @
-- data MyGraph mode = MyGraph
--   { fork    :: mode :- ForkNode :@ Spawn '[To "w1" A, To "w2" B] :@ Barrier "merge"
--   , w1      :: mode :- LLMNode :@ Input A :@ Schema R1 :@ UsesEffects '[Arrive "merge" R1]
--   , w2      :: mode :- LLMNode :@ Input B :@ Schema R2 :@ UsesEffects '[Arrive "merge" R2]
--   , merge   :: mode :- BarrierNode :@ Awaits '[From "w1" R1, From "w2" R2] :@ ...
--   , ...
--   }
-- @
--
-- = Validation Rules
--
-- 1. __Spawn Target Exists__: Each @To "worker" Payload@ in Spawn must reference
--    an existing field in the graph.
--
-- 2. __Worker Arrives at Barrier__: Each spawned worker must have @Arrive barrierName@
--    in its UsesEffects, where @barrierName@ matches the ForkNode's Barrier annotation.
--
-- 3. __Awaits Match Arrives__: The BarrierNode's Awaits sources must match the
--    (source, type) pairs that workers send via Arrive.
module Tidepool.Graph.Validate.ForkBarrier
  ( -- * Main Validation Constraints
    ValidateForkBarrierPairs

    -- * Error Messages
  , SpawnTargetMissingError
  , WorkerMissingArriveError
  , ArriveBarrierMismatchError
  , AwaitsMismatchError

    -- * Internal Type Families (exported for testing)
  , CollectForkNodes
  , CollectBarrierNodes
  , GetSpawnTargetNames
  , GetWorkerArriveBarrier
  , ValidateSpawnTargetsExist
  , ValidateWorkersArrive
  ) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..))
import GHC.Generics (Generic(..), K1(..), M1(..), (:*:)(..), Meta(..), S, D, C, Rep)

import Tidepool.Graph.Types (type (:@), UsesEffects, Arrive)
import Tidepool.Graph.Goto (To)
import Tidepool.Graph.Generic.Core (AsGraph, ForkNode, BarrierNode)
import Tidepool.Graph.Edges (GetSpawnTargets, GetBarrierTarget)

-- ════════════════════════════════════════════════════════════════════════════
-- MAIN VALIDATION CONSTRAINT
-- ════════════════════════════════════════════════════════════════════════════

-- | Bundle validation for all ForkNode/BarrierNode pairs in a graph.
--
-- For each ForkNode:
-- 1. All Spawn targets must be valid field names
-- 2. Each worker must have Arrive pointing to the barrier named in the ForkNode
-- 3. The barrier's Awaits must match what workers send
--
-- This constraint is added to 'ValidGraphRecord' in Generic.hs.
type ValidateForkBarrierPairs :: (Type -> Type) -> Constraint
type family ValidateForkBarrierPairs graph where
  ValidateForkBarrierPairs graph =
    ValidateAllForkNodes
      (CollectForkNodes (FieldsWithNamesOf graph))
      (FieldsWithNamesOf graph)


-- ════════════════════════════════════════════════════════════════════════════
-- FORK NODE COLLECTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Collect all ForkNode fields from a graph.
--
-- Returns list of (fieldName, nodeDef) pairs for ForkNode fields.
type CollectForkNodes :: [(Symbol, Type)] -> [(Symbol, Type)]
type family CollectForkNodes fields where
  CollectForkNodes '[] = '[]
  CollectForkNodes ('(name, def) ': rest) =
    If (IsForkDef def)
       ('(name, def) ': CollectForkNodes rest)
       (CollectForkNodes rest)

-- | Check if a definition is a ForkNode.
type IsForkDef :: Type -> Bool
type family IsForkDef def where
  IsForkDef (node :@ _) = IsForkDef node
  IsForkDef ForkNode = 'True
  IsForkDef _ = 'False

-- | Collect all BarrierNode fields from a graph.
type CollectBarrierNodes :: [(Symbol, Type)] -> [(Symbol, Type)]
type family CollectBarrierNodes fields where
  CollectBarrierNodes '[] = '[]
  CollectBarrierNodes ('(name, def) ': rest) =
    If (IsBarrierDef def)
       ('(name, def) ': CollectBarrierNodes rest)
       (CollectBarrierNodes rest)

-- | Check if a definition is a BarrierNode.
type IsBarrierDef :: Type -> Bool
type family IsBarrierDef def where
  IsBarrierDef (node :@ _) = IsBarrierDef node
  IsBarrierDef BarrierNode = 'True
  IsBarrierDef _ = 'False


-- ════════════════════════════════════════════════════════════════════════════
-- FORK NODE VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validate all ForkNodes in the graph.
type ValidateAllForkNodes :: [(Symbol, Type)] -> [(Symbol, Type)] -> Constraint
type family ValidateAllForkNodes forkNodes allFields where
  ValidateAllForkNodes '[] _ = ()
  ValidateAllForkNodes ('(forkName, forkDef) ': rest) allFields =
    ( ValidateSingleForkNode forkName forkDef allFields
    , ValidateAllForkNodes rest allFields
    )

-- | Validate a single ForkNode.
type ValidateSingleForkNode :: Symbol -> Type -> [(Symbol, Type)] -> Constraint
type family ValidateSingleForkNode forkName forkDef allFields where
  ValidateSingleForkNode forkName forkDef allFields =
    ( -- 1. All Spawn targets must exist as fields
      ValidateSpawnTargetsExist
        forkName
        (GetSpawnTargets forkDef)
        (FieldNamesFromPairs allFields)
    , -- 2. Each worker must have Arrive to the barrier
      ValidateWorkersArrive
        forkName
        (GetBarrierTargetOrError forkName forkDef)
        (GetSpawnTargets forkDef)
        allFields
    )

-- | Get barrier target or produce error.
type GetBarrierTargetOrError :: Symbol -> Type -> Symbol
type family GetBarrierTargetOrError forkName def where
  GetBarrierTargetOrError forkName def =
    FromJustBarrier forkName (GetBarrierTarget def)

type FromJustBarrier :: Symbol -> Maybe Symbol -> Symbol
type family FromJustBarrier forkName m where
  FromJustBarrier _ ('Just x) = x
  FromJustBarrier forkName 'Nothing = TypeError
    ('Text "ForkNode '" ':<>: 'Text forkName ':<>: 'Text "' missing Barrier annotation"
     ':$$: 'Text ""
     ':$$: 'Text "ForkNode must specify which BarrierNode collects the results:"
     ':$$: 'Text "  ForkNode :@ Spawn '[...] :@ Barrier \"mergeNode\""
    )


-- ════════════════════════════════════════════════════════════════════════════
-- SPAWN TARGET VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validate that all Spawn targets are valid field names.
type ValidateSpawnTargetsExist :: Symbol -> [Type] -> [Symbol] -> Constraint
type family ValidateSpawnTargetsExist forkName targets fieldNames where
  ValidateSpawnTargetsExist _ '[] _ = ()
  ValidateSpawnTargetsExist forkName (To (name :: Symbol) _ ': rest) fieldNames =
    ( If (ElemSymbol name fieldNames)
         (() :: Constraint)
         (SpawnTargetMissingError forkName name fieldNames)
    , ValidateSpawnTargetsExist forkName rest fieldNames
    )
  -- Skip non-To entries (shouldn't happen in well-formed Spawn)
  ValidateSpawnTargetsExist forkName (_ ': rest) fieldNames =
    ValidateSpawnTargetsExist forkName rest fieldNames

-- | Extract worker names from Spawn targets.
type GetSpawnTargetNames :: [Type] -> [Symbol]
type family GetSpawnTargetNames targets where
  GetSpawnTargetNames '[] = '[]
  GetSpawnTargetNames (To (name :: Symbol) _ ': rest) = name ': GetSpawnTargetNames rest
  GetSpawnTargetNames (_ ': rest) = GetSpawnTargetNames rest


-- ════════════════════════════════════════════════════════════════════════════
-- WORKER ARRIVE VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Validate that each worker has Arrive pointing to the correct barrier.
type ValidateWorkersArrive :: Symbol -> Symbol -> [Type] -> [(Symbol, Type)] -> Constraint
type family ValidateWorkersArrive forkName barrierName targets allFields where
  ValidateWorkersArrive _ _ '[] _ = ()
  ValidateWorkersArrive forkName barrierName (To (workerName :: Symbol) _ ': rest) allFields =
    ( ValidateSingleWorkerArrives forkName barrierName workerName allFields
    , ValidateWorkersArrive forkName barrierName rest allFields
    )
  ValidateWorkersArrive forkName barrierName (_ ': rest) allFields =
    ValidateWorkersArrive forkName barrierName rest allFields

-- | Validate a single worker has Arrive to the correct barrier.
type ValidateSingleWorkerArrives :: Symbol -> Symbol -> Symbol -> [(Symbol, Type)] -> Constraint
type family ValidateSingleWorkerArrives forkName barrierName workerName allFields where
  ValidateSingleWorkerArrives forkName barrierName workerName allFields =
    CheckWorkerArriveBarrier
      forkName
      barrierName
      workerName
      (GetWorkerArriveBarrier (LookupFieldDef allFields workerName))

-- | Get the barrier name from a worker's Arrive effect.
type GetWorkerArriveBarrier :: Type -> Maybe Symbol
type family GetWorkerArriveBarrier def where
  GetWorkerArriveBarrier def =
    GetArriveBarrierFromMaybeEffects (GetUsesEffectsFixed def)

-- | Extract Arrive barrier from Maybe effect list.
type GetArriveBarrierFromMaybeEffects :: Maybe [Effect] -> Maybe Symbol
type family GetArriveBarrierFromMaybeEffects mEffs where
  GetArriveBarrierFromMaybeEffects 'Nothing = 'Nothing
  GetArriveBarrierFromMaybeEffects ('Just effs) = GetArriveBarrierFromEffects effs

-- | Extract Arrive barrier from effect list.
type GetArriveBarrierFromEffects :: [Effect] -> Maybe Symbol
type family GetArriveBarrierFromEffects effs where
  GetArriveBarrierFromEffects '[] = 'Nothing
  GetArriveBarrierFromEffects (Arrive (barrierName :: Symbol) _ ': _) = 'Just barrierName
  GetArriveBarrierFromEffects (_ ': rest) = GetArriveBarrierFromEffects rest

-- | Check that worker's Arrive barrier matches expected.
type CheckWorkerArriveBarrier :: Symbol -> Symbol -> Symbol -> Maybe Symbol -> Constraint
type family CheckWorkerArriveBarrier forkName expectedBarrier workerName actualBarrier where
  CheckWorkerArriveBarrier _ expectedBarrier _ ('Just expectedBarrier) = ()
  CheckWorkerArriveBarrier forkName expectedBarrier workerName 'Nothing =
    WorkerMissingArriveError forkName expectedBarrier workerName
  CheckWorkerArriveBarrier forkName expectedBarrier workerName ('Just actualBarrier) =
    ArriveBarrierMismatchError forkName expectedBarrier workerName actualBarrier


-- ════════════════════════════════════════════════════════════════════════════
-- ERROR MESSAGES
-- ════════════════════════════════════════════════════════════════════════════

-- | Error when a Spawn target doesn't exist.
type SpawnTargetMissingError :: Symbol -> Symbol -> [Symbol] -> Constraint
type SpawnTargetMissingError forkName target fieldNames = TypeError
  ('Text "═══════════════════════════════════════════════════════════════════"
   ':$$: 'Text "  ForkNode spawn target doesn't exist"
   ':$$: 'Text "═══════════════════════════════════════════════════════════════════"
   ':$$: 'Text ""
   ':$$: 'Text "WHAT HAPPENED"
   ':$$: 'Text "  ForkNode '" ':<>: 'Text forkName ':<>: 'Text "' spawns: To \"" ':<>: 'Text target ':<>: 'Text "\" ..."
   ':$$: 'Text "  But there's no field named \"" ':<>: 'Text target ':<>: 'Text "\" in your graph."
   ':$$: 'Text ""
   ':$$: 'Text "HOW TO FIX"
   ':$$: 'Text "  • Check spelling: is the worker field name correct?"
   ':$$: 'Text "  • Add the missing worker node to your graph"
   ':$$: 'Text "  • Remove this spawn target if it was a mistake"
  )

-- | Error when a worker is missing Arrive effect.
type WorkerMissingArriveError :: Symbol -> Symbol -> Symbol -> Constraint
type WorkerMissingArriveError forkName barrierName workerName = TypeError
  ('Text "═══════════════════════════════════════════════════════════════════"
   ':$$: 'Text "  Worker node missing Arrive effect"
   ':$$: 'Text "═══════════════════════════════════════════════════════════════════"
   ':$$: 'Text ""
   ':$$: 'Text "WHAT HAPPENED"
   ':$$: 'Text "  ForkNode '" ':<>: 'Text forkName ':<>: 'Text "' spawns worker '" ':<>: 'Text workerName ':<>: 'Text "'"
   ':$$: 'Text "  with Barrier \"" ':<>: 'Text barrierName ':<>: 'Text "\""
   ':$$: 'Text ""
   ':$$: 'Text "  But worker '" ':<>: 'Text workerName ':<>: 'Text "' has no Arrive effect!"
   ':$$: 'Text "  The barrier will wait forever for this worker."
   ':$$: 'Text ""
   ':$$: 'Text "HOW TO FIX"
   ':$$: 'Text "  Add Arrive to the worker's UsesEffects:"
   ':$$: 'Text ""
   ':$$: 'Text "  " ':<>: 'Text workerName ':<>: 'Text " :: mode :- LLMNode"
   ':$$: 'Text "      :@ Input ..."
   ':$$: 'Text "      :@ Schema Result"
   ':$$: 'Text "      :@ UsesEffects '[Arrive \"" ':<>: 'Text barrierName ':<>: 'Text "\" Result]"
  )

-- | Error when worker's Arrive points to wrong barrier.
type ArriveBarrierMismatchError :: Symbol -> Symbol -> Symbol -> Symbol -> Constraint
type ArriveBarrierMismatchError forkName expectedBarrier workerName actualBarrier = TypeError
  ('Text "═══════════════════════════════════════════════════════════════════"
   ':$$: 'Text "  Worker Arrive points to wrong barrier"
   ':$$: 'Text "═══════════════════════════════════════════════════════════════════"
   ':$$: 'Text ""
   ':$$: 'Text "WHAT HAPPENED"
   ':$$: 'Text "  ForkNode '" ':<>: 'Text forkName ':<>: 'Text "' uses Barrier \"" ':<>: 'Text expectedBarrier ':<>: 'Text "\""
   ':$$: 'Text "  But worker '" ':<>: 'Text workerName ':<>: 'Text "' has Arrive \"" ':<>: 'Text actualBarrier ':<>: 'Text "\""
   ':$$: 'Text ""
   ':$$: 'Text "  Worker results will go to the wrong barrier!"
   ':$$: 'Text ""
   ':$$: 'Text "HOW TO FIX"
   ':$$: 'Text "  Change the worker's Arrive to match the ForkNode's Barrier:"
   ':$$: 'Text ""
   ':$$: 'Text "  UsesEffects '[Arrive \"" ':<>: 'Text expectedBarrier ':<>: 'Text "\" Result]"
  )

-- | Error when BarrierNode Awaits doesn't match worker Arrives.
type AwaitsMismatchError :: Symbol -> Symbol -> Symbol -> Type -> Constraint
type AwaitsMismatchError barrierName workerName expectedType actualType = TypeError
  ('Text "═══════════════════════════════════════════════════════════════════"
   ':$$: 'Text "  BarrierNode Awaits type mismatch"
   ':$$: 'Text "═══════════════════════════════════════════════════════════════════"
   ':$$: 'Text ""
   ':$$: 'Text "WHAT HAPPENED"
   ':$$: 'Text "  BarrierNode '" ':<>: 'Text barrierName ':<>: 'Text "' expects from '" ':<>: 'Text workerName ':<>: 'Text "':"
   ':$$: 'Text "    " ':<>: 'ShowType expectedType
   ':$$: 'Text ""
   ':$$: 'Text "  But the worker sends:"
   ':$$: 'Text "    " ':<>: 'ShowType actualType
   ':$$: 'Text ""
   ':$$: 'Text "HOW TO FIX"
   ':$$: 'Text "  • Update the worker's Arrive type to match Awaits"
   ':$$: 'Text "  • Or update the BarrierNode's Awaits to match worker output"
  )


-- ════════════════════════════════════════════════════════════════════════════
-- TYPE-LEVEL UTILITIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect kind alias.
type Effect = Type -> Type

-- | Get UsesEffects with fixed kind.
type GetUsesEffectsFixed :: Type -> Maybe [Effect]
type family GetUsesEffectsFixed def where
  GetUsesEffectsFixed (_ :@ UsesEffects effs) = 'Just effs
  GetUsesEffectsFixed (node :@ _) = GetUsesEffectsFixed node
  GetUsesEffectsFixed _ = 'Nothing

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

-- | Get fields with names from a graph type.
type FieldsWithNamesOf :: (Type -> Type) -> [(Symbol, Type)]
type FieldsWithNamesOf graph = FieldsWithNames (Rep (graph AsGraph))

-- | Extract just field names from (Symbol, Type) pairs.
type FieldNamesFromPairs :: [(Symbol, Type)] -> [Symbol]
type family FieldNamesFromPairs pairs where
  FieldNamesFromPairs '[] = '[]
  FieldNamesFromPairs ('(name, _) ': rest) = name ': FieldNamesFromPairs rest

-- | Lookup field definition by name.
type LookupFieldDef :: [(Symbol, Type)] -> Symbol -> Type
type family LookupFieldDef fields name where
  LookupFieldDef '[] name = TypeError ('Text "Internal error: field not found: " ':<>: 'Text name)
  LookupFieldDef ('(name, def) ': _) name = def
  LookupFieldDef (_ ': rest) name = LookupFieldDef rest name

-- | Type-level If.
type If :: Bool -> k -> k -> k
type family If cond t f where
  If 'True  t _ = t
  If 'False _ f = f

-- | Symbol membership check.
type ElemSymbol :: Symbol -> [Symbol] -> Bool
type family ElemSymbol x xs where
  ElemSymbol _ '[] = 'False
  ElemSymbol x (x ': _) = 'True
  ElemSymbol x (_ ': rest) = ElemSymbol x rest

-- | Append type-level lists.
type Append :: [k] -> [k] -> [k]
type family Append xs ys where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys
