# Plan 03: Validation Type Families

## Scope

Add compile-time validation that Goto targets reference valid entry points.

## Files Owned

- `haskell/dsl/core/src/Tidepool/Graph/Validate/RecordStructure.hs`
- `haskell/dsl/core/src/Tidepool/Graph/Validate/EntryPoints.hs` (new)

## Dependencies

- **01-core-types**: Need `GetEntries`, `LookupEntry`, `EntryNames`

## Implementation

### 1. New Validation: Entry Point Exists (EntryPoints.hs)

```haskell
module Tidepool.Graph.Validate.EntryPoints where

import GHC.TypeLits (TypeError, ErrorMessage(..))

-- Validate that a goto target's entry point exists on the target node
type ValidateEntryPoint :: Symbol -> Symbol -> [(Symbol, Type)] -> Constraint
type family ValidateEntryPoint nodeName entryName nodeEntries where
  ValidateEntryPoint node entry entries =
    IfMaybe (LookupEntry entry entries)
      (() :: Constraint)
      (TypeError
        ('Text "Goto to node '" ':<>: 'Text node ':<>: 'Text "' references entry point '"
         ':<>: 'Text entry ':<>: 'Text "' which does not exist."
         ':$$: 'Text "Available entry points: " ':<>: 'ShowType (EntryNames entries)))

-- Validate all gotos in an effects list
type ValidateGotoEntries :: [(Symbol, Type)] -> [Type] -> Constraint
type family ValidateGotoEntries graphFields effs where
  ValidateGotoEntries _ '[] = ()
  ValidateGotoEntries fields (GotoEntry node entry _ ': rest) =
    ( ValidateNodeExists node fields
    , ValidateEntryPointOnNode node entry fields
    , ValidateGotoEntries fields rest
    )
  ValidateGotoEntries fields (Goto Exit _ ': rest) =
    ValidateGotoEntries fields rest
  ValidateGotoEntries fields (_ ': rest) =
    ValidateGotoEntries fields rest

-- Helper: find node's entries from graph fields
type ValidateEntryPointOnNode :: Symbol -> Symbol -> [(Symbol, Type)] -> Constraint
type family ValidateEntryPointOnNode nodeName entryName fields where
  ValidateEntryPointOnNode node entry fields =
    ValidateEntryPoint node entry (NodeEntriesFromFields node fields)

-- Extract entries for a named node from graph fields
type NodeEntriesFromFields :: Symbol -> [(Symbol, Type)] -> [(Symbol, Type)]
type family NodeEntriesFromFields nodeName fields where
  NodeEntriesFromFields _ '[] = '[]
  NodeEntriesFromFields name ('(name, nodeDef) ': _) = NodeEntryInfo nodeDef
  NodeEntriesFromFields name (_ ': rest) = NodeEntriesFromFields name rest
```

### 2. Update ValidGraphRecord (RecordStructure.hs)

```haskell
-- Add entry point validation to bundle
type ValidGraphRecord :: (Type -> Type) -> Constraint
type ValidGraphRecord graph =
  ( Generic (graph AsGraph)
  , RequireGeneric graph
  , ValidateEntryExit graph
  , ValidateGotoTargets graph          -- existing: node exists
  , ValidateGotoEntryPoints graph      -- NEW: entry point exists
  , AllFieldsReachable graph
  , AllLogicFieldsReachExit graph
  , NoDeadGotosRecord graph
  , AllLogicNodesHaveGoto graph
  , NoGotoSelfOnly graph
  , ValidateForkBarrierPairs graph
  )

-- New validation that iterates all nodes and checks their gotos
type ValidateGotoEntryPoints :: (Type -> Type) -> Constraint
type family ValidateGotoEntryPoints graph where
  ValidateGotoEntryPoints graph =
    ValidateAllGotoEntries (FieldsWithNamesOf graph) (FieldsWithNamesOf graph)

type ValidateAllGotoEntries :: [(Symbol, Type)] -> [(Symbol, Type)] -> Constraint
type family ValidateAllGotoEntries allFields nodesToCheck where
  ValidateAllGotoEntries _ '[] = ()
  ValidateAllGotoEntries allFields ('(_, nodeDef) ': rest) =
    ( ValidateNodeGotos allFields nodeDef
    , ValidateAllGotoEntries allFields rest
    )

-- Extract gotos from a node and validate each
type ValidateNodeGotos :: [(Symbol, Type)] -> Type -> Constraint
type family ValidateNodeGotos graphFields nodeDef where
  ValidateNodeGotos fields nodeDef =
    ValidateGotoEntries fields (FromMaybe '[] (GetUsesEffects nodeDef))
```

### 3. Better Error Messages

```haskell
-- When entry point doesn't exist
type EntryPointError :: Symbol -> Symbol -> [Symbol] -> ErrorMessage
type family EntryPointError node entry available where
  EntryPointError node entry available =
    'Text "Invalid goto: node '" ':<>: 'Text node ':<>: 'Text "' has no entry point '"
    ':<>: 'Text entry ':<>: 'Text "'"
    ':$$: 'Text "Available entry points on '" ':<>: 'Text node ':<>: 'Text "': "
    ':<>: 'ShowType available
    ':$$: 'Text "Hint: Check spelling or add the entry point to the target node's Entries annotation"

-- When node has no entries at all
type NoEntriesError :: Symbol -> ErrorMessage
type family NoEntriesError node where
  NoEntriesError node =
    'Text "Invalid goto: node '" ':<>: 'Text node ':<>: 'Text "' has no Entries annotation"
    ':$$: 'Text "Add ':@ Entries '[\"name\" ::: PayloadType]' to the node definition"
```

### 4. LLM Node Entry Requirement

```haskell
-- LLM nodes MUST have Entries and Exits
type ValidateLLMNodeAnnotations :: Symbol -> Type -> Constraint
type family ValidateLLMNodeAnnotations fieldName nodeDef where
  ValidateLLMNodeAnnotations name (LLMNode :@ rest) =
    ( RequireEntries name (LLMNode :@ rest)
    , RequireExits name (LLMNode :@ rest)
    , RequireTemplate name (LLMNode :@ rest)
    )
  ValidateLLMNodeAnnotations _ _ = ()

type RequireEntries :: Symbol -> Type -> Constraint
type family RequireEntries name node where
  RequireEntries name node =
    IfMaybe (GetEntries node)
      (() :: Constraint)
      (TypeError ('Text "LLM node '" ':<>: 'Text name ':<>: 'Text "' requires Entries annotation"))

type RequireExits :: Symbol -> Type -> Constraint
type family RequireExits name node where
  RequireExits name node =
    IfMaybe (GetExits node)
      (() :: Constraint)
      (TypeError ('Text "LLM node '" ':<>: 'Text name ':<>: 'Text "' requires Exits annotation"))
```

## Tests

Create `test/Graph/Validate/EntryPointsSpec.hs`:

```haskell
-- Should compile: valid entry point
data ValidGraph mode = ValidGraph
  { entry :: mode :- Entry Int
  , work  :: mode :- LLMNode :@ Entries '["start" ::: Int] :@ ...
  , route :: mode :- LogicNode :@ UsesEffects '[GotoEntry "work" "start" Int]
  }
-- This should compile successfully

-- Should NOT compile: invalid entry point
data InvalidGraph mode = InvalidGraph
  { entry :: mode :- Entry Int
  , work  :: mode :- LLMNode :@ Entries '["start" ::: Int] :@ ...
  , route :: mode :- LogicNode :@ UsesEffects '[GotoEntry "work" "wrongName" Int]
  }
-- This should fail with: "node 'work' has no entry point 'wrongName'"
```

## PR Criteria

- [ ] `ValidateGotoEntryPoints` constraint added to `ValidGraphRecord`
- [ ] Clear error messages for missing entry points
- [ ] LLM nodes require Entries/Exits annotations
- [ ] Compile-time tests (both passing and failing cases)
- [ ] No runtime changes

## Branch

`refactor/record-nodes-03-validation`
