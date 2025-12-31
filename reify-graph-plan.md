# Plan: Implement ReifyGraph for Record-Based Graphs

## Goal
Implement automatic `ReifyGraph` derivation for record-based graphs so that:
```haskell
-- Instead of manually constructing GraphInfo:
diagram :: Text
diagram = toMermaid (reifyGraph @(SupportGraph AsGraph))
```

## Success Criteria
Replace the manually constructed `GraphInfo` in the Mermaid test with a `reifyGraph`-derived one.

---

## Background

### What Already Exists

1. **GraphInfo/NodeInfo/EdgeInfo types** in `Reify.hs` (lines 72-115)
2. **Reification helpers** in `Reify.hs`:
   - `ReifyTypeList` - converts `[Type]` to `[TypeRep]`
   - `ReifyMaybeType` - converts `Maybe Type` to `Maybe TypeRep`
   - `ReifyGotoTargets` - converts `[(Symbol, Type)]` to `[(Text, TypeRep)]`
   - `ReifyNodeKind` - converts `NodeKind` to `RuntimeNodeKind`
   - `ReifyBool` - converts `Bool` to runtime `Bool`

3. **Field extraction** in `Generic.hs` (lines 393-445):
   - `FieldNamesOf graph` - extracts `[Symbol]` of field names
   - `FieldsWithNamesOf graph` - extracts `[(Symbol, Type)]` pairs

4. **Annotation extraction** in `Edges.hs` (lines 63-187):
   - `GetNeeds`, `GetSchema`, `GetUsesEffects`, `GetTemplate`, `GetSystem`
   - `GetVision`, `GetTools`, `GetMemory`
   - `GetGotoTargets`, `HasGotoExit`

5. **Entry/Exit extraction** in `Generic.hs` (lines 503-528):
   - `GetEntryType graph` - returns `Maybe Type`
   - `GetExitType graph` - returns `Maybe Type`

### What's Missing

A typeclass instance that:
1. Traverses record fields
2. Builds `NodeInfo` for each non-Entry/Exit field
3. Derives edges from annotations
4. Returns complete `GraphInfo`

---

## Implementation Plan

### Step 1: Create ReifyField Typeclass

**File:** `src/Tidepool/Graph/Reify.hs`

Add a typeclass to reify a single field definition to `NodeInfo`:

```haskell
-- | Reify a single node field to runtime NodeInfo
class ReifyField (name :: Symbol) (def :: Type) where
  reifyField :: Proxy name -> Proxy def -> Maybe NodeInfo

-- Entry/Exit are not nodes, return Nothing
instance ReifyField name (Entry a) where
  reifyField _ _ = Nothing

instance ReifyField name (Exit a) where
  reifyField _ _ = Nothing

-- LLMNode with annotations
instance ( KnownSymbol name
         , ReifyTypeList (GetNeeds def)
         , ReifyMaybeType (GetSchema def)
         , ReifyMaybeType (GetTemplate def)
         , ReifyMaybeType (GetSystem def)
         , ReifyMaybeType (GetMemory def)
         , ReifyBool (GetVision def)
         , ReifyTypeList (GetTools def)
         ) => ReifyField name (LLMNode :@ rest) where
  reifyField pName pDef = Just NodeInfo
    { niName = T.pack (symbolVal pName)
    , niKind = RuntimeLLM
    , niNeeds = reifyTypeList (Proxy @(GetNeeds def))
    , niSchema = reifyMaybeType (Proxy @(GetSchema def))
    , niGotoTargets = []
    , niHasGotoExit = False
    , niHasVision = reifyBool (Proxy @(GetVision def))
    , niTools = reifyTypeList (Proxy @(GetTools def))
    , niToolInfos = []  -- TODO: full tool reification
    , niSystem = reifyMaybeType (Proxy @(GetSystem def))
    , niTemplate = reifyMaybeType (Proxy @(GetTemplate def))
    , niMemory = reifyMaybeType (Proxy @(GetMemory def))
    }

-- LogicNode with annotations
instance ( KnownSymbol name
         , ReifyTypeList (GetNeeds def)
         , ReifyGotoTargetsFromEffects (GetUsesEffects def)
         , ReifyHasGotoExit (GetUsesEffects def)
         , ReifyMaybeType (GetMemory def)
         ) => ReifyField name (LogicNode :@ rest) where
  reifyField pName pDef = Just NodeInfo
    { niName = T.pack (symbolVal pName)
    , niKind = RuntimeLogic
    , niNeeds = reifyTypeList (Proxy @(GetNeeds def))
    , niSchema = Nothing
    , niGotoTargets = reifyGotoTargetsFromEffects (Proxy @(GetUsesEffects def))
    , niHasGotoExit = reifyHasGotoExit (Proxy @(GetUsesEffects def))
    , niHasVision = False
    , niTools = []
    , niToolInfos = []
    , niSystem = Nothing
    , niTemplate = Nothing
    , niMemory = reifyMaybeType (Proxy @(GetMemory def))
    }
```

### Step 2: Create ReifyFields Typeclass

Traverse a list of `(Symbol, Type)` pairs to build `[NodeInfo]`:

```haskell
-- | Reify all fields to NodeInfo list
class ReifyFields (fields :: [(Symbol, Type)]) where
  reifyFields :: Proxy fields -> [NodeInfo]

instance ReifyFields '[] where
  reifyFields _ = []

instance (ReifyField name def, ReifyFields rest)
      => ReifyFields ('(name, def) ': rest) where
  reifyFields _ =
    case reifyField (Proxy @name) (Proxy @def) of
      Nothing -> reifyFields (Proxy @rest)  -- Skip Entry/Exit
      Just info -> info : reifyFields (Proxy @rest)
```

### Step 3: Create Edge Derivation

Derive edges from the type-level information:

```haskell
-- | Derive edges from fields
-- Two kinds of edges:
-- 1. Implicit: Schema output -> nodes that Need it
-- 2. Explicit: Goto transitions
deriveEdges :: [(Symbol, Type)] -> Type -> [EdgeInfo]
-- This needs to be a type family + reification class

class DeriveEdges (fields :: [(Symbol, Type)]) (entryType :: Type) where
  deriveEdges :: Proxy fields -> Proxy entryType -> [EdgeInfo]
```

**Edge derivation logic:**
1. Entry -> nodes whose Needs include entryType
2. For each node with Schema S -> nodes whose Needs include S
3. For each Logic node -> its Goto targets (explicit edges)
4. Nodes with Goto Exit or Schema matching exitType -> Exit

### Step 4: Implement ReifyGraph Instance

```haskell
instance ( Generic (graph AsGraph)
         , ReifyMaybeType (GetEntryType graph)
         , ReifyMaybeType (GetExitType graph)
         , ReifyFields (FieldsWithNamesOf graph)
         , DeriveEdges (FieldsWithNamesOf graph) (FromJust (GetEntryType graph))
         ) => ReifyGraph (graph AsGraph) where
  reifyGraph = GraphInfo
    { giEntryType = reifyMaybeType (Proxy @(GetEntryType graph))
    , giExitType = reifyMaybeType (Proxy @(GetExitType graph))
    , giNodes = reifyFields (Proxy @(FieldsWithNamesOf graph))
    , giEdges = deriveEdges (Proxy @(FieldsWithNamesOf graph))
                            (Proxy @(FromJust (GetEntryType graph)))
    , giGroups = []  -- TODO: extract from graph-level annotations
    }
```

### Step 5: Add Helper Type Families

Need a few additional type families:

```haskell
-- Extract Goto targets from Maybe effect list
type ReifyGotoTargetsFromEffects :: Maybe [k] -> Constraint
class ReifyGotoTargetsFromEffects (mEffs :: Maybe [k]) where
  reifyGotoTargetsFromEffects :: Proxy mEffs -> [(Text, TypeRep)]

instance ReifyGotoTargetsFromEffects 'Nothing where
  reifyGotoTargetsFromEffects _ = []

instance ReifyGotoTargets (GetGotoTargets effs)
      => ReifyGotoTargetsFromEffects ('Just effs) where
  reifyGotoTargetsFromEffects _ = reifyGotoTargets (Proxy @(GetGotoTargets effs))

-- Check HasGotoExit from Maybe effect list
class ReifyHasGotoExit (mEffs :: Maybe [k]) where
  reifyHasGotoExit :: Proxy mEffs -> Bool

instance ReifyHasGotoExit 'Nothing where
  reifyHasGotoExit _ = False

instance ReifyBool (HasGotoExit effs) => ReifyHasGotoExit ('Just effs) where
  reifyHasGotoExit _ = reifyBool (Proxy @(HasGotoExit effs))
```

---

## Files to Modify

| File | Changes |
|------|---------|
| `src/Tidepool/Graph/Reify.hs` | Add ReifyField, ReifyFields, edge derivation, ReifyGraph instance |
| `src/Tidepool/Graph/Generic.hs` | Export additional type families if needed |
| `src/Tidepool/Graph/Generic/Core.hs` | May need to export LLMNode/LogicNode for pattern matching |
| `src/Tidepool/Graph.hs` | Re-export reifyGraph if not already |

---

## Testing

1. Create test file that imports `SupportGraph` from `Example.hs`
2. Call `reifyGraph @(SupportGraph AsGraph)`
3. Pass result to `toMermaid`
4. Verify output matches expected Mermaid diagram

```haskell
-- Test: Replace manual GraphInfo with derived one
testMermaid :: IO ()
testMermaid = do
  let graphInfo = reifyGraph @(SupportGraph AsGraph)
  TIO.putStrLn (toMermaid graphInfo)
```

---

## Complexity Notes

### Edge Derivation is the Trickiest Part

Need to determine:
1. Which nodes are connected by implicit data flow (Schema -> Needs)
2. Which nodes are connected by explicit Goto transitions
3. Entry connections (entryType satisfies which Needs)
4. Exit connections (which nodes produce exitType or have Goto Exit)

This requires:
- Collecting all Schema outputs with their source nodes
- For each node, checking if its Needs are satisfied by Entry or a Schema
- For Logic nodes, extracting Goto targets

### Alternative: Simpler Edge Derivation

Could simplify by only deriving explicit edges (Goto) and letting the user manually specify implicit edges, or derive them at runtime by matching TypeReps.

---

## Estimated Scope

- **Core reification (Steps 1-4):** ~150-200 lines
- **Edge derivation (Step 3):** ~100-150 lines (most complex)
- **Helper type families (Step 5):** ~50 lines
- **Tests:** ~30 lines

Total: ~350-400 lines of new code in Reify.hs
