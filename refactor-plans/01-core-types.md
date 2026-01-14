# Plan 01: Core DSL Types & Extraction

## Scope

Define new annotation types and type families for record-based entries/exits.

## Files Owned

- `haskell/dsl/core/src/Tidepool/Graph/Types.hs`
- `haskell/dsl/core/src/Tidepool/Graph/Edges.hs`

## Dependencies

None - this is foundational.

## Implementation

### 1. New Annotation Types (Types.hs)

```haskell
-- Named entry/exit field (type-level)
type (:::) :: Symbol -> Type -> (Symbol, Type)
type name ::: payload = '(name, payload)

-- Multiple entry points annotation
-- Each entry handler: payload -> Eff es TemplateCtx
data Entries (entries :: [(Symbol, Type)])

-- Multiple exit points annotation
-- Each exit IS a tool, handler: payload -> Eff es (GotoChoice targets)
data Exits (exits :: [(Symbol, Type)])

-- Associates exit with its routing targets
data ExitRoutes (routes :: [Type])
```

Example usage:
```haskell
gWork :: mode :- LLMNode
    :@ Entries '[ "fresh" ::: TaskSpec, "retry" ::: RetryInfo ]
    :@ Template WorkTpl
    :@ Exits '[ "complete" ::: CompletePayload, "blocked" ::: BlockedPayload ]
```

### 2. Extraction Type Families (Edges.hs)

```haskell
-- Extract Entries annotation
type GetEntries :: Type -> Maybe [(Symbol, Type)]
type family GetEntries node where
  GetEntries (node :@ Entries entries) = 'Just entries
  GetEntries (node :@ _) = GetEntries node
  GetEntries _ = 'Nothing

-- Extract Exits annotation
type GetExits :: Type -> Maybe [(Symbol, Type)]
type family GetExits node where
  GetExits (node :@ Exits exits) = 'Just exits
  GetExits (node :@ _) = GetExits node
  GetExits _ = 'Nothing

-- Extract entry names only
type EntryNames :: [(Symbol, Type)] -> [Symbol]
type family EntryNames entries where
  EntryNames '[] = '[]
  EntryNames ('(name, _) ': rest) = name ': EntryNames rest

-- Extract exit names only
type ExitNames :: [(Symbol, Type)] -> [Symbol]
type family ExitNames exits where
  ExitNames '[] = '[]
  ExitNames ('(name, _) ': rest) = name ': ExitNames rest

-- Lookup entry payload type by name
type LookupEntry :: Symbol -> [(Symbol, Type)] -> Maybe Type
type family LookupEntry name entries where
  LookupEntry _ '[] = 'Nothing
  LookupEntry name ('(name, payload) ': _) = 'Just payload
  LookupEntry name (_ ': rest) = LookupEntry name rest

-- Lookup exit payload type by name
type LookupExit :: Symbol -> [(Symbol, Type)] -> Maybe Type
type family LookupExit name exits where
  LookupExit _ '[] = 'Nothing
  LookupExit name ('(name, payload) ': _) = 'Just payload
  LookupExit name (_ ': rest) = LookupExit name rest
```

### 3. Remove/Deprecate Old Types

Mark as deprecated (remove in 07-migration):
- `data Input inputType` - replaced by `Entries`
- Keep `UsesEffects` for Logic nodes (they don't have exits, just gotos)

### 4. Entry/Exit Info Aggregation

```haskell
-- Full entry info for a node
type NodeEntryInfo :: Type -> [(Symbol, Type)]
type family NodeEntryInfo node where
  NodeEntryInfo node = FromMaybe '[] (GetEntries node)

-- Full exit info for a node
type NodeExitInfo :: Type -> [(Symbol, Type)]
type family NodeExitInfo node where
  NodeExitInfo node = FromMaybe '[] (GetExits node)

-- Check if node has entries (LLM node requirement)
type HasEntries :: Type -> Bool
type family HasEntries node where
  HasEntries node = IsJust (GetEntries node)

-- Check if node has exits (LLM node requirement)
type HasExits :: Type -> Bool
type family HasExits node where
  HasExits node = IsJust (GetExits node)
```

## Tests

Create `test/Graph/EntriesExitsSpec.hs`:

```haskell
-- Type-level tests using :~: equality
testGetEntries :: GetEntries (LLMNode :@ Entries '[ "a" ::: Int ]) :~: 'Just '[ '("a", Int) ]
testGetEntries = Refl

testLookupEntry :: LookupEntry "retry" '[ '("fresh", Int), '("retry", Text) ] :~: 'Just Text
testLookupEntry = Refl

testEntryNames :: EntryNames '[ '("a", Int), '("b", Text) ] :~: '["a", "b"]
testEntryNames = Refl
```

## PR Criteria

- [ ] New types compile
- [ ] Type families work (test with :~: proofs)
- [ ] Existing code still compiles (old types not removed yet)
- [ ] Haddock documentation on all new types
- [ ] No runtime code in this PR

## Branch

`refactor/record-nodes-01-core-types`
