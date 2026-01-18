# Find Field Access

**Frequency:** Weekly (during refactoring)
**Use Case:** Changing or removing record fields

## Problem

When modifying a record field (renaming, changing type, removing), I need to find all places where it's accessed (read) or updated (written). Grep finds the field name but includes false positives from other records with the same field name.

## Current Workflow

1. Grep for field name (e.g., `esFrontier`)
2. Manually filter results:
   - Is this the right record type?
   - Is it a read (`state.esFrontier`) or write (`state { esFrontier = ... }`)?
   - Is it in a pattern match, record update, or lens operation?

**Time:** 5-10 minutes for common field names
**False positive rate:** High if field name is reused across types

## Desired Tool

```bash
$ find-field-access ExploreState esFrontier
```

**Output:**
```
Accesses of ExploreState.esFrontier:

Reads (5):
  Handlers.hs:154: let frontier = state.esFrontier
  Handlers.hs:203: if null st.esFrontier
  Process.hs:67:   map process state.esFrontier
  Test.hs:89:      state.esFrontier `shouldBe` []
  Utils.hs:45:     length (s.esFrontier)

Writes (3):
  Handlers.hs:178: updateMem state { esFrontier = newFrontier }
  Init.hs:34:      ExploreState { esFrontier = [] }
  Process.hs:92:   state { esFrontier = filtered }

Total: 8 accesses (5 reads, 3 writes)
```

## Why This Matters

- **Refactoring:** Renaming field? Update all access sites
- **Type changes:** Changing field type? Find all reads/writes to update
- **Removing fields:** Ensure field is not used before removing
- **Understanding:** How is this field used?

## Implementation Approach

### Strategy

1. `workspaceSymbol(ExploreState)` → find record type definition
2. Verify field exists in record
3. Grep for field name (fast but noisy)
4. For each match:
   - `hover` to get type information
   - Check if the accessed record is `ExploreState`
   - Classify as read or write based on syntax

### Read vs Write Classification

**Reads:**
```haskell
state.esFrontier          -- record dot syntax
state^.esFrontier         -- lens getter
let x = s.esFrontier      -- in expression
```

**Writes:**
```haskell
state { esFrontier = x }  -- record update
state & esFrontier .~ x   -- lens setter
ExploreState { esFrontier = [] }  -- constructor
```

## Test Cases

### Test 1: Record Dot Read
```haskell
-- File: Handlers.hs:154
processState state = do
  let frontier = state.esFrontier
  ...
```
**Expected:** ✅ Read access

### Test 2: Record Update
```haskell
-- File: Handlers.hs:178
updateState state newFrontier =
  state { esFrontier = newFrontier }
```
**Expected:** ✅ Write access

### Test 3: Constructor
```haskell
-- File: Init.hs:34
initial = ExploreState
  { esTopic = topic
  , esFrontier = []
  , esVisited = Set.empty
  , esBudget = 20
  }
```
**Expected:** ✅ Write access (initialization)

### Test 4: Wrong Type (same field name)
```haskell
-- File: Other.hs:67
data OtherState = OtherState { esFrontier :: Text }

processOther s = s.esFrontier  -- Different type!
```
**Expected:** ❌ Filtered out (wrong type)

## Related Tools

- [show-fields](../high-frequency/05-show-fields.md) - See all fields of a record
- [find-constructors](../high-frequency/01-find-constructors.md) - Find where record is constructed
