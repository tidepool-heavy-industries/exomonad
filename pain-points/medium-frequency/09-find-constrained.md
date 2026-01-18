# Find Constrained Functions

**Frequency:** Weekly
**Use Case:** Finding all functions using a specific effect

## Problem

Want to find all functions with a specific constraint (e.g., `Member (Memory s) effs`). Grep finds constraint declarations but includes type signatures in comments, documentation, and non-function contexts.

## Current Workflow

1. Grep for constraint pattern
2. Manually filter results
3. Extract function names
4. Verify they're actual function definitions

**Time:** 5-8 minutes

## Desired Tool

```bash
$ find-constrained "Member (Memory s) effs"
```

**Output:**
```
Functions with constraint: Member (Memory s) effs

Handlers.hs:89:   processDoc :: Member (Memory DocGenState) effs => ...
Handlers.hs:154:  dgInit :: Member (Memory ExploreState) effs => ...
Process.hs:67:    updateState :: Member (Memory s) effs => ...

Total: 3 functions
```

## Why This Matters

- **Pattern discovery:** How is this effect used across the codebase?
- **Refactoring:** Which functions are affected by effect changes?
- **Understanding:** What capabilities does this module use?

## Implementation

1. Grep for constraint pattern
2. Parse type signatures
3. Extract function names
4. Filter to actual definitions (not comments/docs)

## Test Cases

### Test 1: Exact Match
```haskell
-- File: Handlers.hs:89
processDoc :: Member (Memory DocGenState) effs => Eff effs ()
```
**Expected:** ✅ Found

### Test 2: Polymorphic Match
```haskell
-- File: Process.hs:67
updateState :: Member (Memory s) effs => s -> Eff effs ()
```
**Expected:** ✅ Found (matches pattern with type variable)

### Test 3: Multiple Constraints
```haskell
-- File: Complex.hs:42
process :: (Member (Memory s) effs, Member Log effs) => Eff effs ()
```
**Expected:** ✅ Found (has matching constraint among others)

### Test 4: Comment
```haskell
-- File: Docs.hs:12
-- Example: Member (Memory s) effs constraint
```
**Expected:** ❌ Filtered (comment)

## Related Tools

- [find-effects](../high-frequency/02-find-effects.md) - Find effects used by a function (inverse)
- [find-type-instantiations](../high-frequency/03-find-type-instantiations.md) - Find concrete instantiations
