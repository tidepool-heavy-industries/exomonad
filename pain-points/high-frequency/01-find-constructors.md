# Find Constructors

**Frequency:** 5-10 times per session
**Pain Level:** 6/10 (tedious filtering)

## Problem

When refactoring or understanding a type, I need to know **where instances are created**. LSP's `findReferences` returns everything: constructor calls, pattern matches, imports, type signatures. Manually scanning each result to find actual construction sites is tedious.

## Current Workflow

1. Go to type definition (e.g., `ExploreState`)
2. LSP: Find references
3. Manually scan each reference
4. Identify which are constructor calls vs:
   - Pattern matches (`case x of ExploreState{..} -> ...`)
   - Imports (`import Foo (ExploreState)`)
   - Type signatures (`foo :: ExploreState -> ...`)
5. Make mental note of actual construction sites

**Time:** ~2-3 minutes per search, depends on number of references

## Desired Tool

```bash
$ find-constructors ExploreState
```

**Output:**
```
Handlers.hs:123:  ExploreState { esTopic = "test", esFrontier = [] }
Types.hs:87:      defaultExploreState = ExploreState "test" [] Set.empty 20
Runner.hs:156:    initial = ExploreState query mempty mempty budget
```

## Why This Matters

- **Impact analysis:** Adding a field? Find all constructor calls that need updating
- **Refactoring:** Changing constructor arguments? Find all call sites
- **Understanding:** Where does this state get initialized?

## Implementation Approach

### Pure LSP + Heuristics

1. `definition(ExploreState)` → get type location
2. `findReferences(ExploreState)` → all usages
3. For each reference:
   - Read surrounding lines (±3 lines of context)
   - **Heuristic filters:**
     - ❌ Skip if in import declaration
     - ❌ Skip if after `::` (type signature)
     - ❌ Skip if after `case ... of` or pattern position
     - ✅ Keep if followed by `{` (record construction)
     - ✅ Keep if followed by arguments in expression position
4. Return filtered locations

### Heuristic Rules

**Constructor call patterns:**
```haskell
ExploreState { field = value }       -- record syntax
ExploreState arg1 arg2 arg3          -- positional args
pure $ ExploreState ...              -- in expression
let x = ExploreState ...             -- in binding
```

**NOT constructor calls:**
```haskell
import Foo (ExploreState)            -- import
foo :: ExploreState -> ...           -- type signature
case x of ExploreState{..} -> ...    -- pattern match
```

### Accuracy

Heuristics should give **~90% accuracy** without LLM. False positives are rare, false negatives possible if unusual syntax.

## Test Cases

### Test 1: Record Constructor
```haskell
-- File: Handlers.hs:123
mkState topic = ExploreState
  { esTopic = topic
  , esFrontier = []
  , esVisited = Set.empty
  , esBudget = 20
  }
```
**Expected:** ✅ Detected as constructor call

### Test 2: Pattern Match
```haskell
-- File: Handlers.hs:200
processState (ExploreState topic frontier visited budget) = ...
```
**Expected:** ❌ Filtered out (pattern match)

### Test 3: Import
```haskell
-- File: Types.hs:5
import Tidepool.Scout.Types (ExploreState, TeachingDoc)
```
**Expected:** ❌ Filtered out (import)

### Test 4: Type Signature
```haskell
-- File: Handlers.hs:89
runExploration :: ExploreState -> IO TeachingDoc
```
**Expected:** ❌ Filtered out (type signature)

## Extension: Show Constructor Arity

Could also show expected vs provided arguments:

```bash
$ find-constructors ExploreState --check-arity
```

**Output:**
```
Handlers.hs:123:  ✓ ExploreState { esTopic, esFrontier, esVisited, esBudget }
                     (4/4 fields provided)
Types.hs:87:      ✗ ExploreState "test" [] Set.empty
                     (3/4 fields provided, missing: esBudget)
```

This catches bugs where fields are omitted (pre-Haskell record puns).

## Related Tools

- [find-pattern-matches](../medium-frequency/06-find-pattern-matches.md) - Inverse: find destructuring sites
- [show-fields](05-show-fields.md) - See what fields exist on the type
