# Find Callers (Filtered)

**Frequency:** 10+ times per session
**Pain Level:** 5/10 (LSP works but noisy results)

## Problem

LSP's `findReferences` returns **all** references: actual function calls, type signatures, imports, re-exports, comments. I usually only care about **actual call sites** where the function is invoked.

## Current Workflow

1. LSP: Find references on function name
2. Manually scan each result
3. Filter out:
   - Import statements
   - Type signatures
   - Re-exports
   - Module headers
   - Comments/documentation
4. Identify actual calls

**Time:** 1-2 minutes per search
**Annoyance:** High (lots of noise, especially for commonly-imported functions)

## Desired Tool

```bash
$ find-callers exploreEff
```

**Output:**
```
Actual call sites:

Handlers.hs:203:  result <- exploreEff query
Runner.hs:87:     runM $ runLSP session $ exploreEff q
Test.hs:42:       testResult = runPure $ exploreEff testQuery

Total: 3 call sites (filtered 8 other references)
```

## Why This Matters

- **Refactoring:** Changing function signature? Find all call sites.
- **Impact analysis:** Who depends on this function?
- **Understanding usage:** How is this function typically called?
- **Dead code detection:** Is this function actually used?

## Example: Real Use Case

Finding callers of `getMem`:

**LSP raw output:**
```
Memory.hs:135:  getMem :: Member (Memory s) effs => Eff effs s  [DEFINITION]
Memory.hs:68:   -- | Get current memory state. See 'getMem'.     [COMMENT]
Graph.hs:98:    import Tidepool.Graph.Memory (getMem, putMem)  [IMPORT]
Handlers.hs:43: import Tidepool.Graph.Memory (getMem)          [IMPORT]
Handlers.hs:154: state <- getMem                                [CALL]
Handlers.hs:203: st <- getMem                                   [CALL]
Test.hs:126:    it "getMem returns current state" $ do         [STRING]
```

**Filtered output (what I want):**
```
Handlers.hs:154: state <- getMem
Handlers.hs:203: st <- getMem

Total: 2 call sites
```

## Implementation Approach

### Heuristic Filtering

1. `findReferences(functionName)` → all references
2. For each reference, read surrounding context (±2 lines)
3. **Filter out:**
   - Lines containing `import`
   - Lines after `::` in type signatures
   - Lines in comments (start with `--` or in `{- ... -}`)
   - Lines in module headers (`module Foo (bar) where`)
   - Lines in strings/documentation
4. **Keep:**
   - Expression position (after `=`, `<-`, `let`, `$`, etc.)
   - Argument position (inside function call)

### Heuristic Rules

**Call site patterns:**
```haskell
x <- getMem              -- monadic bind
let x = getMem in ...    -- let binding
pure $ getMem            -- application
foo getMem bar           -- argument
```

**NOT call sites:**
```haskell
import Foo (getMem)               -- import
getMem :: Member (Memory s) effs  -- type signature
-- See getMem for details         -- comment
module Foo (getMem) where         -- export list
"use getMem to fetch state"       -- string literal
```

### Implementation Steps

```python
def findCallers(functionName):
  refs = lsp.findReferences(functionName)
  callSites = []

  for ref in refs:
    context = readLines(ref.file, ref.line - 2, ref.line + 2)

    # Filter out non-call sites
    if isImport(context):
      continue
    if isTypeSignature(context):
      continue
    if isComment(context):
      continue
    if isModuleHeader(context):
      continue
    if isStringLiteral(context):
      continue

    # This is likely a call site
    callSites.append(ref)

  return callSites
```

### Detection Functions

**isImport:**
```python
def isImport(context):
  return any("import" in line for line in context)
```

**isTypeSignature:**
```python
def isTypeSignature(context):
  # Look for :: on the same line or previous line
  for line in context:
    if "::" in line and functionName in line:
      # Check if functionName appears before ::
      parts = line.split("::")
      if functionName in parts[0]:
        return True
  return False
```

**isComment:**
```python
def isComment(context):
  targetLine = context[len(context) // 2]  # middle line is the reference
  stripped = targetLine.lstrip()
  return stripped.startswith("--") or stripped.startswith("{-")
```

**isStringLiteral:**
```python
def isStringLiteral(context):
  targetLine = context[len(context) // 2]
  # Count quotes before the reference
  before = targetLine[:targetLine.index(functionName)]
  return before.count('"') % 2 == 1  # inside string if odd number of quotes
```

## Test Cases

### Test 1: Import Statement
```haskell
-- File: Handlers.hs:5
import Tidepool.Graph.Memory (getMem, putMem)
```
**Expected:** ❌ Filtered out

### Test 2: Type Signature
```haskell
-- File: Types.hs:42
helper :: Member (Memory s) effs => Eff effs s
helper = getMem
```
**Expected:**
- Line 42: ❌ Filtered (type signature)
- Line 43: ✅ Kept (actual call)

### Test 3: Actual Call
```haskell
-- File: Handlers.hs:154
processDoc = do
  state <- getMem
  pure state
```
**Expected:** ✅ Kept (call site)

### Test 4: Comment
```haskell
-- File: Memory.hs:68
-- | Get current memory. Use 'getMem' to retrieve state.
getMem :: Member (Memory s) effs => Eff effs s
```
**Expected:**
- Line 68: ❌ Filtered (comment)
- Line 69: ❌ Filtered (definition/signature)

### Test 5: String Literal
```haskell
-- File: Test.hs:126
it "getMem returns current state" $ do
  result <- getMem
  result `shouldBe` expectedState
```
**Expected:**
- Line 126: ❌ Filtered (in string)
- Line 127: ✅ Kept (actual call)

## Accuracy Considerations

### False Positives

Heuristics might keep:
- Documentation examples in docstrings (if not properly detected)
- Quoted code in comments

**Mitigation:** Improve comment detection to handle multi-line comments and docstrings.

### False Negatives

Heuristics might filter out:
- Calls in unusual contexts (e.g., TH splices: `$(getMem)`)
- Calls in string interpolation (QuasiQuotes)

**Mitigation:** Add special cases for known patterns, or accept 95% accuracy.

## Extensions

### Show Call Context

```bash
$ find-callers getMem --context 3
```

**Output:**
```
Handlers.hs:152-157:
  152: processDoc :: Member (Memory DocGenState) effs => Eff effs ()
  153: processDoc = do
→ 154:   state <- getMem
  155:   let frontier = state.esFrontier
  156:   logDebug $ "Processing " <> show frontier
  157:   pure ()
```

### Group by File

```bash
$ find-callers getMem --group-by-file
```

**Output:**
```
Handlers.hs (2 calls):
  - Line 154: state <- getMem
  - Line 203: st <- getMem

Runner.hs (1 call):
  - Line 87: runM $ runLSP session $ getMem

Total: 3 calls across 2 files
```

### Show Call Patterns

```bash
$ find-callers getMem --show-patterns
```

**Output:**
```
Monadic bind (2): state <- getMem
Application (1): pure $ getMem

Most common pattern: Monadic bind
```

## Related Tools

- [find-effects](02-find-effects.md) - Find effects used by a function (caller → callee direction)
- [find-pattern-matches](../medium-frequency/06-find-pattern-matches.md) - Find destructuring sites (inverse of constructors)
