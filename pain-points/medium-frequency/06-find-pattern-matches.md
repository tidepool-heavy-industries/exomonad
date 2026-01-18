# Find Pattern Matches

**Frequency:** Weekly (during refactoring)
**Use Case:** Adding/removing constructors from sum types

## Problem

When adding a constructor to a sum type (e.g., adding `Gemini` to `LLMKind`), I need to find all pattern matches to ensure they're updated. LSP's `findReferences` returns all references, but I only care about **pattern matches** (destructuring), not constructor calls, imports, or type signatures.

## Current Workflow

1. LSP: Find references on type name
2. Manually scan each reference
3. Identify which are pattern matches:
   - `case x of LLMKind -> ...`
   - Function definitions: `handleLLM Anthropic = ...`
   - Let/where patterns: `let Anthropic = ...`
4. Check if pattern is exhaustive or uses wildcards

**Time:** 3-5 minutes per type
**Risk:** Missing a pattern match breaks compilation or causes runtime errors

## Desired Tool

```bash
$ find-pattern-matches LLMKind
```

**Output:**
```
Pattern matches on LLMKind:

Handlers.hs:123 (exhaustive):
  case kind of
    Anthropic -> callAnthropic
    OpenAI    -> callOpenAI
    Local     -> callLocal

Types.hs:87 (uses wildcard):
  showKind Anthropic = "Anthropic"
  showKind _         = "Other"

Total: 2 match sites
  - 1 exhaustive (SAFE for new constructor)
  - 1 with wildcard (REVIEW: may hide new case)
```

## Why This Matters

- **Refactoring safety:** Ensure all pattern matches handle new constructor
- **Dead code detection:** Find patterns that never match
- **Code review:** Verify exhaustiveness when adding variants

## Implementation Approach

### Heuristic Detection

1. `findReferences(LLMKind)` → all references
2. For each reference, check context:
   - After `case ... of`
   - In function definition left-hand side
   - After `let` or `where` in binding position
3. Read surrounding lines to extract full pattern match
4. Classify as exhaustive vs using wildcards

### Pattern Match Contexts

**Case expression:**
```haskell
case x of
  Anthropic -> ...
  OpenAI    -> ...
```

**Function definition:**
```haskell
handleLLM Anthropic args = ...
handleLLM OpenAI args    = ...
handleLLM Local args     = ...
```

**Let/where binding:**
```haskell
let Anthropic = extractKind config in ...
```

## Test Cases

### Test 1: Exhaustive Case
```haskell
-- File: Handlers.hs:123
processKind kind = case kind of
  Anthropic -> "anthropic"
  OpenAI    -> "openai"
  Local     -> "local"
```
**Expected:** ✅ Detected as exhaustive pattern match

### Test 2: Wildcard Pattern
```haskell
-- File: Handlers.hs:200
showKind Anthropic = "Anthropic"
showKind _         = "Other"
```
**Expected:** ⚠️ Detected as wildcard pattern (may need update)

### Test 3: Constructor Call (not pattern)
```haskell
-- File: Config.hs:45
defaultKind = Anthropic
```
**Expected:** ❌ Filtered out (constructor call, not pattern)

## Related Tools

- [find-constructors](../high-frequency/01-find-constructors.md) - Inverse: find construction sites
- [show-constructors](08-show-constructors.md) - List all constructors of sum type
