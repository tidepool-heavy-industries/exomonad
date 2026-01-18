# Show Constructors

**Frequency:** Weekly
**Use Case:** Quick reference for sum types

## Problem

When working with sum types (ADTs), I need to quickly see all constructors and their types. Currently requires navigating to definition and reading the type declaration.

## Current Workflow

1. Go to definition (e.g., `LLMKind`)
2. Read constructor list
3. Navigate back

**Time:** 30-60 seconds

## Desired Tool

```bash
$ show-constructors LLMKind
```

**Output:**
```
Type: LLMKind
Location: Types.hs:42

Constructors:
  - Anthropic :: LLMKind
  - OpenAI    :: LLMKind
  - Local     :: LLMKind

Total: 3 constructors
```

## Why This Matters

- **Quick reference:** What are the possible values?
- **Pattern matching:** Know what cases to handle
- **Refactoring:** See full list before adding/removing

## Implementation

Similar to `show-fields` but parses sum type syntax instead of record syntax.

### Parsing Sum Types

```haskell
data LLMKind = Anthropic | OpenAI | Local
```

**Pattern:** `= Constructor1 | Constructor2 | ...`

### GADT Style

```haskell
data Memory s r where
  GetMem :: Memory s s
  PutMem :: s -> Memory s ()
```

**Pattern:** Parse each constructor with its full type signature.

## Test Cases

### Test 1: Simple Sum Type
```haskell
data LLMKind = Anthropic | OpenAI | Local
```
**Expected:**
```
Constructors:
  - Anthropic
  - OpenAI
  - Local
```

### Test 2: Constructors with Arguments
```haskell
data Result = Success Int | Failure String
```
**Expected:**
```
Constructors:
  - Success :: Int -> Result
  - Failure :: String -> Result
```

### Test 3: GADT
```haskell
data Memory s r where
  GetMem :: Memory s s
  PutMem :: s -> Memory s ()
```
**Expected:**
```
Constructors (GADT):
  - GetMem :: Memory s s
  - PutMem :: s -> Memory s ()
```

## Related Tools

- [show-fields](../high-frequency/05-show-fields.md) - For record types
- [find-pattern-matches](06-find-pattern-matches.md) - Find where constructors are matched
- [find-constructors](../high-frequency/01-find-constructors.md) - Find where constructors are called
