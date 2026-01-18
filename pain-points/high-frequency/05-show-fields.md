# Show Fields (Quick Reference)

**Frequency:** 10+ times per session
**Pain Level:** 4/10 (lots of navigation overhead)

## Problem

When working with record types, I frequently need to quickly see **what fields exist and their types**. Current workflow requires navigating to definition and visually scanning the record declaration.

## Current Workflow

1. Go to definition of record type (e.g., `ExploreState`)
2. Scroll to find the record declaration
3. Read fields and types
4. Navigate back to where I was

**Time:** 30-60 seconds per lookup
**Interruption cost:** Context switching overhead

## Desired Tool

```bash
$ show-fields ExploreState
```

**Output:**
```
Type: ExploreState
Location: Types.hs:45

Fields:
  esTopic    :: Text
  esFrontier :: [SymbolKey]
  esVisited  :: Set SymbolKey
  esBudget   :: Int

Total: 4 fields
```

## Why This Matters

- **Quick reference:** Don't remember if it's `esFrontier` or `frontier`
- **Type checking:** What type does this field expect?
- **Refactoring:** What fields exist before I add a new one?
- **Learning:** Understanding a new record type structure

## Example: Real Use Case

When writing code that uses `ExploreState`:

```haskell
processState :: Member (Memory ExploreState) effs => Eff effs ()
processState = do
  state <- getMem
  -- What fields does state have? Quick lookup!
  let frontier = state.esFrontier  -- ah, it's esFrontier
```

Instead of navigating to definition, just run `show-fields ExploreState`.

## Implementation Approach

### Pure LSP + Parsing

1. `workspaceSymbol(ExploreState)` → find type
2. `definition(ExploreState)` → get location
3. Read file at location
4. Parse record syntax:
   ```haskell
   data ExploreState = ExploreState
     { esTopic    :: Text
     , esFrontier :: [SymbolKey]
     , esVisited  :: Set SymbolKey
     , esBudget   :: Int
     }
   ```
5. Extract fields and types
6. Format output

### Record Syntax Parsing

**Pattern:** `{ fieldName :: Type , ... }`

**Regex approach:**
```regex
\{\s*(\w+)\s*::\s*([^,}]+)
```

**Captures:**
- Group 1: Field name
- Group 2: Field type

**Handle multi-line:**
```haskell
data Foo = Foo
  { field1 :: Type1
  , field2 :: Type2
  }
```

Read from `{` to `}`, parse each line.

### Alternative: LSP Hover

Some LSP servers provide field info via hover on the type name:

```haskell
data ExploreState = ExploreState { ... }
       ^
       hover here
```

**Hover response might include:**
```
data ExploreState = ExploreState
  { esTopic :: Text
  , esFrontier :: [SymbolKey]
  , esVisited :: Set SymbolKey
  , esBudget :: Int
  }
```

If available, this is easier than parsing. Check if HLS provides this.

## Test Cases

### Test 1: Simple Record
```haskell
-- File: Types.hs:45
data ExploreState = ExploreState
  { esTopic    :: Text
  , esFrontier :: [SymbolKey]
  , esVisited  :: Set SymbolKey
  , esBudget   :: Int
  }
```

**Expected:**
```
Type: ExploreState
Location: Types.hs:45

Fields:
  esTopic    :: Text
  esFrontier :: [SymbolKey]
  esVisited  :: Set SymbolKey
  esBudget   :: Int

Total: 4 fields
```

### Test 2: Record with Complex Types
```haskell
-- File: Types.hs:23
data DocGenState = DocGenState
  { dgQuery   :: TeachQuery
  , dgResults :: Map SymbolKey TeachingUnit
  , dgConfig  :: Maybe TeachingConfig
  }
```

**Expected:**
```
Type: DocGenState
Location: Types.hs:23

Fields:
  dgQuery   :: TeachQuery
  dgResults :: Map SymbolKey TeachingUnit
  dgConfig  :: Maybe TeachingConfig

Total: 3 fields
```

### Test 3: Sum Type (not a record)
```haskell
-- File: Types.hs:89
data LLMKind = Anthropic | OpenAI | Local
```

**Expected:**
```
Type: LLMKind
Location: Types.hs:89

Not a record type (sum type with 3 constructors)

Constructors:
  - Anthropic
  - OpenAI
  - Local
```

### Test 4: GADT Record
```haskell
-- File: Memory.hs:119
data Memory s r where
  GetMem :: Memory s s
  PutMem :: s -> Memory s ()
```

**Expected:**
```
Type: Memory
Location: Memory.hs:119

GADT (not a simple record)

Constructors:
  GetMem :: Memory s s
  PutMem :: s -> Memory s ()
```

## Extensions

### Show Default Values

If a `defaultFoo` function exists, show default values:

```bash
$ show-fields ExploreState --with-defaults
```

**Output:**
```
Type: ExploreState
Location: Types.hs:45

Fields (with defaults from defaultExploreState):
  esTopic    :: Text         = "untitled"
  esFrontier :: [SymbolKey]  = []
  esVisited  :: Set SymbolKey = Set.empty
  esBudget   :: Int          = 20

Total: 4 fields
```

This requires finding and parsing `defaultExploreState` definition.

### Show Field Usages

```bash
$ show-fields ExploreState --usage
```

**Output:**
```
Type: ExploreState
Location: Types.hs:45

Fields (with usage counts):
  esTopic    :: Text         (read: 5, write: 2)
  esFrontier :: [SymbolKey]  (read: 12, write: 8)
  esVisited  :: Set SymbolKey (read: 8, write: 6)
  esBudget   :: Int          (read: 3, write: 1)
```

This requires finding all field accesses (e.g., `state.esFrontier`).

### Compare Two Records

```bash
$ show-fields ExploreState DocGenState --compare
```

**Output:**
```
Common field patterns:
  Both have "topic" field (esTopic vs dgQuery.topic)

Unique to ExploreState:
  esFrontier :: [SymbolKey]
  esVisited  :: Set SymbolKey
  esBudget   :: Int

Unique to DocGenState:
  dgResults :: Map SymbolKey TeachingUnit
  dgConfig  :: Maybe TeachingConfig
```

Useful when refactoring or unifying similar types.

## Challenges

### 1. Multiline Types

```haskell
data Foo = Foo
  { complexField :: forall a. Show a
                 => Maybe (Either String a)
                 -> IO ()
  }
```

Types can span multiple lines. **Solution:** Parse from `::` to next `,` or `}`, handling newlines.

### 2. Record Syntax Variations

```haskell
-- Style 1: Spaces around ::
{ field :: Type }

-- Style 2: No spaces
{field::Type}

-- Style 3: Alignment
{ field1 :: Type1
, field2 :: Type2
}
```

Parser must handle all variations.

### 3. StrictData Extension

```haskell
data Foo = Foo
  { !field1 :: Type1  -- strict field
  , field2  :: Type2
  }
```

Handle `!` prefix (strictness annotation).

### 4. Documentation Comments

```haskell
data Foo = Foo
  { field1 :: Type1  -- ^ Field documentation
  , field2 :: Type2  -- ^ More docs
  }
```

Optionally include field documentation in output.

## Related Tools

- [show-constructors](../medium-frequency/08-show-constructors.md) - For sum types (inverse)
- [find-field-access](../medium-frequency/07-find-field-access.md) - Find where field is used
- [find-constructors](01-find-constructors.md) - Find where record is constructed
