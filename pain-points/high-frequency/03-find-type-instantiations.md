# Find Type Instantiations

**Frequency:** 2-3 times per session
**Pain Level:** 7/10 (lots of false positives, manual type inference)

## Problem

When working with polymorphic types (especially GADT-style effects), I need to know **what concrete types are used at call sites**. For example, `Memory s` is polymorphic in `s`, and I want to know all concrete instantiations: `Memory DocGenState`, `Memory ExploreState`, etc.

## Current Workflow

1. Grep for `Memory` (or search for the type name)
2. Manually inspect each occurrence
3. Try to infer the type parameter from context:
   - Look at nearby type signatures
   - Check what record fields are accessed
   - Trace back to where the value was created
4. Make mental note of concrete types found
5. Deduplicate

**Time:** 5-8 minutes per search
**False positive rate:** High (finds imports, type definitions, etc.)

## Desired Tool

```bash
$ find-type-instantiations Memory
```

**Output:**
```
Concrete instantiations of Memory:

1. Memory DocGenState
   - Handlers.hs:154: getMem @DocGenState
   - Handlers.hs:203: updateMem @DocGenState state'
   - Types.hs:45: Definition of DocGenState

2. Memory ExploreState
   - OtherGraph.hs:87: getMem @ExploreState
   - OtherGraph.hs:112: putMem @ExploreState initial
   - Types.hs:23: Definition of ExploreState

3. Memory ChatState
   - Chat.hs:67: getMem @ChatState
   - Types.hs:89: Definition of ChatState

Total: 3 unique instantiations
```

## Why This Matters

- **Understanding polymorphism:** What concrete types are actually used?
- **Refactoring:** If I change the `Memory` type class, what types are affected?
- **Finding examples:** Want to add a new `Memory Foo` usage? See existing patterns.
- **Dead code detection:** Is `Memory BarState` actually used anywhere?

## Example: Real Use Case

In `tidepool-core`, the `Memory` effect is polymorphic:

```haskell
data Memory s r where
  GetMem :: Memory s s
  PutMem :: s -> Memory s ()

getMem :: Member (Memory s) effs => Eff effs s
```

When reading code, I see:
```haskell
state <- getMem
```

But what IS `state`? I need to trace back through type inference to find out it's `DocGenState`. This tool would tell me immediately.

## Implementation Approach

### Strategy: Type Application Parsing

Many modern Haskell codebases use visible type applications:

```haskell
getMem @DocGenState           -- explicit
getMem :: Eff effs DocGenState  -- in type signature
```

**Algorithm:**

1. `workspaceSymbol(Memory)` → find all occurrences
2. `findReferences(Memory)` → all usage sites
3. For each reference:
   - `hover` → get type information
   - **Parse type from hover text:**
     - Look for: `Memory SomeType`
     - Extract: `SomeType`
   - Handle type applications: `@SomeType` in source
4. For each unique type found:
   - `workspaceSymbol(SomeType)` → find definition
   - Group usage sites by type
5. Return aggregated results

### Parsing Hover Text

**Example hover response:**
```
getMem :: forall s (effs :: [Type -> Type]).
          Member (Memory s) effs =>
          Eff effs s
```

If we hover on a **call site**:
```
getMem @DocGenState

Hover: Eff effs DocGenState
```

We can extract `DocGenState` from the return type.

### Handling Visible Type Applications

```haskell
getMem @DocGenState  -- explicit type application
```

**Pattern:** `\b(\w+)\s+@\s*(\w+)`
**Extract:** Second capture group = concrete type

### Handling Implicit Instantiations

```haskell
state <- getMem
updateMem state'
```

No explicit type. **Solution:**
1. Hover on `getMem` → might show inferred type
2. Hover on `state` → shows concrete type
3. If still unclear, trace to where `state` is used:
   - Record field access: `state.esFrontier` → look up record type
   - Pattern match: `case state of ExploreState{..}` → extract type

This is where an **LLM could help**: parse complex hover text and infer types from usage context.

## Test Cases

### Test 1: Explicit Type Application
```haskell
-- File: Handlers.hs:154
state <- getMem @DocGenState
```

**Expected:** Detected as `Memory DocGenState` instantiation

### Test 2: Implicit via Record Access
```haskell
-- File: Handlers.hs:160
state <- getMem
let frontier = state.esFrontier  -- esFrontier is a DocGenState field
```

**Expected:** Inferred as `Memory DocGenState` (requires field lookup)

### Test 3: Constraint in Signature
```haskell
-- File: Handlers.hs:89
processDoc :: Member (Memory DocGenState) effs => Eff effs ()
processDoc = do
  state <- getMem
  ...
```

**Expected:** Detected as `Memory DocGenState` (from constraint)

### Test 4: Multiple Types in Same File
```haskell
-- File: Multi.hs
foo :: Member (Memory StateA) effs => Eff effs ()
foo = getMem >> pure ()

bar :: Member (Memory StateB) effs => Eff effs ()
bar = getMem >> pure ()
```

**Expected:**
- `Memory StateA` at Multi.hs:2
- `Memory StateB` at Multi.hs:5

## Challenges

### 1. Type Inference

Haskell's type inference means types aren't always explicit. Without visible type applications, we rely on:
- Hover showing inferred types
- Constraint declarations
- Usage context (field access, pattern matching)

### 2. Type Aliases

```haskell
type MyState = DocGenState
getMem :: Member (Memory MyState) effs => Eff effs MyState
```

Should we report `Memory MyState` or `Memory DocGenState`? **Solution:** Report both, mark alias relationship.

### 3. Higher-Kinded Types

```haskell
data Foo f a = Foo (f a)
-- Is this Foo [] Int or Foo Maybe String?
```

Harder to parse from hover text. Might need to **skip** or report as `Foo f a` (un-instantiated).

### 4. Nested Type Parameters

```haskell
Member (Memory (SomeWrapper DocGenState)) effs
```

Parse recursively: `Memory (SomeWrapper DocGenState)` → extract `SomeWrapper DocGenState` → extract `DocGenState`.

## Tiered Implementation

### Tier 1: Explicit Type Applications Only
Just parse `@TypeName` syntax. Fast, simple, low accuracy (misses implicit sites).

### Tier 2: Add Constraint Parsing
Parse `Member (Memory Foo) effs` from signatures. Higher accuracy.

### Tier 3: Add Hover Inference
Use hover on call sites to get inferred types. Much higher accuracy.

### Tier 4: Add Usage Context Analysis
Analyze field access, pattern matching to infer types. **This is where LLM helps.**

## Related Tools

- [find-effects](02-find-effects.md) - Find what effects are used (related to constraints)
- [find-constrained](../medium-frequency/09-find-constrained.md) - Find functions with specific constraint
- [show-fields](05-show-fields.md) - Show fields of a record type (useful for inference)
