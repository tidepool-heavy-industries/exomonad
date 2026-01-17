# Degenerate Training Data Examples

An example is **degenerate** when it provides no meaningful learning signal for FunctionGemma's core job: **navigating the code graph by deciding which edges to follow**.

## Degenerate Criteria

### 1. No Candidates (Unreachable)

```
Inputs: (none)
Output: (none)
References: [empty or none]
```

The model has nowhere to navigate. Can't teach it anything.

---

### 2. Polymorphic/Generic Types (Too Abstract)

```
Output: a, b, c, m, f
```

Single-letter type variables don't teach domain-specific navigation. The model learns "generic type parameters exist" but not "follow this edge to understand the codebase."

---

### 3. Hub Symbols with No Inputs (Dead Ends)

```
Inputs: (none)
References: [many references - hub symbol]
```

The symbol is heavily used (20+ references) but has no input types. This means:
- You can't *reach* it from other symbols
- It's only reachable as an entry point
- Following references teaches nothing about data flow

**Example**: `Text` - everything references it, but you can't get there by following types.

---

### 4. Sparse References + No Inputs (Isolated)

```
Inputs: (none)
References: [one usage]
```

The symbol appears in exactly one place and has no inputs. No navigation paths.

---

### 5. Internal/Generated Symbols (Noise)

```
Symbol: $fFoo$cBar, $wfoo, etc.
```

GHC-generated symbols (worker functions, type class dictionaries) don't represent real code navigation.

---

### 6. Identity Transforms (No Learning Signal)

```
Inputs: Foo
Output: Foo
```

When input type = output type (common for identity functions, wrappers), there's redundancy. The model sees "Foo leads to Foo" which doesn't expand navigation.

---

## Why This Matters for FunctionGemma

FunctionGemma learns patterns like:
- "Given `User` as context, follow `userId` (field) and `UserId` (type) and `loginHandler` (reference)"
- "Given `Response`, follow `status`, `headers`, and `sendResponse` to understand the flow"

**Degenerate examples violate this** by having:
- No edges to follow (no inputs/outputs/references)
- Edges that lead nowhere (hub symbols)
- Edges that are noise (internal symbols)
- Edges that don't teach anything (identity transforms)

The filtering is about **data quality for learning**, not completeness. Better to have 486 high-signal examples than 800 with lots of noise.
