# Next Steps for Types-First Dev v2

## Files Created This Session

1. **DESIGN-v2.md** - Full architecture document with diagrams
2. **SKETCH-types-v2.hs** - Proposed type definitions (not compilable, for discussion)
3. **templates/impl-skeleton.jinja** - Template for generating impl stubs
4. **templates/test-skeleton.jinja** - Template for generating test skeleton with SPEC errors

## What's Working Now (v1)

- Types node → fork → parallel (tests, impl) → merge
- Agents return structured output (testModuleCode, implModuleCode)
- Files written to disk in merge handler
- Session forking disabled (was broken)

## Key Differences in v2

| Aspect | v1 | v2 |
|--------|----|----|
| Signatures | `[Text]` strings | `[FunctionSig]` structured |
| Module header | LLM generates | Template generates |
| Test file | LLM writes whole thing | Skeleton with SPEC errors |
| Agent invocation | Once to completion | Incremental loop |
| Crosstalk | None | Harness-mediated rebases |
| Progress tracking | None | IncrementalProgress schema |

## Immediate Next Steps

### Step 1: Update TypeDefinitions Schema
```haskell
-- FROM:
data TypeDefinitions = TypeDefinitions
  { tdDataType :: Text
  , tdSignatures :: [Text]  -- ["push :: a -> Stack a -> Stack a"]
  , tdModuleHeader :: Text
  }

-- TO:
data TypeDefinitions = TypeDefinitions
  { tdDataTypeName :: Text
  , tdDataType :: Text
  , tdSignatures :: [FunctionSig]  -- structured!
  , tdTestPriorities :: [TestPriority]
  }
```

### Step 2: Update types.jinja Prompt
Tell the LLM to output structured signatures instead of strings.

### Step 3: Add Skeleton Generation Step
Between types node and fork:
```
types node completes
    ↓
[harness: render impl-skeleton.jinja → src/Data/Stack.hs]
[harness: render test-skeleton.jinja → test/Main.hs]
[harness: cabal build - verify compiles]
    ↓
fork node
```

### Step 4: Provision Worktrees Correctly
- impl-worktree: gets impl skeleton, NO test file
- test-worktree: gets impl skeleton AND test skeleton

### Step 5: Test the Skeleton Flow
Before tackling incremental loops, verify:
- Types node produces structured output
- Skeletons generate correctly
- Skeletons compile
- Parallel agents can fill them in (even if single-shot for now)

## Questions for You to Consider

1. **FunctionSig.doc** - Should this be freeform or structured further?
   - Freeform: "Add element to top of stack"
   - Structured: { verb: "add", subject: "element", location: "top" }

2. **Property type derivation** - The test skeleton uses `Stack Int -> Bool` for everything.
   - Is that good enough?
   - Should types node output property signatures too?
   - Or let test agent fix the signatures as part of its work?

3. **Incremental granularity** - When test agent reports progress:
   - One property per invocation?
   - Agent decides how many?
   - Harness suggests target?

4. **Crosstalk frequency** - Check for crosstalk:
   - After every invocation?
   - Only when agent reports completion of something?
   - On a timer?

5. **Session message injection** - Format for context updates:
   - Plain text?
   - Structured JSON that agent parses?
   - Mix (human-readable with optional structured details)?

## Later Phases (Don't Start Yet)

- Incremental loop infrastructure
- Crosstalk protocol
- Merge agent
- Validation loop with failure feedback

## Files to Modify (When Ready)

```
types-first-dev/
├── src/TypesFirstDev/
│   ├── Types.hs         # Update TypeDefinitions
│   ├── Schema.hs        # Update TH derivations
│   ├── Context.hs       # Add SkeletonContext
│   ├── Templates.hs     # Add skeleton templates
│   ├── Graph.hs         # Add skeleton generation node
│   └── Handlers.hs      # Update handlers
├── templates/
│   ├── types.jinja      # Update for structured output
│   ├── impl-skeleton.jinja   # NEW (created)
│   └── test-skeleton.jinja   # NEW (created)
└── app/Main.hs          # Update to use new flow
```

## Current Git Status

Uncommitted changes from v1 work:
- ClaudeCodeResult type addition
- Parallel fork/merge handlers
- File writing in merge
- Schema.hs for TH derivation
- Various fixes

Consider: Commit v1 state before starting v2, or continue on same branch?
