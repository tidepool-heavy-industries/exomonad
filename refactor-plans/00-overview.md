# Record-Based Node DSL Refactor

## Summary

Replace single-input/single-exit node model with record-based entry/exit handlers.

**Before**:
```haskell
gWork :: mode :- LLMNode :@ Input TaskSpec :@ Template WorkTpl :@ Schema WorkResult
-- Single input, single structured output, routing via llmAfter
```

**After**:
```haskell
gWork :: mode :- LLMNode
    :@ Entries '[ "fresh" ::: TaskSpec, "retry" ::: RetryInfo ]
    :@ Template WorkTpl
    :@ Exits '[ "complete" ::: CompletePayload, "blocked" ::: BlockedPayload ]
-- Multiple entry handlers (all produce same ctx), multiple exit tools (each with own handler)
```

## Execution Model

- LLM picks which exit **tool** to call (exit = tool)
- Each exit has handler: `ExitPayload -> Eff es (GotoChoice targets)`
- Goto targets both node AND entry point: `gotoChoice @"work" @"retry" retryInfo`

## Work Units (Non-Overlapping)

| Plan | Files Owned | Dependencies | Parallelizable With |
|------|-------------|--------------|---------------------|
| 01-core-types | Types.hs, Edges.hs | None | - |
| 02-goto-refactor | Goto.hs, Goto/Internal.hs | 01 (types only) | 03, 04 |
| 03-validation | Validate/*.hs | 01 | 02, 04 |
| 04-handler-generation | Generic.hs | 01 | 02, 03 |
| 05-dispatch | Interpret.hs | 01, 04 | 06 |
| 06-actor-runtime | runtime/actor/ | 02 | 05 |
| 07-migration | tests, types-first-dev | ALL | - |

## Execution Order

```
Wave 1: [01-core-types]
         ↓
Wave 2: [02-goto-refactor] [03-validation] [04-handler-generation]  (parallel)
         ↓                   ↓               ↓
Wave 3: [06-actor-runtime]  [05-dispatch]                           (parallel)
         ↓                   ↓
Wave 4: [07-migration]
```

## PR Strategy

1. Each plan creates branch `refactor/record-nodes-NN-description`
2. Plans 02-06 branch from 01's merged result
3. Plan 07 branches from all merged
4. Use feature flags if needed for incremental merge

## Breaking Change

This is a clean break. All graphs must migrate. No backward compatibility layer.
