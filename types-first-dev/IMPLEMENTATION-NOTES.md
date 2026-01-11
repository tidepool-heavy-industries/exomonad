# V3 TDD Implementation Progress - Phases 10-12

**Status**: Handler implementation and routing complete. Executor integration pending.

**Test Results**: 10/10 tests passing ✅

---

## Phases Completed

### Phase 10: Handler Assembly & Graph Wiring ✅

**Objective**: Wire handlers into TDDGraph execution via handler record.

**What Changed**:
- Fixed all 6 handler before-functions to return `(TemplateContext, SessionOperation)` tuple
  - Was: returning just context
  - Now: `implBefore :: Input → Eff es (Context, SessionOperation)`
- Updated handlers in all 6 modules:
  - `ImplReviewMerge.hs` (3 handlers: Impl, TDDReviewImpl, Merger)
  - `TDDWriteTests.hs` (1 handler)
  - `Scaffold.hs` (1 handler)
  - `Rebaser.hs` (1 handler)
- Added `SessionOperation` routing:
  - `StartFresh slug` - start new session
  - `ContinueFrom sessionId` - resume previous session
  - `ForkFrom parentId childSlug` - fork from parent

**Key Discovery**:
Handler signature investigation revealed that `ClaudeCodeLLMHandler` has 4 fields matching `LLMHandler`:
```haskell
data ClaudeCodeLLMHandler schema = ClaudeCodeLLMHandler
  { llmSystem :: Text
  , llmUser :: TemplateContext → Text
  , llmBefore :: Input → Eff es (Context, SessionOperation)  -- Tuple!
  , llmAfter :: (schema, SessionId) → Eff es (GotoChoice [...])
  }
```

### Phase 11: Handler Type Investigation ✅

**Objective**: Understand exact `ClaudeCodeLLMHandler` signature and document pattern.

**Findings**:
1. Before-function returns `(context, SessionOperation)` tuple
2. After-function receives `(parsedSchema, SessionId)` pair
3. `SessionOperation` enum allows:
   - Fresh sessions (no context from previous attempts)
   - Continuation sessions (preserve context across retries)
   - Fork sessions (branching from parent context)

**Documentation Added**:
- Updated `haskell/dsl/core/CLAUDE.md` with complete handler pattern
- Added examples of before/after handler signatures
- Documented `SessionOperation` variants and use cases

### Phase 12: After-Function Routing Logic ✅

**Objective**: Implement routing logic for all 6 LLM handlers based on LLM output.

**Handlers Implemented**:

#### Impl Handler
```haskell
implBefore :: Input → Eff es (Context, SessionOperation)
  - Reads ImplMem for stored SessionId
  - Returns ContinueFrom if stored, else StartFresh
  - Chains conversation context across retries

implAfter :: Input → (Exit, SessionId) → Eff es (GotoChoice [...])
  - ImplTestsPassed → route to TDDReviewImpl
  - ImplRequestRetry → Impl self-loop OR exit (max 5 attempts)
  - ImplStuck → exit with stuck message
  - Stores SessionId in memory for next attempt
```

#### TDDReviewImpl Handler
```haskell
tddReviewImplBefore :: Input → Eff es (Context, SessionOperation)
  - StartFresh (no retry logic at this level)

tddReviewImplAfter :: (Exit, SessionId) → Eff es (GotoChoice [...])
  - TDDApproved → route to Merger
  - TDDMoreTests → route back to TDDWriteTests
  - TDDReject → exit graph
  - TODO: Executor must provide parent/child NodeInfo for Merger
```

#### TDDWriteTests Handler
```haskell
tddWriteTestsBefore :: Input → Eff es (Context, SessionOperation)
  - StartFresh

tddWriteTestsAfter :: (Exit, SessionId) → Eff es (GotoChoice [...])
  - TDDTestsReady → route to Impl with TestsReadyPayload
  - TDDInvalidScaffold → route back to Scaffold
  - TODO: Retrieve original ScaffoldInput from memory
```

#### Scaffold Handler
```haskell
scaffoldAfter :: Exit → Eff es (GotoChoice [...])
  - ScaffoldInitWork → route to Fork with InitWorkPayload
  - ScaffoldClarificationNeeded → exit graph
  - TODO: Child spawning (requires Subgraph effect + executor context)
```

#### Merger Handler
```haskell
mergerAfter :: Exit → Eff es (GotoChoice [...])
  - MergerComplete → exit with MergeComplete result
  - MergerRejected → route back to Impl for retry
  - TODO: Retrieve original ImplInput from memory for retry
```

#### Rebaser Handler
```haskell
rebaserAfter :: Exit → Eff es (GotoChoice [...])
  - RebaserClean/Adapted → route to TDDWriteTests
  - RebaserConflict → route to Scaffold for conflict escalation
  - TODO: Get inputs from memory with updated context
```

---

## Memory Threading (Session Continuation)

**ImplMem** stores:
```haskell
data ImplMem = ImplMem
  { imConversationId :: Text
  , imSessionId :: Maybe SessionId    -- NEW: For retry continuation
  , imPassedTests :: [Text]
  , imAttemptHistory :: [AttemptRecord]
  }
```

**Flow**:
1. `implBefore` reads `imSessionId` from memory
2. If `Just sid` exists → `ContinueFrom sid` (resume session)
3. If `Nothing` → `StartFresh` (new session)
4. `implAfter` stores new SessionId via `updateMem`
5. Next attempt reads stored SessionId → continuation

**TDDMem** stores:
```haskell
data TDDMem = TDDMem
  { tmConversationId :: Text
  , tmCoveredCriteria :: [Text]    -- Tracks which criteria have tests
  , tmPendingTests :: [Text]       -- Tests written but not passing yet
  }
```

---

## What's Working ✅

- [x] Handler before-functions return correct tuple type
- [x] Handler after-functions parse LLM output via decision tools
- [x] Impl self-loop retry logic (max 5 attempts)
- [x] Session ID persistence in memory
- [x] Routing via GotoChoice to next nodes
- [x] All 10 basic integration tests passing
- [x] Library compiles without errors

---

## What's Pending (Next Phases)

### Phase 13: Fork & ImplBarrier Orchestration ⏳

**Problem**: ForkNode and BarrierNode are graph DSL constructs, not traditional handlers.

**What needs implementation**:
- ForkNode dispatcher: Spawn TDDWriteTests + ImplBarrier in parallel
- BarrierNode async merge: Wait for TestsReady + children's MergeComplete
- Subgraph effect integration: Spawn child graphs, collect results

**Blockers**:
- Graph executor must invoke ForkNode routing
- Subgraph effect interpreter needed
- Child result collection via `awaitAny`

### Phase 14: Impl Retry Self-Loop ✅ (Completed in Phase 12)

Already implemented:
```haskell
case exit of
  ImplRequestRetry _diagnosis _from _to _failing
    | input.iiAttemptCount >= 5 -> gotoExit ImplStuck
    | otherwise -> gotoSelf (input { iiAttemptCount = input.iiAttemptCount + 1 })
```

### Phase 15: Full E2E with Claude Code Execution ⏳

**Requirements**:
1. Graph executor that invokes handlers with proper signatures
2. Executor context providing:
   - Parent/child NodeInfo (for Merger)
   - Original inputs (for back-routing)
   - Subgraph state (for child execution)
3. ClaudeCodeLLMHandler invocation:
   - Render template with context
   - Call Claude Code subprocess via mantle
   - Parse structured output via decision tools
   - Route via after-handler

**What would be tested**:
- URL shortener spec loads and parses
- Scaffold decomposes into child tasks
- Children execute independently via Subgraph
- ImplBarrier collects TestsReady + MergeComplete
- Impl retries on test failure (up to 5 times)
- TDDReviewImpl routes to Merger on approval
- Merger broadcasts results to parent
- Root graph exits with complete MergeComplete

---

## Executor Context Gaps

Several TODOs in handlers require executor to provide context:

### MergerInput Construction (TDDReviewImplAfter)
```haskell
let mergerInput = MergerInput
      { miParentNode = ???        -- Need from executor
      , miChildNode = ???         -- Need from executor
      , miTddApproval = approval
      , miContractSuite = ???
      }
```

**Solution**: Executor passes node context to handler via effect or closure.

### Back-routing Inputs (TDDWriteTestsAfter, MergerAfter)
```haskell
-- Route back to Scaffold with original ScaffoldInput
gotoChoice @"v3Scaffold" (???)

-- Route back to Impl with original ImplInput
gotoChoice @"v3Impl" (???)
```

**Solution**: Store inputs in memory during before-handler, retrieve in after-handler.

### Child Result Collection (ImplBarrierHandler)
```haskell
-- Wait for all children to complete
children <- awaitAll @Spec @MergeComplete (pending)
```

**Solution**: Subgraph effect must provide async collection primitives.

---

## Architecture Notes

### Handler Signature Pattern

All LLM handlers follow this pattern:

```haskell
myBefore :: (Members [...] es) => Input → Eff es (Context, SessionOperation)
myBefore input = do
  -- Compute context from input
  -- Determine session strategy (fresh vs. continue)
  pure (ctx, sessionOp)

myAfter :: (Members [...] es) => (Exit, SessionId) → Eff es (GotoChoice [...])
myAfter (exit, sessionId) = do
  -- Update memory (store SessionId, etc.)
  case exit of
    ExitVariant1 fields → route to next node
    ExitVariant2 fields → route to different node
    ...
```

### Routing via GotoChoice

All routable decisions use typed routing:

```haskell
gotoChoice @"v3Impl" implInput           -- route to specific node
gotoSelf retryInput                      -- self-loop on same input
gotoExit resultValue                     -- exit with result
```

Type safety ensures: Can only route to nodes that accept that input type.

### Memory Effect Pattern

Each node stores persistent state for later retrieval:

```haskell
-- Before
updateMem @TDDMem $ \m → m { tmConversationId = newId }

-- After
mem ← getMem @TDDMem
let conversationId = mem.tmConversationId
```

Memory is shared within node group:
- TDDWriteTests ↔ TDDReviewImpl (share TDDMem)
- Impl ↔ ImplBarrier (share ImplMem)

---

## File Changes Summary

| File | Change | Impact |
|------|--------|--------|
| `src/TypesFirstDev/Handlers/ImplReviewMerge.hs` | Fixed 3 before/after pairs | Core handler routing |
| `src/TypesFirstDev/Handlers/TDDWriteTests.hs` | Fixed before/after | TDD loop handler |
| `src/TypesFirstDev/Handlers/Scaffold.hs` | Fixed before, kept after | Entry point |
| `src/TypesFirstDev/Handlers/Rebaser.hs` | Fixed before, kept after | Sibling adaptation |
| `src/TypesFirstDev/Types/Memory.hs` | Added `imSessionId` field | Session continuation |
| `src/TypesFirstDev/Schema.hs` | Removed ImplMem schema | Fixed JSON schema conflict |
| `haskell/dsl/core/CLAUDE.md` | Added handler pattern docs | Framework documentation |

---

## Next Steps (For Future Phases)

1. **Implement Graph Executor**
   - Invoke handlers with proper signatures
   - Provide node context to handlers
   - Interpret GotoChoice routing

2. **Wire Executor Context**
   - Pass parent/child NodeInfo to handlers
   - Store/retrieve node inputs for back-routing
   - Integrate Subgraph effect

3. **Implement Subgraph Effect**
   - Spawn child graphs recursively
   - Await child results via awaitAny
   - Collect MergeComplete results

4. **E2E Test with Claude Code**
   - Spawn actual Claude Code subprocess
   - Run real TDD workflow on test spec
   - Verify all phases execute end-to-end

---

## Summary

**Accomplishments**:
- ✅ Investigated handler type signature (Phase 11)
- ✅ Fixed all 6 handler signatures (Phase 10)
- ✅ Implemented after-function routing (Phase 12)
- ✅ Wired session continuation via Memory
- ✅ Documented handler pattern for framework
- ✅ All tests passing

**Remaining Work**:
- Executor context provision
- Graph execution engine
- Subgraph effect implementation
- E2E test with Claude Code

The handler layer is complete and ready for executor integration.
