# Types-First Dev v2: Incremental Multi-Agent Design

## Executive Summary

Redesign of types-first-dev to use **incremental Claude Code invocations** with **crosstalk between parallel agents**. Key shifts:

1. **Incremental work units** - Each Claude Code invocation does "some work", not "all work"
2. **Session continuity** - Same session accumulates history across invocations
3. **Harness-mediated crosstalk** - Between invocations, harness can rebase/inject context
4. **Test failures as prompts** - Skeleton tests fail with specification messages
5. **Structured templates** - LLM outputs semantic content, templates generate files

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              TYPES NODE                                      │
│                                                                              │
│  Input: StackSpec { projectPath, moduleName, description }                   │
│                                                                              │
│  Output: TypeDefinitions {                                                   │
│    dataType: "data Stack a = Empty | Push a (Stack a)",                     │
│    signatures: [                                                             │
│      { name: "empty", type: "Stack a", doc: "The empty stack" },            │
│      { name: "push", type: "a -> Stack a -> Stack a", doc: "Add to top" },  │
│      ...                                                                     │
│    ],                                                                        │
│    testPriorities: [                                                         │
│      { name: "lifo_order", description: "Stack maintains LIFO..." },        │
│      { name: "push_pop_inverse", description: "push then pop..." },         │
│    ]                                                                         │
│  }                                                                           │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         SKELETON GENERATION (Harness)                        │
│                                                                              │
│  From TypeDefinitions, harness generates via Jinja templates:                │
│                                                                              │
│  1. src/Data/Stack.hs - Module with stubs:                                  │
│     module Data.Stack (Stack, empty, push, ...) where                       │
│     data Stack a = Empty | Push a (Stack a)                                 │
│     empty :: Stack a                                                         │
│     empty = error "TODO: implement empty"                                    │
│     push :: a -> Stack a -> Stack a                                         │
│     push = error "TODO: implement push"                                      │
│     ...                                                                      │
│                                                                              │
│  2. test/Main.hs - Skeleton with failing-test-prompts:                      │
│     module Main where                                                        │
│     import Test.QuickCheck                                                   │
│     import Data.Stack                                                        │
│                                                                              │
│     -- Per-function tests                                                    │
│     prop_empty :: Bool                                                       │
│     prop_empty = error "SPEC: empty returns empty stack, isEmpty empty"     │
│                                                                              │
│     prop_push :: Int -> Stack Int -> Bool                                   │
│     prop_push = error "SPEC: push adds to top, peek (push x s) == Just x"  │
│                                                                              │
│     -- Priority tests (from testPriorities)                                  │
│     prop_lifo_order :: [Int] -> Bool                                        │
│     prop_lifo_order = error "SPEC: LIFO - push a,b,c; pop returns c,b,a"  │
│                                                                              │
│     main :: IO ()                                                            │
│     main = do                                                                │
│       putStrLn "=== Function tests ==="                                     │
│       quickCheck prop_empty                                                  │
│       quickCheck prop_push                                                   │
│       putStrLn "=== Priority tests ==="                                     │
│       quickCheck prop_lifo_order                                            │
│       ...                                                                    │
│                                                                              │
│  [CHECKPOINT: cabal build - verify skeleton compiles]                        │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                              FORK INTO PARALLEL                              │
│                                                                              │
│  Create two worktrees:                                                       │
│                                                                              │
│  test-worktree/                    impl-worktree/                           │
│  ├── src/Data/Stack.hs (stubs)     ├── src/Data/Stack.hs (stubs)           │
│  └── test/Main.hs (skeleton)       └── (NO test file - clean)               │
│                                                                              │
│  Note: impl-worktree intentionally lacks test skeleton to avoid             │
│  confusing the impl agent with failing tests it shouldn't address           │
└─────────────────────────────────────────────────────────────────────────────┘
                    │                               │
                    ▼                               ▼
┌──────────────────────────────┐   ┌──────────────────────────────────────────┐
│     TEST AGENT LOOP          │   │         IMPL AGENT LOOP                  │
│                              │   │                                          │
│  Session: test-session       │   │  Session: impl-session                   │
│  CWD: test-worktree/         │   │  CWD: impl-worktree/                     │
│                              │   │                                          │
│  Invocation 1:               │   │  Invocation 1:                           │
│    "Fill in prop_empty"      │   │    "Implement empty"                     │
│    → commits changes         │   │    → commits changes                     │
│    → structured output       │   │    → structured output                   │
│                              │   │                                          │
│  [CHECKPOINT]                │   │  [CHECKPOINT]                            │
│  [CROSSTALK: notify impl?]   │   │  [CROSSTALK: pull test commits?]         │
│                              │   │                                          │
│  Invocation 2:               │   │  Invocation 2:                           │
│    Context: "impl did X"     │   │    Context: "tests added prop_empty"     │
│    "Fill in prop_push"       │   │    "Implement push"                      │
│    → commits changes         │   │    → commits changes                     │
│    → structured output       │   │    → structured output                   │
│                              │   │                                          │
│  ... continue until done ... │   │  ... continue until done ...             │
└──────────────────────────────┘   └──────────────────────────────────────────┘
                    │                               │
                    └───────────────┬───────────────┘
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                              MERGE AGENT                                     │
│                                                                              │
│  Sees:                                                                       │
│    - main branch (skeleton files)                                           │
│    - test-worktree (filled-in tests)                                        │
│    - impl-worktree (filled-in impl)                                         │
│                                                                              │
│  Uses git CLI to merge. For conflicts:                                       │
│    - Can send messages to test-session or impl-session                      │
│    - Uses "pointer into history-state" to continue their context            │
│    - Requests clarification or asks one to yield                            │
│                                                                              │
│  Final: merged code on main branch                                          │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                           VALIDATION LOOP                                    │
│                                                                              │
│  [CHECKPOINT: cabal build - verify merged code compiles]                    │
│  [CHECKPOINT: cabal test - run all tests]                                   │
│                                                                              │
│  If failures:                                                                │
│    - Can invoke impl-session again with failure context                     │
│    - Can invoke test-session to check test correctness                      │
│    - Iterate until green                                                    │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Schema Redesign

### TypeDefinitions (from Types Node)

```haskell
data FunctionSig = FunctionSig
  { fsName :: Text
    -- ^ Function name (e.g., "push")
  , fsType :: Text
    -- ^ Type signature (e.g., "a -> Stack a -> Stack a")
  , fsDoc :: Text
    -- ^ Brief description for test generation
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TestPriority = TestPriority
  { tpName :: Text
    -- ^ Property name (e.g., "lifo_order")
  , tpDescription :: Text
    -- ^ What to test (e.g., "Stack maintains LIFO order")
  , tpRationale :: Maybe Text
    -- ^ Why this is important (optional)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TypeDefinitions = TypeDefinitions
  { tdDataType :: Text
    -- ^ The data type definition
  , tdDataTypeName :: Text
    -- ^ Just the type name (e.g., "Stack")
  , tdSignatures :: [FunctionSig]
    -- ^ Structured function signatures
  , tdTestPriorities :: [TestPriority]
    -- ^ High-level properties to test
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
```

### IncrementalProgress (for agent loops)

```haskell
data WorkStatus = MoreWork | Done
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data IncrementalProgress = IncrementalProgress
  { ipStatus :: WorkStatus
    -- ^ Whether more work remains
  , ipCompleted :: [Text]
    -- ^ What was completed this invocation (e.g., ["prop_empty", "prop_push"])
  , ipRemaining :: [Text]
    -- ^ What still needs work (e.g., ["prop_pop", "prop_peek"])
  , ipNotes :: Maybe Text
    -- ^ Any notes for the harness/other agents
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
```

---

## Template Designs

### impl-skeleton.jinja (generates src/Module/Path.hs)

```jinja
module {{ moduleName }}
  ( {{ dataTypeName }}
  {% for fn in signatures %}
  , {{ fn.name }}
  {% endfor %}
  ) where

{{ dataType }}

{% for fn in signatures %}
-- | {{ fn.doc }}
{{ fn.name }} :: {{ fn.type }}
{{ fn.name }} = error "TODO: implement {{ fn.name }}"

{% endfor %}
```

### test-skeleton.jinja (generates test/Main.hs)

```jinja
module Main where

import Test.QuickCheck
import {{ moduleName }}

-- ════════════════════════════════════════════════════════════════════════════
-- Per-Function Property Tests
-- ════════════════════════════════════════════════════════════════════════════

{% for fn in signatures %}
-- | Test for {{ fn.name }}: {{ fn.doc }}
prop_{{ fn.name }} :: {{ propTypeFor(fn) }}
prop_{{ fn.name }} = error "SPEC[{{ fn.name }}]: {{ fn.doc }}"

{% endfor %}

-- ════════════════════════════════════════════════════════════════════════════
-- Priority Property Tests
-- ════════════════════════════════════════════════════════════════════════════

{% for tp in testPriorities %}
-- | {{ tp.description }}
{% if tp.rationale %}-- Rationale: {{ tp.rationale }}{% endif %}
prop_{{ tp.name }} :: PropertyType  -- TODO: derive appropriate type
prop_{{ tp.name }} = error "SPEC[{{ tp.name }}]: {{ tp.description }}"

{% endfor %}

-- ════════════════════════════════════════════════════════════════════════════
-- Test Runner
-- ════════════════════════════════════════════════════════════════════════════

main :: IO ()
main = do
  putStrLn "=== Per-Function Tests ==="
  {% for fn in signatures %}
  putStrLn "Testing {{ fn.name }}..."
  quickCheck prop_{{ fn.name }}
  {% endfor %}

  putStrLn ""
  putStrLn "=== Priority Tests ==="
  {% for tp in testPriorities %}
  putStrLn "Testing {{ tp.name }}..."
  quickCheck prop_{{ tp.name }}
  {% endfor %}

  putStrLn ""
  putStrLn "All tests complete."
```

---

## Incremental Work Loop

### Concept

Instead of:
```
runClaudeCode(prompt) → complete output
```

We do:
```
session = newSession()
while not done:
    result = runClaudeCode(session, prompt, schema=IncrementalProgress)
    checkpoint(result)
    if crosstalk_available():
        inject_context(session, crosstalk_message)
    if result.status == Done:
        break
```

### Session Continuity

Each `runClaudeCode` call:
- Continues the same Claude session (via `--resume-session`)
- Accumulates conversation history
- Returns structured output (IncrementalProgress)

Between invocations:
- Harness can inject a user message: "FYI: test agent just committed prop_push"
- Harness can run checkpoints: `cabal build`, `cabal test`
- Harness can trigger rebases between worktrees

### Crosstalk Protocol

```
┌─────────────────────────────────────────────────────────────────┐
│  Test Agent commits prop_empty                                  │
│       ↓                                                         │
│  Harness detects commit in test-worktree                        │
│       ↓                                                         │
│  Harness runs: cd impl-worktree && git fetch test && git rebase │
│       ↓                                                         │
│  Harness injects into impl-session:                             │
│    "Context update: test agent added prop_empty which tests     │
│     that isEmpty empty == True. Consider this when implementing │
│     the empty function."                                        │
│       ↓                                                         │
│  Next impl invocation sees this context                         │
└─────────────────────────────────────────────────────────────────┘
```

---

## Open Questions / TBD

### 1. Work Unit Granularity
- One function per invocation?
- Agent decides how much to do?
- Harness specifies target?

**Proposal:** Agent decides, reports via IncrementalProgress.completed

### 2. Crosstalk Triggers
- Every N seconds?
- On every commit?
- On every invocation boundary?

**Proposal:** On invocation boundary, check if other agent has new commits

### 3. Property Type Derivation
For `prop_push :: ???`, how do we derive the property type from `push :: a -> Stack a -> Stack a`?

**Options:**
- LLM figures it out during skeleton generation
- Template has heuristics (functions with Stack in signature get Stack Int -> Bool, etc.)
- Leave as `error "TODO"` and test agent fills in signature too

**Proposal:** Test agent fills in signature as part of its work

### 4. Checkpoint Failure Handling
What if `cabal build` fails mid-loop?

**Options:**
- Inject error into agent's next prompt
- Retry with guidance
- Escalate to merge agent

**Proposal:** Inject error, let agent self-correct

### 5. Session Pointer for Merge Agent
How does merge agent "send message to test-session at history point X"?

**Needs:** Session ID + message injection mechanism that preserves history

**Proposal:** Track session IDs, implement `sendToSession(sessionId, message)` that continues that session with new user turn

### 6. Test Agent vs Impl Agent Ordering
- Fully parallel?
- Test slightly ahead?
- Impl waits for test skeleton to stabilize?

**Proposal:** Start parallel, test agent naturally gets ahead (simpler task), impl pulls test commits as available

### 7. Merge Conflict Resolution
If both touched same lines (unlikely given separation), how does merge agent resolve?

**Proposal:** Merge agent has full context of both sessions, uses judgment. Can ask either agent to yield/adjust via session continuation.

---

## Implementation Phases

### Phase 1: Schema & Template Refactor
- [ ] Update TypeDefinitions with FunctionSig, TestPriority
- [ ] Create impl-skeleton.jinja template
- [ ] Create test-skeleton.jinja template
- [ ] Add skeleton generation step after types node
- [ ] Verify skeleton compiles

### Phase 2: Incremental Loop Infrastructure
- [ ] Add IncrementalProgress schema
- [ ] Implement session continuity (--resume-session)
- [ ] Implement invocation loop with done detection
- [ ] Add checkpoint hooks (compile check)

### Phase 3: Worktree Provisioning
- [ ] Impl worktree: skeleton impl, NO test file
- [ ] Test worktree: skeleton impl + test skeleton
- [ ] Git setup for cross-worktree operations

### Phase 4: Basic Parallel Execution
- [ ] Run both loops in parallel (no crosstalk yet)
- [ ] Collect results
- [ ] Basic merge (no conflicts expected)

### Phase 5: Crosstalk
- [ ] Detect commits in other worktree
- [ ] Implement rebase mechanism
- [ ] Implement context injection
- [ ] Test interleaved execution

### Phase 6: Merge Agent
- [ ] Design merge agent prompt
- [ ] Implement session-messaging for conflict resolution
- [ ] Test conflict scenarios

### Phase 7: Validation Loop
- [ ] Post-merge test run
- [ ] Failure injection back to agents
- [ ] Iterate until green

---

## Next Steps (Immediate)

1. **Update Types.hs** with new schema (FunctionSig, TestPriority, etc.)
2. **Create templates** for skeleton generation
3. **Add skeleton generation** between types node and fork
4. **Test** that skeleton compiles before proceeding to parallel phase

This gives us the foundation before tackling the incremental loop complexity.
