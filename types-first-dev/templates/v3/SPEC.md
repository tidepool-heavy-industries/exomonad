# V3 Protocol Specification

7-node TDD graph with parallel spawn, tree coordination, and typed DSL mapping.

**Nodes:** Scaffold → Fork → (TDDWriteTests, ImplBarrier) → Impl → TDDReviewImpl → Merger → Rebaser

```
Entry(Spec)
    │
    ▼
┌──────────┐
│ Scaffold │──[Subgraph]──▶ Child Graphs (spawnSelf per childSpec)
└──────────┘
    │
    ▼ InitWork
┌──────────┐
│ ForkNode │ ─────────────────────────────────────────────────────┐
└──────────┘                                                      │
    │                                                             │
    ├──[Spawn]──▶ TDDWriteTests ──TestsReady──┐                  │
    │                                          │                  │
    └──[Spawn]──▶ ImplBarrier ◄────────────────┤                  │
                      │                        │                  │
                      │ Awaits: TestsReady     │                  │
                      │       + [MergeComplete]◄──────────────────┘
                      ▼                        (children via Subgraph)
                 ┌────────┐
                 │  Impl  │
                 └────────┘
                      │
    ┌─────────────────┴─────────────────┐
    │                                   │
    ▼ TestsPassed                       ▼ RequestRetry (self-loop)
┌───────────────┐                  ┌────────┐
│ TDDReviewImpl │                  │  Impl  │ (max 5 attempts)
└───────────────┘                  └────────┘
    │
    ├── Approved ──▶ Merger ──MergeComplete──▶ Rebaser (siblings)
    │                   │
    │                   └──▶ Parent (broadcast)
    │
    └── MoreTests ──▶ TDDWriteTests (write additional tests)
```

**Key insight:** TDDWriteTests, TDDReviewImpl, and Impl share parent's conversation history as starting context (threaded via Memory effect). Different templates, same backing conversationId.

---

## DSL Mapping

This spec maps to the Tidepool Graph DSL. See `haskell/dsl/core/CLAUDE.md` for DSL reference.

| Protocol Concept | DSL Construct |
|------------------|---------------|
| Parallel spawn (TDD + Impl) | `ForkNode` with `Spawn` annotation |
| Wait for dependencies | `BarrierNode` with `Awaits` annotation |
| Child graph spawning | `Subgraph` effect (`spawnSelf`, `awaitAny`) |
| Node-private state | `Memory` annotation |
| Shared conversation | `Global` annotation on graph |
| Typed transitions | `Goto` / `GotoChoice` |
| Self-loop retry | `Goto Self` with `dispatchGotoWithSelf` |
| Sum type outputs | `oneOf` schema (validated at compile time) |

---

## Shared Types

```yaml
Spec:
  id: Text                        # unique identifier
  description: Text
  acceptanceCriteria: [Criterion]
  targetPath: FilePath
  testPath: FilePath
  complexityConstraints: Maybe Constraints  # optional O(n), space limits, etc.

Constraints:
  time: Maybe Text   # e.g. "O(n log n)"
  space: Maybe Text  # e.g. "O(1)"

Criterion:
  id: Text
  text: Text

PlannedTest:
  criterionId: Text
  plannedTestName: Text
  approach: Text

Critique:
  file: FilePath
  line: Int
  issue: Text
  requiredFix: Text

ImpactLevel: Trivial | Additive | Breaking

ChangeEntry:
  symbol: Text
  changeType: SignatureChange | NewExport | RemovedExport | BehaviorChange
  reason: Text

NodeInfo:
  nodeId: Text
  branch: Text

CoverageReport:
  criteriaWithTests: [(Text, Text)]  # (criterionId, testName)
  criteriaMissing: [Text]

MergeComplete:
  mergeCommit: Text
  author: Text
  impactLevel: ImpactLevel
  changes: [ChangeEntry]

# Broadcast payload (subset of MergeComplete for sibling notification)
MergeEvent:
  author: Text
  impactLevel: ImpactLevel
  changes: [ChangeEntry]

# Scaffold output, input to TDD and Impl
InitWorkPayload:
  scaffoldCommit: Text
  interfaceFile: FilePath
  contractSuite: FilePath
  testPlan: [PlannedTest]

# TDD WriteTests output, input to Impl
TestsReadyPayload:
  testsCommit: Text
  testFiles: [FilePath]
  pendingCriteria: [Text]

# Impl output, input to TDD ReviewImpl
ImplResult:
  commitHash: Text
  iterations: Int
  passedTests: [Text]

# TDD Approved output, input to Merger
TDDApproval:
  signOff: Text
  coverageReport: CoverageReport

# Rebaser adaptation record
Adaptation:
  symbol: Text
  change: Text
  reason: Text
```

---

## Node 1: Scaffold

**DSL:** `LLMNode :@ Input ScaffoldInput :@ Schema ScaffoldExit :@ Template ScaffoldTpl`

**Effects:** `[LLM, Subgraph Spec MergeComplete, Git, Emit]`

### Input Type

```yaml
ScaffoldInput:
  spec: Spec
  parentContext: Maybe ParentContext

ParentContext:
  interface: Text        # parent's interface definitions
  assignedCriteria: Text # which criteria this child must satisfy
```

### Output Type (oneOf)

```yaml
ScaffoldExit:
  oneOf:
    - InitWork:
        # Always emitted - triggers parallel spawn of TDD + Impl
        scaffoldCommit: Text
        interfaceFile: FilePath
        contractSuite: FilePath
        testPlan: [PlannedTest]
        # Optional children (spawn in parallel with TDD/Impl)
        childSpecs: Maybe [ChildSpec]
        interfaces: Maybe [InterfaceFile]
    - ClarificationNeeded:
        question: Text
        ambiguityReference: Text
        specSentence: Text       # exact text causing confusion

ChildSpec:
  id: Text
  description: Text
  acceptanceCriteria: [Criterion]
  targetPath: FilePath
  testPath: FilePath

InterfaceFile:
  path: FilePath
  exports: [Text]
```

**Key change:** No separate `SpawnTree` vs `InitLeaf`. Always `InitWork`, optionally with children.

**Child spawning (Subgraph effect):** If `childSpecs` is present, handler spawns children:

```haskell
scaffoldAfter :: ScaffoldExit -> Eff (Subgraph Spec MergeComplete ': es) (GotoChoice targets)
scaffoldAfter (InitWork payload) = do
  -- Spawn children if present (run in parallel, await later at ImplBarrier)
  forM_ (payload.childSpecs) $ \childSpec ->
    spawnSelf (Spec { id = childSpec.id, ... })

  -- Transition to ForkNode which spawns TDD + ImplBarrier in parallel
  pure $ gotoChoice @"fork" payload
```

### Template

```jinja
# Scaffold: {{ spec.description }}

You are the **Planner**. Analyze the spec and set up the work structure.

## Spec

**ID:** `{{ spec.id }}`
**Description:** {{ spec.description }}

### Acceptance Criteria
{% for c in spec.acceptanceCriteria %}
- {{ c.id }}: {{ c.text }}
{% endfor %}

{% if spec.complexityConstraints %}
### Complexity Constraints
{% if spec.complexityConstraints.time %}- Time: {{ spec.complexityConstraints.time }}{% endif %}
{% if spec.complexityConstraints.space %}- Space: {{ spec.complexityConstraints.space }}{% endif %}
{% endif %}

{% if parentContext %}
## Parent Context

You are a child node. Parent's interface:
{{ parentContext.interface }}

You must satisfy: {{ parentContext.assignedCriteria }}

Parent's interface files are **read-only**.
{% endif %}

---

## Decision: Children Needed?

**No children** (implement directly):
- Single responsibility
- <500 LOC
- All criteria tightly coupled

**With children** (decompose):
- Multiple subsystems
- Clear interface boundaries
- Independent testability

Either way, TDD and Impl nodes spawn in parallel.

---

## Actions

1. Create interface file (Types.hs with signatures)
2. Create contract suite (tests this node must pass)
3. Plan tests (one per criterion)
4. **If children needed:**
   - Define interfaces between children
   - Partition criteria among children
5. Commit scaffold

---

## Output

Emit `InitWork`:
- Always: scaffoldCommit, interfaceFile, contractSuite, testPlan
- If decomposing: childSpecs, interfaces

Or `ClarificationNeeded` if spec is ambiguous.
```

---

## Node 2: Fork

**DSL:** `ForkNode :@ Input InitWorkPayload :@ Spawn '[To "tddWriteTests" InitWorkPayload, To "implBarrier" InitWorkPayload] :@ Barrier "implBarrier"`

**Effects:** None (pure routing)

Spawns TDDWriteTests and ImplBarrier in parallel after Scaffold completes.

```haskell
-- ForkNode handler builds HList of payloads for workers
forkHandler :: InitWorkPayload -> Eff es (HList '[InitWorkPayload, InitWorkPayload])
forkHandler payload = pure $ payload ::: payload ::: HNil
```

---

## Node 3: TDDWriteTests

**DSL:** `LLMNode :@ Input TDDWriteTestsInput :@ Schema TDDWriteTestsExit :@ Template TDDWriteTestsTpl :@ Memory TDDMem`

**Effects:** `[LLM, Memory TDDMem, Git, Emit]`

Writes failing tests for all criteria. Spawned in parallel with ImplBarrier via ForkNode.

### Input Type

```yaml
TDDWriteTestsInput:
  spec: Spec
  scaffold: InitWorkPayload
```

### Memory (node-private, threaded context)

```yaml
TDDMem:
  conversationId: Text     # resume from parent's context
  coveredCriteria: [Text]  # criterionIds with passing tests
  pendingTests: [Text]     # tests written but not yet passed
```

### Output Type (oneOf)

```yaml
TDDWriteTestsExit:
  oneOf:
    - TestsReady:
        testsCommit: Text
        testFiles: [FilePath]
        pendingCriteria: [Text]
    - InvalidScaffold:
        missingType: Text
        expectedLocation: FilePath
```

### Template

```jinja
# TDD WriteTests: {{ spec.description }}

You are the **Test Author**. Write failing tests that drive implementation.

## Spec

{% for c in spec.acceptanceCriteria %}
- {{ c.id }}: {{ c.text }}
{% endfor %}

## Test Plan (from scaffold)

{% for t in scaffold.testPlan %}
- {{ t.criterionId }}: `{{ t.plannedTestName }}` — {{ t.approach }}
{% endfor %}

## Already Covered

{% for id in coveredCriteria %}
- {{ id }} ✓
{% endfor %}

---

## Your Task

Write failing tests for ALL uncovered criteria in one batch. Commit them together.

**Note:** TDD writes all tests upfront (batch model), not one test at a time (ping-pong model).

### Requirements

1. Test public API only
2. One criterion per test
3. Tests MUST fail on skeleton (red)
4. Commit all tests together, then emit TestsReady

---

## Output

- **TestsReady**: Tests committed → arrives at ImplBarrier
- **InvalidScaffold**: Types missing → routes back to Scaffold
```

---

## Node 4: ImplBarrier

**DSL:** `BarrierNode :@ Awaits '[TestsReadyPayload, HList [MergeComplete]] :@ UsesEffects '[Goto "impl" ImplInput]`

**Effects:** `[Subgraph Spec MergeComplete]` (to call `awaitAny` for children)

Blocks until TDDWriteTests emits TestsReady AND all children emit MergeComplete.

### Barrier Logic

```haskell
implBarrierHandler
  :: (TestsReadyPayload, HList [MergeComplete])  -- Results from TDD + children
  -> Eff (Subgraph Spec MergeComplete ': es) (GotoChoice '[To "impl" ImplInput])
implBarrierHandler (testsReady, childMerges) = do
  -- Collect any remaining children (spawned by Scaffold)
  allChildMerges <- collectRemainingChildren childMerges

  pure $ gotoChoice @"impl" ImplInput
    { testsReady = testsReady
    , childMerges = if null allChildMerges then Nothing else Just allChildMerges
    , attemptCount = 1
    , critiqueList = Nothing
    }

collectRemainingChildren :: [MergeComplete] -> Eff (Subgraph s MergeComplete ': es) [MergeComplete]
collectRemainingChildren acc = do
  pending <- getPending
  if null pending
    then pure acc
    else do
      (_, result) <- awaitAny
      collectRemainingChildren (result : acc)
```

---

## Node 5: TDDReviewImpl

**DSL:** `LLMNode :@ Input TDDReviewImplInput :@ Schema TDDReviewImplExit :@ Template TDDReviewImplTpl :@ Memory TDDMem`

**Effects:** `[LLM, Memory TDDMem, Git, Emit]`

Reviews Impl's work after it claims tests pass. Shares Memory with TDDWriteTests.

### Input Type

```yaml
TDDReviewImplInput:
  spec: Spec
  scaffold: InitWorkPayload
  implResult: ImplResult
  diff: Text
```

### Output Type (oneOf)

```yaml
TDDReviewImplExit:
  oneOf:
    - Approved:
        signOff: Text
        coverageReport: CoverageReport
    - MoreTests:
        critiques: [Critique]
        additionalTests: [PlannedTest]
    - Reject:
        reason: Text
        missingCriteria: [Text]
```

### Template

```jinja
# TDD ReviewImpl: {{ spec.description }}

You are the **Reviewer**. Impl claims tests pass. Verify and decide.

## Impl Result

Commit: `{{ implResult.commitHash }}`
Iterations: {{ implResult.iterations }}

**Tests passed:**
{% for testName in implResult.passedTests %}
- `{{ testName }}`
{% endfor %}

## Diff

```diff
{{ diff }}
```

## Review Protocol

### Phase 1: Mechanical Checks
- [ ] `cabal build` passes
- [ ] `cabal test` passes
- [ ] No `undefined` outside stubs
- [ ] Interface unchanged from scaffold

### Phase 2: Semantic Review
- Does impl actually satisfy the criterion?
- Code readable in 60 seconds?
- No unsafe imports?

---

## Output

- **Approved**: All good → routes to Merger
- **MoreTests**: Need additional coverage → routes to TDDWriteTests
- **Reject**: Fundamental problems → escalate
```

---

## Node 6: Impl

**DSL:** `LLMNode :@ Input ImplInput :@ Schema ImplExit :@ Template ImplTpl :@ Memory ImplMem :@ UsesEffects '[Goto Self ImplInput, Goto "tddReviewImpl" TDDReviewImplInput, Goto "scaffold" ScaffoldInput, Goto Exit StuckResult]`

**Effects:** `[LLM, Memory ImplMem, Git, Emit]`

Impl node makes tests pass. Receives input from ImplBarrier after dependencies are met.

### Input Type

```yaml
ImplInput:
  spec: Spec
  scaffold: InitWorkPayload          # see Shared Types
  testsReady: TestsReadyPayload      # see Shared Types; from TDD node
  childMerges: Maybe [MergeComplete] # see Shared Types; if children exist
  attemptCount: Int                  # orchestrator-tracked
  critiqueList: Maybe [Critique]     # from TDD's MoreTests
```

### Memory (node-private, threaded context)

```yaml
ImplMem:
  conversationId: Text       # resume from parent's context
  passedTests: [Text]        # tests that now pass
  attemptHistory: [AttemptRecord]

AttemptRecord:
  strategy: Text
  outcome: Text              # "failed: <reason>" or "partial: <progress>"
```

### Output Type (oneOf)

```yaml
ImplExit:
  oneOf:
    - TestsPassed:
        commitHash: Text
        iterations: Int
        passedTests: [Text]
    - RequestRetry:
        diagnosis: Text
        strategyFrom: Text
        strategyTo: Text
        failingTests: [Text]
    - BlockedDependency:
        missingSymbol: Text
        expectedImportPath: FilePath
    - SpecAmbiguity:
        specSentence: Text
        contradictionTrace: Text
        question: Text
    - Stuck:
        diagnosis: Text
        recommendation: Text
        attempts: Int
```

### Template

```jinja
# Impl: {{ spec.description }}

You are the **Implementer**. Make ALL failing tests pass.

## Tests to Pass

{% for f in testsReady.testFiles %}
- {{ f }}
{% endfor %}

Criteria: {{ testsReady.pendingCriteria | join(", ") }}

{% if childMerges %}
## Child Subsystems Available

{% for m in childMerges %}
- {{ m.author }}: {{ m.changes | map(attribute='symbol') | join(", ") }}
{% endfor %}

Use these subsystems - don't reimplement.
{% endif %}

## Attempt {{ attemptCount }} of 5

{% if critiqueList %}
### TDD Feedback

{% for c in critiqueList %}
- **{{ c.file }}:{{ c.line }}** — {{ c.issue }}
  Fix: {{ c.requiredFix }}
{% endfor %}
{% endif %}

---

## Your Task

1. Run tests (verify red)
2. Implement minimum code to pass ALL tests
3. Run tests (verify green)
4. Commit

**Constraints:**
- Do NOT touch test files
- Do NOT change interface signatures
- USE child subsystems if available

---

## Output

- **TestsPassed**: All green → routes to TDDReviewImpl for review
- **RequestRetry**: Some failing, have strategy → self-loop (max 5)
- **BlockedDependency**: Need missing code → routes to Scaffold
- **SpecAmbiguity**: Unclear requirement → routes to Scaffold
- **Stuck**: Cannot proceed → human escalation
```

---

## Node 7: Merger

**DSL:** `LLMNode :@ Input MergerInput :@ Schema MergerExit :@ Template MergerTpl`

**Effects:** `[LLM, Git, Emit]`

Files MR to parent after TDDReviewImpl approves.

### Input Type

```yaml
MergerInput:
  parentNode: NodeInfo
  childNode: NodeInfo
  tddApproval: TDDApproval  # see Shared Types
  contractSuite: FilePath
```

### Output Type (oneOf)

```yaml
MergerExit:
  oneOf:
    - MergeComplete  # see Shared Types
    - MergeRejected:
        reason: ContractViolation | BuildFailure | IntegrationFailure
        details: Text
        failingTests: [Text]
```

### Template

```jinja
# Merge: {{ childNode.nodeId }}

You are the **Merger**. Fold completed child into parent.

## Context

Parent: `{{ parentNode.nodeId }}` on `{{ parentNode.branch }}`
Child: `{{ childNode.nodeId }}` on `{{ childNode.branch }}`

### TDD Approval

{{ tddApproval.signOff }}

**Coverage:**
{% for item in tddApproval.coverageReport.criteriaWithTests %}
- {{ item[0] }}: `{{ item[1] }}` ✓
{% endfor %}

---

## Merge Protocol

### 1. Squash Merge

```bash
git merge --squash {{ childNode.branch }}
git commit -m "feat({{ childNode.nodeId }}): implement"
```

### 2. Verification

- [ ] `cabal build` passes
- [ ] Contract suite passes: `cabal test --match="Contract"`
- [ ] Integration tests pass: `cabal test`

---

## Output

- **MergeComplete**: Success → broadcast to siblings, exit
- **MergeRejected**: Failed → routes back to Impl
```

---

## Node 8: Rebaser

**DSL:** `LLMNode :@ Input RebaserInput :@ Schema RebaserExit :@ Template RebaserTpl`

**Effects:** `[LLM, Git, Emit]`

Triggered by sibling MergeComplete broadcast. Adapts to sibling changes.

### Input Type

```yaml
RebaserInput:
  node: NodeInfo
  parentBranch: Text
  newParentHead: Text
  mergeEvent: MergeEvent  # see Shared Types
```

### Output Type (oneOf)

```yaml
RebaserExit:
  oneOf:
    - RebaseClean:
        newBase: Text
    - RebaseAdapted:
        newBase: Text
        adaptations: [Adaptation]  # see Shared Types
    - RebaseConflict:
        conflictFile: FilePath
        ourChange: Text
        theirChange: Text
        whyUnresolvable: Text
```

### Template

```jinja
# Rebase: Sibling Merged

You are the **Rebaser**. A sibling just merged. Adapt or break.

## Context

Your Node: `{{ node.nodeId }}`
Your Branch: `{{ node.branch }}`
New Parent HEAD: `{{ newParentHead }}`

## MergeEvent

**Author:** {{ mergeEvent.author }}
**Impact:** {{ mergeEvent.impactLevel }}

{% for c in mergeEvent.changes %}
- {{ c.symbol }} ({{ c.changeType }}): {{ c.reason }}
{% endfor %}

---

## Rebase Protocol

### 1. Pause Impl/TDD Loop

No commits during rebase.

### 2. Rebase

```bash
git fetch origin {{ parentBranch }}
git rebase origin/{{ parentBranch }}
```

### 3. Conflict Resolution

**Invariant:** Sibling is Canon. You are Draft.
**Restriction:** Rebaser may only make MECHANICAL changes.

{% if mergeEvent.impactLevel == "Trivial" %}
Auto-resolve expected. No semantic changes.
{% elif mergeEvent.impactLevel == "Additive" %}
Sibling added new exports. You may use them.
{% else %}
**BREAKING** — Sibling changed APIs you may depend on.

**Allowed (mechanical):**
- Import updates
- Symbol renames
- Signature adjustments (same logic)

**Forbidden (semantic):**
- Changing function bodies
- Altering control flow
- Rewriting logic

If breaking change requires logic updates → emit `RebaseConflict` and let Impl (with full TDD context) handle it.
{% endif %}

### 4. Verify

```bash
cabal test
```

---

## Output

- **RebaseClean**: Trivial rebase → resume at TDDWriteTests
- **RebaseAdapted**: Breaking changes absorbed → resume at TDDWriteTests
- **RebaseConflict**: Cannot resolve → escalate to Scaffold
```

---

## Edge Summary

### Graph Flow

```
Scaffold ──InitWork──▶ Fork ──Spawn──▶ TDDWriteTests ──TestsReady──┐
    │                    │                                          │
    │ (Subgraph)         └──Spawn──▶ ImplBarrier ◄──────────────────┤
    │                                     │                         │
    ▼                                     │ (Awaits + awaitAny)     │
  Children ──MergeComplete────────────────┘                         │
                                          ▼                         │
                                       Impl ──TestsPassed──▶ TDDReviewImpl
                                         │                         │
                                         │ (Goto Self)             │
                                         └─────────────────────────┤
                                                                   │
                                         ├── Approved ──▶ Merger ──┼──▶ Rebaser
                                         │                         │    (siblings)
                                         └── MoreTests ──▶ TDDWriteTests
```

### Node Edges

| From | Exit | To | DSL Construct |
|------|------|----|---------------|
| Scaffold | InitWork | Fork | `Goto "fork"` |
| Scaffold | ClarificationNeeded | Exit | `Goto Exit` |
| Fork | (spawns) | TDDWriteTests, ImplBarrier | `Spawn '[To "tddWriteTests", To "implBarrier"]` |
| TDDWriteTests | TestsReady | ImplBarrier | `Arrive "implBarrier"` (implicit) |
| TDDWriteTests | InvalidScaffold | Scaffold | `Goto "scaffold"` |
| ImplBarrier | (unblocks) | Impl | `Goto "impl"` (after Awaits satisfied) |
| Impl | TestsPassed | TDDReviewImpl | `Goto "tddReviewImpl"` |
| Impl | RequestRetry | Impl | `Goto Self` (max 5 via attemptCount) |
| Impl | BlockedDependency | Scaffold | `Goto "scaffold"` |
| Impl | SpecAmbiguity | Scaffold | `Goto "scaffold"` |
| Impl | Stuck | Exit | `Goto Exit` |
| TDDReviewImpl | Approved | Merger | `Goto "merger"` |
| TDDReviewImpl | MoreTests | TDDWriteTests | `Goto "tddWriteTests"` |
| TDDReviewImpl | Reject | Exit | `Goto Exit` |
| Merger | MergeComplete | Parent + Rebaser | Broadcast (Subgraph parent, sibling event) |
| Merger | MergeRejected | Impl | `Goto "impl"` |
| Rebaser | RebaseClean | TDDWriteTests | `Goto "tddWriteTests"` |
| Rebaser | RebaseAdapted | TDDWriteTests | `Goto "tddWriteTests"` |
| Rebaser | RebaseConflict | Scaffold | `Goto "scaffold"` |

### Wait Dependencies (DSL Constructs)

| Node | Waits For | DSL Construct |
|------|-----------|---------------|
| ImplBarrier | TDDWriteTests.TestsReady | `BarrierNode :@ Awaits '[TestsReadyPayload, ...]` |
| ImplBarrier | Children.MergeComplete | `Subgraph` effect (`awaitAny`) |
| Merger | TDDReviewImpl.Approved | Data flow (Merger input requires TDDApproval) |
| Rebaser | Sibling.MergeComplete | Event subscription (outside graph) |

**Blocking behavior:** ImplBarrier (a `BarrierNode`) blocks until:
1. TDDWriteTests emits TestsReady (via `Awaits`)
2. All children emit MergeComplete (via `Subgraph.awaitAny`)

---

## Retry Protocol

Orchestrator handles retries, not nodes:

- **Max 5 attempts** per node invocation (tracked by orchestrator via `attemptCount`)
- If node emits `RequestRetry` at attempt 5 → orchestrator auto-converts to `Stuck`
- **Timeout:** If no output after 60s, nudge with reminder to emit decision
- Nodes see `attemptCount` in input but cannot lie about it

**Nudge escalation:**
1. Attempt 1-4: Normal retry with `RequestRetry` context
2. Attempt 5: Final attempt, must succeed or emit `Stuck`
3. Timeout: "REMINDER: Emit a decision to complete this phase"

---

## Parent-as-Judge Pattern

**Key insight:** The parent node wrote the spec, so it has full context to judge children.

**Where:** Parent's TDDReviewImpl node acts as judge. When a child emits MergeComplete, the parent's TDDReviewImpl is invoked with the child's diff to verify `assignedCriteria` satisfaction.

When a child completes (MergeComplete), the parent evaluates:
- Does the merged code satisfy the criteria assigned to this child?
- Does it integrate correctly with sibling outputs?

No separate Judge node needed. The parent's TDDReviewImpl/Merger flow naturally includes semantic evaluation because it authored the acceptance criteria.

### When Parent Judges

| Event | Parent Action |
|-------|---------------|
| Child MergeComplete | Verify child satisfies `assignedCriteria` from `ParentContext` |
| All children complete | Run contract suite, verify integration |
| Contract failure | Route failing child back to its TDDWriteTests |

### Why This Works

- Parent has spec context (wrote the criteria)
- Parent has interface context (defined boundaries)
- Parent has sibling context (sees all children's work)
- Threaded Memory effect (shares conversationId)

---

## Envelope Pattern

Routing envelope is small/fast. Full payload written to context store.

```yaml
# What orchestrator sees (fast routing)
envelope:
  type: "BLOCKED_DEPENDENCY"
  severity: "BLOCKER"
  route_hint: "check_sibling_exports"

# What recipient reads (full context)
payload_ref: "sha256:abcd1234..."  # content-addressable
```

**Why?** Orchestrator doesn't need stack traces. It just needs to know who runs next.

**Content-Addressable Storage:** Use CAS (like git blobs) for payloads. If Node A generates a massive `CoverageReport`, Node B only fetches the chunks it needs. Avoids constant serialize/deserialize of large payloads.

---

## Design Decisions

1. **Threaded context** via Memory effect - TDDWriteTests, TDDReviewImpl, and Impl share parent's conversationId
2. **Explicit parallel spawn** via ForkNode/BarrierNode - no hidden orchestrator magic
3. **Tree recursion** via Subgraph effect - children spawned with `spawnSelf`, awaited with `awaitAny`
4. **Two TDD nodes** (not modal) - TDDWriteTests and TDDReviewImpl are separate graph nodes
5. **Self-loop retry** via `Goto Self` with `dispatchGotoWithSelf` - max 5 attempts tracked in input
6. **oneOf schemas** for sum type outputs - compile-time validated
7. **Parent-as-judge** - parent's TDDReviewImpl evaluates children (wrote the spec, has context)
8. **Envelope pattern** for fast routing with lazy payload loading

### DSL Implementation Checklist

```haskell
data V3Graph mode = V3Graph
  { entry       :: mode :- Entry ScaffoldInput
  , scaffold    :: mode :- LLMNode :@ Input ScaffoldInput :@ Schema ScaffoldExit :@ ...
  , fork        :: mode :- ForkNode :@ Input InitWorkPayload :@ Spawn '[...] :@ Barrier "implBarrier"
  , tddWriteTests :: mode :- LLMNode :@ Input TDDWriteTestsInput :@ Schema TDDWriteTestsExit :@ Memory TDDMem
  , implBarrier :: mode :- BarrierNode :@ Awaits '[TestsReadyPayload, ...] :@ UsesEffects '[Goto "impl" ImplInput]
  , impl        :: mode :- LLMNode :@ Input ImplInput :@ Schema ImplExit :@ Memory ImplMem :@ UsesEffects '[Goto Self ...]
  , tddReviewImpl :: mode :- LLMNode :@ Input TDDReviewImplInput :@ Schema TDDReviewImplExit :@ Memory TDDMem
  , merger      :: mode :- LLMNode :@ Input MergerInput :@ Schema MergerExit
  , rebaser     :: mode :- LLMNode :@ Input RebaserInput :@ Schema RebaserExit
  , exit        :: mode :- Exit MergeComplete
  }
  deriving Generic
```

### Deferred (MVP cuts)

- **Priority mailbox** - FIFO for now, add priority ordering at scale
- **Separate Judge node** - Parent-as-judge sufficient for MVP
- **Sibling event subscription** - Rebaser triggering mechanism (outside core graph)
