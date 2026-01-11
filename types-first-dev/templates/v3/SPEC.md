# V3 Protocol Specification

5-node TDD graph with parallel spawn and tree coordination.

**Nodes:** Scaffold → TDD, Impl (parallel) → Merger → Rebaser

```
Entry(Spec)
    │
    ▼
┌──────────┐
│ Scaffold │
└──────────┘
    │
    ├──[parallel]── SpawnChildren ──▶ Child Graphs (isolated worktrees)
    │                                      │
    │                                      ▼
    │                                Child MRs merge into impl branch
    │
    ├──[parallel]── TDD (writes tests, reviews impl)
    │                    │
    │                    ▼
    │               Tests merge into impl branch
    │
    └──[waits]───── Impl (waits for children + tests, then implements)
                         │
    ┌────────────────────┘
    │
    ▼
┌────────────────────────────────────────────────────┐
│                    TDD LOOP                         │
│                                                     │
│  Impl ──TestsPassed──▶ TDD ──Approved──▶ Merger    │
│    ▲                    │                           │
│    └──MoreTests─────────┘                           │
│                                                     │
└────────────────────────────────────────────────────┘
                                │
                                ▼
                           ┌────────┐  MergeComplete   ┌──────────┐
                           │ Merger │─────────────────▶│ Rebaser  │
                           └────────┘                  └──────────┘
                                │                           │
                                ▼                           ▼
                           (MR to parent)           (siblings adapt)
```

**Key insight:** TDD and Impl nodes share parent's conversation history as starting context (threaded). Different templates, same backing thread.

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

**Key change:** No separate `SpawnTree` vs `InitLeaf`. Always `InitWork`, optionally with children. Orchestrator spawns (children + TDD + Impl) in parallel.

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

## Node 2: TDD

TDD node writes tests AND reviews impl. Spawned in parallel with Impl, shares parent's conversation context.

**Dual-phase invocation:** TDD is invoked TWICE per work item:
1. **First spawn:** WriteTests mode (writes failing tests, emits TestsReady)
2. **Second spawn:** ReviewImpl mode (after Impl emits TestsPassed)

Same node definition, different mode in input. Orchestrator manages phase transitions.

### Input Type

```yaml
TDDInput:
  spec: Spec
  scaffold: InitWorkPayload  # see Shared Types
  mode: TDDMode

TDDMode:
  oneOf:
    - WriteTests:
        # Initial mode - write failing tests
    - ReviewImpl:
        # After Impl claims TestsPassed
        implResult: ImplResult  # see Shared Types
        diff: Text
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
TDDExit:
  oneOf:
    # WriteTests mode outputs
    - TestsReady:
        testsCommit: Text
        testFiles: [FilePath]
        pendingCriteria: [Text]  # criteria with tests now waiting
    - InvalidScaffold:
        missingType: Text
        expectedLocation: FilePath

    # ReviewImpl mode outputs
    - Approved:
        signOff: Text
        coverageReport: CoverageReport
    - MoreTests:
        critiques: [Critique]
        additionalTests: [PlannedTest]  # new tests to write
    - Reject:
        reason: Text
        missingCriteria: [Text]
```

### Template

```jinja
# TDD: {{ spec.description }}

{% if mode.WriteTests %}
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

- **TestsReady**: Tests committed, waiting for Impl
- **InvalidScaffold**: Types missing → routes back to Scaffold

{% elif mode.ReviewImpl %}
You are the **Reviewer**. Impl claims tests pass. Verify and decide.

## Impl Result

Commit: `{{ mode.ReviewImpl.implResult.commitHash }}`
Iterations: {{ mode.ReviewImpl.implResult.iterations }}

**Tests passed:**
{% for testName in mode.ReviewImpl.implResult.passedTests %}
- `{{ testName }}`
{% endfor %}

## Diff

```diff
{{ mode.ReviewImpl.diff }}
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
- **MoreTests**: Need additional coverage → write more tests, loop
- **Reject**: Fundamental problems → escalate

{% endif %}
```

---

## Node 3: Impl

Impl node makes tests pass. Spawned in parallel with TDD, waits for:
1. Child MRs to merge (if any children)
2. TDD's TestsReady

Shares parent's conversation context.

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

- **TestsPassed**: All green → routes to TDD for review
- **RequestRetry**: Some failing, have strategy → self-loop (max 5)
- **BlockedDependency**: Need missing code → routes to Scaffold
- **SpecAmbiguity**: Unclear requirement → routes to Scaffold
- **Stuck**: Cannot proceed → human escalation
```

---

## Node 4: Merger

Files MR to parent after TDD approves.

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

## Node 5: Rebaser

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

### 1. Pause TDD Loop

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

- **RebaseClean**: Trivial rebase → resume TDD loop
- **RebaseAdapted**: Breaking changes absorbed → resume TDD loop
- **RebaseConflict**: Cannot resolve → escalate to Scaffold
```

---

## Edge Summary

### Parallel Spawn (from Scaffold.InitWork)

```
Scaffold ──InitWork──▶ Orchestrator spawns in parallel:
                        ├── Children (if childSpecs present)
                        ├── TDD (WriteTests mode)
                        └── Impl (waits for TDD + children)
```

### Node Edges

| From | Exit | To | Action |
|------|------|----|--------|
| Scaffold | InitWork | TDD, Impl, Children | Parallel spawn |
| Scaffold | ClarificationNeeded | Exit | Escalate to parent |
| TDD | TestsReady | Impl | Tests committed, impl can start |
| TDD | InvalidScaffold | Scaffold | Types missing |
| TDD | Approved | Merger | All good, file MR |
| TDD | MoreTests | TDD (WriteTests) | Write additional tests, loop |
| TDD | Reject | Exit | Fundamental problems, escalate |
| Impl | TestsPassed | TDD (ReviewImpl) | Claim tests pass, request review |
| Impl | RequestRetry | Impl | Self-loop (max 5) |
| Impl | BlockedDependency | Scaffold | Need missing code |
| Impl | SpecAmbiguity | Scaffold | Unclear requirement |
| Impl | Stuck | Exit | Human escalation |
| Merger | MergeComplete | Parent + Rebaser | Broadcast to parent and siblings |
| Merger | MergeRejected | Impl | Contract/build failure |
| Rebaser | RebaseClean | TDD | Resume with updated base |
| Rebaser | RebaseAdapted | TDD | Verify tests, resume |
| Rebaser | RebaseConflict | Scaffold | Cannot resolve |

### Wait Dependencies

| Node | Waits For |
|------|-----------|
| Impl | TDD.TestsReady + all Children.MergeComplete |
| Merger | TDD.Approved |
| Rebaser | Sibling.MergeComplete broadcast |

**Blocking behavior:** Impl session does NOT start until preconditions are met. Orchestrator holds Impl spawn until:
- TDD emits TestsReady (tests committed to impl branch)
- All children emit MergeComplete (if childSpecs present in InitWork)

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

**Where:** Parent's TDD node (ReviewImpl mode) acts as judge. When a child emits MergeComplete, the parent's TDD is invoked with the child's diff to verify `assignedCriteria` satisfaction.

When a child completes (MergeComplete), the parent evaluates:
- Does the merged code satisfy the criteria assigned to this child?
- Does it integrate correctly with sibling outputs?

No separate Judge node needed. The parent's TDD/Merge flow naturally includes semantic evaluation because it authored the acceptance criteria.

### When Parent Judges

| Event | Parent Action |
|-------|---------------|
| Child MergeComplete | Verify child satisfies `assignedCriteria` from `ParentContext` |
| All children complete | Run contract suite, verify integration |
| Contract failure | Route failing child back to its TDD |

### Why This Works

- Parent has spec context (wrote the criteria)
- Parent has interface context (defined boundaries)
- Parent has sibling context (sees all children's work)
- No fresh-context overhead (reuses parent's conversation)

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

1. **Threaded context** via Memory effect - both TDD and Impl resume from parent's conversation history
2. **Orchestrator-tracked** retry budget (LLM can't lie about attemptCount)
3. **Explicit Merger/Rebaser** nodes (not hidden in orchestrator)
4. **oneOf schemas** for sum type outputs (relaxed from Anthropic restriction)
5. **Parent-as-judge** - parent node evaluates children (wrote the spec, has context)
6. **Envelope pattern** for fast routing with lazy payload loading

### Deferred (MVP cuts)

- **Priority mailbox** - FIFO for now, add priority ordering at scale
- **Separate Judge node** - Parent-as-judge sufficient for MVP
