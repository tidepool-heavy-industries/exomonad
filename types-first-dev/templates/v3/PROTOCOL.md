# V3 Protocol

Three-phase TDD with structured handoff.

## Flow

```
spec.yaml
    │
    ▼
┌─────────┐     TaskManifest      ┌─────────────┐
│ SCAFFOLD ├─────────────────────▶│ IMPLEMENT   │
└─────────┘                       └──────┬──────┘
    │                                    │
    │ spawns facets                      │
    ▼                                    ▼
┌─────────┐                       ┌─────────────┐
│ SCAFFOLD │ (child)              │   REVIEW    │
└─────────┘                       └─────────────┘
    │                                    │
    ▼                                    ▼
┌─────────────┐                   APPROVE / REJECT
│ IMPLEMENT   │ (child)
└─────────────┘
    │
    ▼
  merge back to parent
```

## TaskManifest Schema

Passed between phases. Machine-parseable handoff, not prose.

```yaml
TaskManifest:
  meta:
    taskId: string          # Unique task identifier
    phase: SCAFFOLD | IMPLEMENT | REVIEW
    state: PENDING | IN_PROGRESS | COMPLETED | FAILED | BLOCKED

  git:
    baseCommit: sha
    branch: string
    targetPath: string
    testPath: string

  contract:
    # Immutable after scaffold phase
    interfaceFiles: [path]
    testFiles: [path]

  constraints:
    requiresNewDeps: bool
    maxCyclomaticComplexity: int (optional)
    readOnlyPaths: [glob]    # Paths this agent cannot modify

  facets:
    # Populated by scaffold, consumed by parent's implement phase
    - name: string
      taskId: string         # Child task's ID
      state: PENDING | COMPLETED | FAILED
      commitHash: sha (optional)

  diagnostics:
    # Populated on failure
    failureType: COMPILATION | TEST_ASSERTION | TIMEOUT | SPEC_FLAW
    specChallenge: {...}     # If SPEC_FLAW
    lastError: string
    iterations: int
```

## Phase Responsibilities

### Scaffold
- Writes interface files (immutable after this phase)
- Writes contract tests (test public API, not internals)
- Identifies facets for parallel decomposition
- Commits scaffold

### Implement
- Fills in `undefined` bodies
- Cannot modify interface or test files
- Iterates until tests pass
- On persistent failure: classifies as skill issue vs spec flaw
- Spec flaws escalate back to scaffold

### Review
- Phase 1: Mechanical auto-reject filters
- Phase 2: Semantic review (only if Phase 1 passes)
- Verdict: APPROVE | REJECT | REQUEST_CHANGES

## Invariants

1. **Interface Immutability**: Once scaffold commits Types.hs, no agent can modify it
2. **Test Immutability**: Implementer cannot change test logic to make them pass
3. **Parallel Safety**: Child facets only share interface files, never impl files
4. **Failure Classification**: Implementer must distinguish skill issue from spec flaw
5. **Mechanical First**: Review checks formatting/deps before wasting tokens on semantics

## Spec Challenge Protocol

When implementer detects impossible spec:

```yaml
failure:
  type: SPEC_FLAW
  specChallenge:
    testName: "prop_sortInConstantTime"
    reason: "Comparison sort requires O(n log n) minimum"
    suggestion: "Change to O(n log n) or use radix sort constraint"
```

This triggers re-activation of scaffold agent to revise the contract.
