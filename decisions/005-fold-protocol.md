# ADR 005: Fold Protocol (PR Lifecycle + Merge Cascade)

**Status:** Accepted (design decided, partially implemented)

## Context

The fold phase of the hylomorphism is children merging work back into the parent branch via PRs. With jj colocated mode (ADR 007), rebase cascade and conflict handling become VCS primitives.

## Decision

### Branch Topology

PRs always target the parent bookmark. Bookmark naming uses `.` as hierarchy separator:

```
main
 └── main.feature
      ├── main.feature.auth
      │    ├── main.feature.auth.middleware
      │    └── main.feature.auth.tests
      └── main.feature.api
```

### Child Completion Flow

1. Child completes work, creates bookmark, pushes, files PR against parent
2. Copilot reviews automatically, child iterates autonomously
3. Child goes dormant when PR is green
4. Parent receives event, reviews for architectural fit
5. Parent merges PR
6. jj auto-rebases all descendant commits (siblings included)
7. Siblings sync on next operation — conflicts stored as data, not failures

### Merge Strategy

| Node Type | Strategy | Rationale |
|-----------|----------|-----------|
| Worker (Gemini leaf) | Squash | Single logical change, clean history |
| Subtree (Claude) | Merge commit | Preserves child PR history |
| Root → main | Squash | Clean main branch history |

## Implementation Status

- `file_pr` tool: implemented (auto-detects base branch from `.` naming)
- Stop hooks: implemented (PR status + Copilot review checks)
- `merge_pr` tool: not yet built
- jj auto-rebase cascade: works via colocated mode, not yet wired into spawn flow

## Consequences

- GitHub is the audit trail (PRs, reviews, CI checks)
- Copilot handles mechanical review for free; parents only review for architectural fit
- Rebase cascade is a VCS primitive (jj), not orchestration code we maintain
- Conflicts never block the tree — always stored as data
