# ADR-002: Fold Protocol (PR Lifecycle, Merge, Rebase)

## Status

Accepted

## Context

The fold phase of the hylomorphism is children merging work back into the parent branch via PRs. This involves: child filing PRs, Copilot reviewing, child iterating, parent merging, and siblings rebasing.

## Decision

### Branch Topology

```
main
 └── main.feature                    (root subtree)
      ├── main.feature.auth          (child subtree)
      │    ├── main.feature.auth.middleware  (leaf worker)
      │    └── main.feature.auth.tests      (leaf worker)
      └── main.feature.api           (child subtree)
           └── main.feature.api.endpoints   (leaf worker)
```

PRs always target the parent branch. Branch naming uses `.` as hierarchy separator. The branch name encodes the full path in the task tree.

### Child Completion Flow

```
1. Child completes implementation work
2. Child commits, pushes, files PR against parent branch (via file_pr tool)
3. GitHub triggers Copilot review + CI automatically
4. Child receives Copilot feedback (via GitHub poll → Zellij injection)
5. Child iterates autonomously:
   a. Addresses Copilot comments
   b. Pushes
   c. Waits for next review
   d. Repeat until Copilot approves + CI passes
6. Child goes dormant (PR is green, waiting for parent to merge)
7. Parent receives "[PR READY]" event
8. Parent reviews (architectural fit, integration with siblings):
   a. Approve → merge (see Merge Protocol below)
   b. Request changes → inject feedback into child's pane → child wakes and iterates
9. Parent merges PR
```

### Merge Protocol

Parent performs the merge, not the child. Parent has context about:
- What other children are doing
- Whether this child's work fits the decomposition intent
- Whether to merge now or wait for sibling dependencies

After merge:
1. Parent runs `gh pr merge <number> --squash` (or `--merge` for subtrees with history)
2. Siblings detect parent branch movement on their next stop-hook check
3. Parent checks: all children merged? → file own PR upward
4. Or: spawn additional children if new work is discovered

### Rebase Cascade

When child A's PR merges into the parent branch, siblings B and C are now behind.

**Detection:** Child stop-hook (runs at natural stopping points — session end, before exit). The hook:
1. Fetches parent branch: `git fetch origin {parent_branch}`
2. Checks if parent is ahead: `git rev-list HEAD..origin/{parent_branch} --count`
3. If ahead: performs `git rebase origin/{parent_branch}`
4. If conflict: child resolves autonomously using its code context. If unresolvable, sends question to parent.

**Why stop-hook, not continuous:** Children are actively working. Interrupting mid-task for a rebase is disruptive and may conflict with in-progress changes. Stop-hook is a natural breakpoint.

### Merge Strategy by Node Type

| Node Type | Merge Strategy | Rationale |
|-----------|---------------|-----------|
| Worker (Gemini leaf) | Squash merge | Single logical change, clean history |
| Subtree (Claude) | Merge commit | Preserves child PR history for debugging |
| Root → main | Squash merge | Clean main branch history |

### Review Token Economics

| Reviewer | Cost | Catches |
|----------|------|---------|
| CI | Infrastructure only | Compilation, test failures |
| Copilot | Free (included in GitHub) | Style, conventions, obvious bugs, security |
| Parent node | LLM tokens | Architectural fit, decomposition intent, integration |
| Human | Attention | Final quality gate on root PR, can inspect any level |

Parent only burns tokens on PRs that clear the mechanical bar. Most child ↔ Copilot iteration is zero marginal cost.

## Failure Modes

**Child PR fails CI repeatedly.** After N Copilot iterations (configurable, default 5), child sends "[STALLED]" event to parent. Parent can: send guidance, kill and respawn with different approach, or absorb the work.

**Child goes silent.** If no events from a child for configurable timeout (default 30m), parent gets a "[SILENT]" warning event. Can inspect child's tab directly or send a ping.

**Merge conflict between siblings.** Indicates overlapping decomposition — a parent planning error. Parent arbitrates: may merge one sibling first, then rebase the other, providing guidance on conflict resolution.

**Decomposition was wrong.** Parent realizes mid-fold that the split doesn't work. Can kill remaining children, re-decompose, spawn new subtrees. The fold is not committed until the parent merges upward.

## Consequences

**Positive:**
- GitHub is the audit trail (PRs, reviews, CI checks)
- Copilot handles the mechanical review loop for free
- Parents only wake for high-value decisions (architectural review, merge)
- Standard git workflow — nothing proprietary in the merge mechanics

**Negative:**
- Squash merges lose per-commit history within leaves
- Stop-hook rebase means siblings may work on stale code briefly
- GitHub API polling adds latency vs. webhooks (acceptable for v1)
