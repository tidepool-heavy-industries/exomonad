# ADR-002: Fold Protocol (PR Lifecycle, Merge, Rebase)

## Status

Accepted (Updated: jj-first per ADR-004)

## Context

The fold phase of the hylomorphism is children merging work back into the parent branch via PRs. This involves: child filing PRs, Copilot reviewing, child iterating, parent merging, and siblings rebasing.

With jj (ADR-004), the rebase cascade and conflict handling become VCS primitives rather than orchestration logic we build.

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

PRs always target the parent bookmark. Bookmark naming uses `.` as hierarchy separator. Each bookmark has a corresponding jj Change ID for durable tracking.

### Child Completion Flow

```
1. Child completes implementation work
2. Child creates bookmark, pushes, files PR against parent (via file_pr tool)
   jj bookmark create feature/auth -r @-
   jj git push --bookmark feature/auth
   gh pr create --head feature/auth --base main.feature
3. GitHub triggers Copilot review + CI automatically
4. Child receives Copilot feedback (via GitHub poll → Zellij injection)
5. Child iterates autonomously:
   a. Addresses Copilot comments
   b. Pushes (jj git push)
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
2. Parent runs `jj git fetch` to pull the merged state
3. **jj auto-rebases all descendant commits in the graph** (siblings included)
4. Sibling workspaces become "stale" — they sync on next `jj workspace update-stale`
5. Parent checks: all children merged? → file own PR upward
6. Or: spawn additional children if new work is discovered

### Rebase Cascade (jj-native)

When child A's PR merges into the parent bookmark, siblings B and C need to rebase.

**With jj, this is automatic.** When parent runs `jj git fetch` after merging A's PR:
1. jj detects the parent bookmark has moved
2. All descendant commits (B, C) are auto-rebased in the commit graph
3. If rebase causes conflicts, they're stored as data in the commit — **not a failure**
4. Sibling workspaces become "stale" but are not disrupted mid-work
5. Siblings call `jj workspace update-stale` at natural breakpoints to sync their working copy
6. If conflicts exist after sync, agent resolves them (conflict markers in files, not a blocked state)

**No stop-hook rebase check needed.** No fetch/rev-list/rebase logic. The VCS handles it.

### Conflict Handling

Conflicts are **data, not exceptions** in jj:

- A conflicted commit is a valid commit. The agent can keep working.
- Conflict markers use `snapshot` style (configured in `.jjconfig.toml`) — base, side A diff, side B snapshot. Highly parseable by LLMs.
- Detection: `jj log -r 'conflicts()'` returns all conflicted commits
- Resolution: agent edits files to resolve, commits normally
- If agent can't resolve: sends question to parent via messaging

### Merge Strategy by Node Type

| Node Type | Merge Strategy | Rationale |
|-----------|---------------|-----------|
| Worker (Gemini leaf) | Squash merge | Single logical change, clean history |
| Subtree (Claude) | Merge commit | Preserves child PR history for debugging |
| Root → main | Squash merge | Clean main branch history |

jj handles squash merges cleanly: `jj git fetch` auto-abandons local commits whose changes are incorporated into the squashed parent.

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

**Merge conflict between siblings.** jj stores the conflict as data. The sibling resolves it autonomously — it has full context of its own code. If the conflict is in code it doesn't own, it sends a question to parent. This is no longer a "parent planning error" — it's a normal event handled by the VCS.

**Decomposition was wrong.** Parent realizes mid-fold that the split doesn't work. Can kill remaining children, re-decompose, spawn new subtrees. The fold is not committed until the parent merges upward. jj's operation log allows full rollback if needed (`jj op restore`).

## Consequences

**Positive:**
- GitHub is the audit trail (PRs, reviews, CI checks)
- Copilot handles the mechanical review loop for free
- Parents only wake for high-value decisions (architectural review, merge)
- Rebase cascade is a VCS primitive, not orchestration code we maintain
- Conflicts never block the tree — always stored as data
- Full deterministic undo via jj operation log

**Negative:**
- Squash merges lose per-commit history within leaves
- GitHub API polling adds latency vs. webhooks (acceptable for v1)
- Agents may occasionally hallucinate git commands instead of jj (mitigated: operations wrapped in effects)
