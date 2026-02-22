# Hylomorphism Worktree Model

**Status:** Accepted

## Decision

The agent tree is a git worktree tree. Spawning agents unfolds the tree downward (creating worktrees and branches). Merging work folds it back up (PRs targeting parent branches, merged recursively to main).

Every PR targets its parent branch, not main. The tree collapses via recursive merge, not flat integration.

## Branching

Dot-separated naming encodes the tree structure:

```
main
 └── main.feature
      ├── main.feature.auth
      │    ├── main.feature.auth.middleware
      │    └── main.feature.auth.tests
      └── main.feature.api
```

The parent of `main.feature.auth.middleware` is `main.feature.auth`. PRs always target the parent branch. This is derived mechanically from the branch name — no configuration or metadata needed.

## Unfold Phase (Spawn)

The TL decomposes work and spawns children:

1. TL writes a spec commit (type stubs, interface definitions, failing tests)
2. TL calls `spawn_subtree` or `spawn_leaf_subtree`
3. Server creates a git worktree at `.exo/worktrees/{slug}/`
4. Server creates a new branch `{parent_branch}.{slug}`
5. Server creates a Zellij tab for the child agent
6. Child agent works in its isolated worktree

Each child gets its own worktree — full filesystem isolation. No file-level conflicts between concurrent agents.

### Why Worktrees

- **Isolation**: Each agent has its own working directory. No merge conflicts during development.
- **Cheap**: Git worktrees share the object store. Creating one is instant.
- **Familiar**: Standard git operations work. No custom VCS layer.
- **Identity**: The worktree's branch name IS the agent's identity (see agent-identity-model).

## Fold Phase (Merge)

Work flows back up the tree via PRs:

1. Child completes work, pushes to its branch, files PR against parent branch
2. Copilot reviews automatically; child iterates autonomously against review feedback
3. Child calls `notify_parent` with `status: success` when review-clean
4. Parent receives `[CHILD COMPLETE]` notification (via Zellij STDIN injection)
5. Parent merges the PR via `merge_pr` tool
6. If parent has more children, repeat; otherwise parent folds into ITS parent

### Merge Strategy

| Node Type | Strategy | Rationale |
|-----------|----------|-----------|
| Worker (Gemini leaf) | Squash | Single logical change, clean history |
| Subtree (Claude) | Merge commit | Preserves child PR history |
| Root → main | Squash | Clean main branch history |

### Recursive Collapse

The tree collapses bottom-up:
1. Leaves merge into their parent branches
2. Intermediate nodes merge into THEIR parent branches
3. The root branch merges into main

Each level of the tree is one PR. The merge cascade is not automated — each parent explicitly merges its children's PRs after reviewing for architectural fit.

## Implementation

- `spawn_subtree`: Creates worktree + Zellij tab for Claude agent (TL role, can spawn children). Depth-capped at 2.
- `spawn_leaf_subtree`: Creates worktree + Zellij tab for Gemini agent (dev role, files PR).
- `spawn_workers`: Creates Gemini panes in parent directory (ephemeral, no worktree, no branch).
- `file_pr`: Creates PR with auto-detected base branch from dot-separated naming.
- `merge_pr`: Merges child PR (`gh pr merge` + `git fetch`).

## Consequences

- GitHub is the audit trail — every merge is a PR with review history
- Copilot handles mechanical review; parents only review for architectural fit
- Concurrent agents never conflict at the filesystem level
- Branch naming is the entire coordination mechanism — no registry, no database
- Worktree cleanup is manual (not yet automated)
- Depth cap of 2 prevents unbounded tree growth

## Why Not Alternatives

**Shared directory (no worktrees)**: File-level conflicts between concurrent agents. Requires task-level coordination to avoid touching the same files.

**Docker containers**: Solves isolation but adds heavyweight infrastructure. Git worktrees achieve the same isolation with zero overhead.

**jj (Jujutsu)**: Previously used for automatic rebase cascade. Replaced with plain git worktrees — simpler, fewer edge cases with colocated mode, better tool ecosystem compatibility.

**Flat PRs to main**: Loses the tree structure. With nested PRs, each parent can review its children's work in context before folding up. Flat PRs would require the root TL to review everything.
