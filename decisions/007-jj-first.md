# ADR 007: jj (Jujutsu) as Primary VCS

**Status:** Implemented

## Context

The hylo tree requires frequent branching, parallel workspaces, automatic rebasing, and merge conflict handling across many concurrent agents. Git handles these as exceptional states requiring manual intervention. jj treats them as normal operations on an immutable commit graph.

## Decision

**jj is the primary VCS. Colocated mode (`jj git init --colocate`) for GitHub compatibility.**

### Why jj

| Problem | Git | jj |
|---------|-----|-----|
| Agent crashes mid-work | Uncommitted changes lost | Working copy IS a commit |
| Sibling rebase after merge | Manual fetch + rebase | Auto-rebase in commit graph |
| Merge conflicts | Hard stop | Conflicted commit is valid, resolution is async |
| Staging area mistakes | Partial commits, index lock | No staging area, no index |
| Squash merge on GitHub | Divergent history errors | Change IDs survive, auto-abandon on fetch |
| Querying tree state | Custom registry | `conflicts()`, `visible_heads()` revsets |

### Colocated Mode

`jj git init --colocate` places `.jj` alongside `.git`. jj auto-exports state to `.git` on every operation. GitHub Actions, `gh` CLI, Copilot, CI all work unchanged.

### Git Worktrees for Isolation (NOT jj Workspaces)

We keep `git worktree add` for agent isolation. Claude Code's `--resume --fork-session` depends on Git-aware session discovery — it looks for a `.git` file/directory. `git worktree add` creates a `.git` FILE in the worktree enabling cross-worktree `--resume`. `jj workspace add` does not create this file.

Therefore:
- **`git worktree add`** = isolation layer (creates directory with `.git` file)
- **`jj` colocated** = operations layer (auto-rebase, conflicts-as-data, revsets)

### Identity Model

Dual identity: bookmarks for hierarchy (`main.feature.auth` → PR targets `main.feature`), Change IDs for durable tracking (survive rebases/squashes).

### What This Eliminated

- Stop-hook rebase check — replaced by auto-rebase
- Stop-hook uncommitted changes check — working copy is always a commit
- Filesystem-based divergence detection — replaced by revsets
- Manual merge conflict handling — conflicts are data, not failures

## Implementation Status

- jj colocated mode initialized in project root
- `.jjconfig.toml` configured (non-interactive pager, git diff format, auto-snapshot)
- `jj.*` effect namespace fully implemented: proto, Rust handler (JjHandler), Haskell effects
- Operations: `bookmark_create`, `git_push`, `git_fetch`, `log`, `new`, `status`
- Spawn flow still uses `git worktree add` for isolation (as designed)
- Agents not yet using jj commands for VCS operations within worktrees (next step)
