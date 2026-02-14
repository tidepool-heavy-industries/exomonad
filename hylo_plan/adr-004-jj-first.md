# ADR-004: jj (Jujutsu) as Primary VCS

## Status

Accepted

## Context

The hylo tree requires frequent branching, parallel workspaces, automatic rebasing, and merge conflict handling across many concurrent agents. Git handles these as exceptional states requiring manual intervention. jj treats them as normal operations on an immutable commit graph.

We're building greenfield LLM-operated infrastructure. Agents don't have a "learning curve" — we control their prompts and wrap operations in effects. The question is purely: does jj's model match our coordination needs better than git's?

## Decision

**jj is the primary VCS. Colocated mode (`jj git init --colocate`) for GitHub compatibility.**

### Why jj

| Problem | Git | jj |
|---------|-----|-----|
| Agent crashes mid-work | Uncommitted changes lost | Working copy IS a commit. Always persisted. |
| Sibling rebase after merge | Stop-hook fetches parent, runs `git rebase`. Conflicts block. | Auto-rebase in commit graph. Conflicts stored as data, not failures. |
| Merge conflicts | Hard stop. Agent must resolve before continuing. | Conflicted commit is valid. Agent can keep working. Resolution is async. |
| Staging area mistakes | `git add` forgotten, partial commits, index lock contention | No staging area. No index. No lock contention. |
| Squash merge on GitHub | Divergent history errors, manual cleanup | Change IDs survive. jj auto-abandons local commits on fetch. |
| Querying tree state | Custom filesystem registry | `conflicts()`, `children(x)`, `visible_heads()` revsets |
| Crash recovery / debugging | `git reflog` (local, transient) | Immutable operation log. `jj op restore` for full repo rollback. |

### Colocated Mode

`jj git init --colocate` places `.jj` alongside `.git`. jj auto-exports state to `.git` on every operation. This means:

- GitHub Actions read `.git` normally
- `gh` CLI works unchanged
- Copilot reviews work unchanged
- GitHub PR workflow unchanged
- Existing CI pipelines unchanged

The only gotcha: git repo stays in detached HEAD state. We never run raw `git` write commands — all writes go through `jj`.

### Git Worktrees for Isolation (NOT jj Workspaces)

**We keep `git worktree add` for agent isolation. We do NOT use `jj workspace add`.**

Claude Code's `--resume --fork-session` depends on Git-aware session discovery:
1. Sessions are stored per-directory in `~/.claude/projects/<slugified-path>/`
2. `--resume` looks for a `.git` file/directory and aggregates sessions across all worktrees of the same repo
3. `--fork-session` reads from parent's storage, writes to child's path-encoded directory

`git worktree add` creates a `.git` FILE in the worktree pointing back to the main repo — this is what enables cross-worktree `--resume`. `jj workspace add` creates a `.jj` pointer but NO `.git` file, breaking session discovery.

Therefore:
- **`git worktree add`** = isolation layer (creates directory with `.git` file → Claude Code project identity)
- **`jj` colocated** = operations layer (auto-rebase, conflicts-as-data, revsets, within those worktrees)

jj sees the commits made via `git worktree add` and manages them through its colocated `.git` backend. The two coexist cleanly.

### Identity Model

**Dual identity: bookmarks for hierarchy, Change IDs for tracking.**

- **Bookmarks** (branch pointers): Encode tree position for PR targeting and human readability. `main.feature.auth` → PR targets `main.feature`.
- **Change IDs**: Durable agent identity. Survives rebases, squashes, amends. The orchestrator's database key for tracking agent state.

Bookmarks and the commit graph are global to the shared `.jj` store. All worktrees see changes immediately via jj's colocated git backend.

### Worktree Lifecycle

**Spawning an agent worktree:**
```bash
# Create git worktree (gives .git file for Claude Code session discovery)
git worktree add -b main.feature.auth ../worktrees/auth main.feature
cd ../worktrees/auth

# jj sees this worktree via colocated .git backend
# Agent uses jj commands for all VCS operations
jj new   # start a new change

# Optional: sparse checkout for workers (via git sparse-checkout or jj sparse)
```

**Cleanup:**
```bash
git worktree remove ../worktrees/auth
# jj commit history remains in the shared store. Full audit trail preserved.
```

### Auto-Rebase Behavior

When a parent bookmark moves (e.g., sibling's PR merged and `jj git fetch` runs):
1. All descendant commits are rebased **in the commit graph instantly**
2. Agent's worktree files on disk are NOT mutated mid-work
3. Next `jj status` or `jj log` in the worktree reflects the new graph state
4. If rebase caused conflicts, they appear as conflict markers in files
5. Agent can keep working — conflicts ride along as data

**This eliminates our entire stop-hook rebase check.** The VCS handles propagation automatically.

### GitHub PR Workflow

```bash
# Create bookmark for the PR
jj bookmark create feature/auth -r @-

# Push to GitHub
jj git push --bookmark feature/auth

# Create PR with standard gh CLI
gh pr create --head feature/auth --base main.feature
```

After squash merge on GitHub:
```bash
jj git fetch
# jj auto-abandons local commits, rebases children onto squashed parent
```

### Conflict Resolution

Conflicts are stored as data in commit objects. Resolution is async:

1. **Detection**: `jj log -r 'conflicts()'` finds all conflicted commits
2. **Inspection**: Agent reads conflict markers (snapshot style — base, side A diff, side B snapshot)
3. **Resolution**: Agent edits the file to resolve, commits
4. **Verification**: `jj log -r 'conflicts()' --no-pager` should return empty

Agents don't need to stop working to resolve conflicts. They can finish their current task, then address conflicts.

### Revsets for Orchestration

| Query | Revset |
|-------|--------|
| All active agent heads | `visible_heads() & ~::main` |
| All conflicted commits | `conflicts()` |
| Is workspace behind parent? | `main.feature ~ ::@` (non-empty = behind) |
| Commits between parent and child | `main.feature..main.feature.auth` |
| Specific workspace's commit | `agent-auth@` |

JSON output for machine consumption:
```bash
jj log -r 'visible_heads()' -T 'json(self)'
```

Returns: `change_id`, `commit_id`, `description`, `author`, `parents`, `is_conflicted`, `is_empty`.

### Registry Model

**Hybrid: filesystem for runtime metadata, revsets for VCS state.**

| Source | Owns |
|--------|------|
| `.exomonad/` (filesystem) | Pane IDs, MCP endpoints, agent type (Claude/Gemini), config |
| jj revsets | Conflicts, merge status, branch relationships, divergence, commit graph |

The filesystem answers "what agents exist and how do I reach them."
Revsets answer "what's the state of the code."

## Configuration

```toml
# .jjconfig.toml (per-repo)
[ui]
pager = "cat"                          # Non-interactive for agents
conflict-marker-style = "snapshot"     # Parseable by LLMs

[git]
push-bookmark-prefix = "agent/"        # Namespace agent branches
```

Linux tuning for many workspaces:
```bash
# /etc/sysctl.conf
fs.inotify.max_user_watches = 524288
```

## Consequences

**Positive:**
- Eliminates stop-hook rebase check (auto-rebase)
- Eliminates "uncommitted changes" failure mode (working copy = commit)
- Non-blocking fold phase (conflicts as data)
- Rich orchestration queries via revsets
- Full audit trail via immutable operation log
- GitHub compatibility preserved via colocated mode

**Negative:**
- Less LLM training data for jj commands (mitigated: we wrap in effects, agents rarely need raw jj)
- inotify limits need tuning for many workspaces on Linux
- No `jj git sync` command yet — orchestrator must handle fetch+rebase explicitly
- Colocated mode keeps git in detached HEAD (cosmetic, no functional impact)

## What This Eliminates

The following planned features are no longer needed:
- Stop-hook rebase check (ADR-002) — replaced by auto-rebase + `update-stale`
- Stop-hook uncommitted changes check — working copy is always a commit
- Filesystem-based divergence detection — replaced by revsets
- Manual merge conflict handling in fold protocol — conflicts are data, not failures
