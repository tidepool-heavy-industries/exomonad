# Phase 1: End-to-End Hylo Loop

Get a working recursive decomposition → implementation → PR fold loop with two levels. jj-first (ADR-004).

## What Already Exists

| Component | Status |
|-----------|--------|
| `spawn_subtree` (Claude, worktree + tab) | Done (git-based, needs jj migration) |
| `spawn_workers` (Gemini, batch panes) | Done (needs worktree isolation) |
| Session forking (`--resume --fork-session`) | Done |
| `file_pr` (auto-detect parent branch) | Done |
| `note` / `question` / `answer_question` | Done (messaging system) |
| `wait_for_event` / `notify_parent` | Done (EventQueue) |
| Zellij STDIN injection effect | Done |
| Stop hooks (check uncommitted, PR status) | Done (rebase/uncommitted checks eliminated by jj) |
| Copilot review polling (`wait_for_copilot_review`) | Done |
| `merge_pr` tool | Not built |
| Event router (event → Zellij injection to parent) | Not built |
| GitHub poller (PR status → events) | Not built |
| Dormant parent pattern (events as synthetic user msgs) | Not built |
| jj workspace integration | Not built |
| jj colocated mode setup | Not built |

## Implementation Waves

### Wave 0: jj Foundation

Set up jj colocated mode as the VCS operations layer. Git worktrees remain the isolation layer (required for Claude Code `--fork-session` session discovery).

**Deliverables:**
1. **Colocated init** — `jj git init --colocate` in project root. `.jj` alongside `.git`. Verify GitHub Actions, `gh` CLI, Copilot all still work.
2. **`.jjconfig.toml`** — Non-interactive pager, snapshot conflict markers, agent bookmark prefix.
3. **jj effects** — New effect namespace `jj.*` in Haskell WASM + Rust handler. Core operations:
   - `jj.bookmark_create` — create bookmark at revision
   - `jj.git_push` — push bookmark to remote
   - `jj.git_fetch` — fetch from remote (triggers auto-rebase)
   - `jj.log` — query revsets, return JSON
   - `jj.new` — start a new change
   - `jj.status` — working copy status including conflicts
4. **Keep `git worktree add` for isolation** — `spawn_subtree` still uses `git worktree add` (gives `.git` file for Claude Code cross-worktree `--resume`). Agent uses `jj` commands within the worktree for all VCS operations.
5. **Migrate workers to worktrees** — Each worker gets a git worktree (not shared parent directory). Sparse patterns restrict to relevant directories.
6. **Remove git stop-hooks** — Delete rebase check and uncommitted changes check. jj handles both natively (auto-rebase, working copy = commit).

**Files:**
- `proto/effects/jj.proto` + vendored copy — new proto for jj effects
- `haskell/wasm-guest/src/ExoMonad/Guest/Effects/Jj.hs` — jj effect types
- `haskell/wasm-guest/src/ExoMonad/Effects/Jj.hs` — effect runner (yield_effect)
- `rust/exomonad-core/src/handlers/jj.rs` — jj effect handler (subprocess calls)
- `.jjconfig.toml` — repo configuration
- `.exomonad/lib/StopHook.hs` — remove rebase/uncommitted checks

**Key constraint:** `git worktree add` for isolation, `jj` for operations. The two coexist via colocated mode — jj reads/writes through the `.git` backend that git worktrees share.

**Verification:**
1. `jj git init --colocate` succeeds, `gh pr list` still works
2. `spawn_subtree` creates git worktree, agent uses `jj` commands within it
3. `jj log -r 'visible_heads()'` from main repo shows agent's work across all worktrees
4. `claude --resume <parent-id> --fork-session` works from child worktree (`.git` file enables session discovery)
5. GitHub Actions CI still triggers on push

### Wave 1: Event Router (enables dormant parents)

The keystone piece. Without this, parents must poll.

**Deliverables:**
1. **Pane registry** — Server stores `(session_id → zellij_pane_id)` mapping. Populated at spawn time, and at init time for root TL.
2. **Event templates** — Haskell functions rendering structured events to natural language. Template per event type (child_complete, question, pr_ready, copilot_review, ci_status).
3. **Injection endpoint** — Server route that accepts an event, resolves target pane, renders template, calls `zellij action write-chars` + Enter.
4. **Hook wiring** — Child SessionEnd hook calls the injection endpoint.

**Files:**
- `haskell/wasm-guest/src/ExoMonad/Guest/Tools/Events.hs` — event template rendering
- `rust/exomonad-core/src/services/event_router.rs` — pane registry + injection logic
- `rust/exomonad-core/src/handlers/events.rs` — wire notify_event to injection
- `rust/exomonad/src/serve.rs` — injection HTTP endpoint

**Verification:** Spawn a subtree, have it call `notify_parent`. Parent's Zellij pane receives a natural language message. Parent Claude responds to it.

### Wave 2: `merge_pr` Tool + jj Post-Merge Flow

**Deliverables:**
1. **`merge_pr` MCP tool** — Haskell tool definition + GitHub effect. Calls `gh pr merge`. Then runs `jj git fetch` to pull merged state (triggers auto-rebase of descendants).
2. **Sibling `update-stale`** — After parent merges and fetches, siblings detect staleness on their next jj operation. No explicit notification needed — but parent can optionally inject a "[REBASE AVAILABLE]" message.

**Files:**
- `haskell/wasm-guest/src/ExoMonad/Guest/Tools/PR.hs` — merge_pr tool
- `proto/effects/github.proto` — MergePullRequest message

**Verification:** Parent merges child PR via `merge_pr`. Parent runs `jj git fetch`. `jj log -r 'conflicts()'` shows any resulting conflicts (or none). Sibling workspace detects staleness.

### Wave 3: GitHub Poller + Copilot Feedback Loop

**Deliverables:**
1. **GitHub PR poller** — Background task in MCP server. Tracks PRs filed by agents. Polls for status changes (Copilot review, CI). Generates events routed via Wave 1 infrastructure.
2. **Child Copilot iteration** — When a child receives a Copilot review event (injected into its pane), it addresses the comments, pushes (`jj git push`), and waits for the next review.

**Files:**
- `rust/exomonad-core/src/services/github_poller.rs` — polling loop + PR tracking
- `rust/exomonad/src/serve.rs` — start poller as background task
- Event templates for copilot_review and ci_status events

**Verification:** Child files PR. Copilot posts review. Child receives review as synthetic message, addresses comments, pushes. Repeat until green.

## End-to-End Verification Scenario

```
1. Human gives root TL a multi-component task
2. Root writes spec commit (types, tests, ADR) — jj auto-commits working copy
3. Root creates bookmarks, spawns 2 subtrees (jj workspaces, session fork)
4. Subtrees each spawn 2 workers (jj workspaces with sparse patterns)
5. Workers implement, create bookmarks, push, file PRs
6. Workers iterate with Copilot autonomously
7. Workers go dormant when PRs are green
8. Subtree parents receive "[PR READY]" events
9. Subtrees review and merge worker PRs (gh pr merge + jj git fetch)
10. jj auto-rebases remaining siblings (conflict-free or conflicts-as-data)
11. Subtrees file PRs against root bookmark
12. Root reviews and merges subtree PRs
13. Root files final PR against main
14. Human reviews on GitHub
```

## Non-Goals (Phase 1)

- GitHub webhooks (polling is fine for v1)
- Cloud-hosted MCP server
- Automatic depth adjustment (human approves depth > 2)
- Researcher/explorer node types
- Sleeptime evolution integration
- Semantic stream detection (see `next_wave/`)
