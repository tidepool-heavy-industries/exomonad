# Hylo Phase 1: Code Audit Guide

You are auditing the spawn/cleanup/PR pipeline for correctness. This is the critical path for recursive agent decomposition. Every bug here breaks the entire hylo model.

## What You're Auditing

The hylo model: a TL agent decomposes work by spawning subtree agents (worktree + branch + Zellij tab). Each subtree can spawn further subtrees or workers. When done, agents file PRs against their parent's branch. Parents merge child PRs, then file their own PR upward. The tree folds via PR merges.

## Critical Code Paths

### 1. `spawn_gemini_teammate` — The Core Spawn Path

**File:** `rust/exomonad-core/src/services/agent_control.rs`

This is the most important function. It creates an isolated agent with its own worktree, branch, and Zellij tab.

**What it should do, in order:**
1. Check Zellij environment is available
2. Resolve `effective_project_dir` (handles subrepo case)
3. Slugify the agent name for filesystem/git safety
4. Build `internal_name = "{slug}-{agent_suffix}"` (e.g. `auth-service-claude`)
5. Build `display_name = "{emoji} {slug}"` (e.g. `🤖 auth-service`)
6. Idempotency check: if Zellij tab with `display_name` already exists, return early
7. Determine base branch (explicit param or current branch via `git rev-parse`)
8. Create branch name: `"{base_branch}.{slug}"` — uses `.` separator
9. Create worktree path: `{effective_dir}/.exomonad/worktrees/{internal_name}`
10. Clean up existing worktree if present (idempotency)
11. `git worktree add -b {branch_name} {worktree_path} {base_branch}`
12. Write MCP config into the worktree (agent-type-aware)
13. For Gemini: set `GEMINI_CLI_SYSTEM_SETTINGS_PATH` env var
14. Open Zellij tab with cwd = worktree_path
15. Emit agent:started event
16. Return SpawnResult

**Things to audit:**
- Does the branch name format (`base.slug`) match what `file_pr` expects? The `detect_base_branch` function in `file_pr.rs` uses `rfind('.')` to strip the last segment. Verify this works for all depths.
- What happens if `git worktree add` fails? Is the error propagated cleanly?
- What happens if the base branch doesn't exist? (e.g. parent already merged and deleted)
- Is the `internal_name` used consistently everywhere it needs to be? It appears in: worktree path, config dir path, tmp dir path, Zellij tab matching, cleanup reconstruction. Any mismatch = cleanup can't find the agent.
- Does the idempotency check (`is_zellij_tab_alive`) match by `display_name`? But cleanup matches by `identifier` (the slug without emoji). Are these consistent?
- The `display_name` is NOT truncated (GEMINI_NOTES #8 fixed this). Verify no truncation crept back in.

### 2. `write_agent_mcp_config` — Agent Config Writing

**File:** `rust/exomonad-core/src/services/agent_control.rs`

Writes MCP server config so the spawned agent can talk to the exomonad server.

**What it should do:**
- Claude agents: write `.mcp.json` in worktree root with `"url"` field
- Gemini agents: write `.gemini/settings.json` in worktree with `"httpUrl"` field
- Both use per-agent endpoint: `http://localhost:{port}/agents/{agent_name}/mcp`
- Port discovered from `.exomonad/server.pid` file via `read_server_port()`

**Things to audit:**
- `agent_name` is extracted from `agent_dir.file_name()`. This is `internal_name` (e.g. `auth-service-claude`). Verify this matches what the server expects in its `/agents/{name}/mcp` route.
- What happens if `.exomonad/server.pid` doesn't exist or has stale content?
- Claude's `.mcp.json` — does Claude Code actually read this? Is the format correct? (Claude Code docs say `.mcp.json` at project root is read automatically.)
- Gemini's `.gemini/settings.json` — the `GEMINI_CLI_SYSTEM_SETTINGS_PATH` env var must point to this exact file. Verify the env var is set in the spawn call.
- Are there any race conditions? Two agents spawned simultaneously could both try to read the same server.pid.

### 3. `spawn_worker` — Lightweight In-Place Spawn

**File:** `rust/exomonad-core/src/services/agent_control.rs`

Spawns a Gemini agent as a Zellij pane in the parent's worktree. No branch, no worktree.

**What it should do:**
1. Check Zellij env
2. Require MCP server port (from `self.mcp_server_port`, not filesystem)
3. Slugify name, build `internal_name = "{slug}-gemini"`
4. Idempotency check (same as subtree)
5. Write settings to `/tmp/exomonad-agents/{internal_name}/settings.json`
6. Set `EXOMONAD_AGENT_ID` and `GEMINI_CLI_SYSTEM_SETTINGS_PATH` env vars
7. Open Zellij pane (not tab) with cwd = project_dir
8. Emit agent:started event

**Things to audit:**
- Workers use `self.mcp_server_port` while subtrees use `read_server_port()` from filesystem. Why the difference? Is `self.mcp_server_port` always set when spawn_worker is called?
- Workers write to `/tmp/exomonad-agents/`. This is a global namespace. Two different projects running simultaneously could collide if agents have the same name. Is this a real risk?
- Workers are NOT registered in any persistent tracking (no config.json entry). `list_agents` won't find them. `cleanup_agent` can only clean them if it knows the exact identifier. Is cleanup of workers even possible currently?
- The idempotency check uses `is_zellij_tab_alive` but workers are panes, not tabs. Does this function also check panes? If not, spawning the same worker twice creates duplicates.

### 4. `cleanup_agent` — The Cleanup Path

**File:** `rust/exomonad-core/src/services/agent_control.rs`

Cleans up everything a spawned agent created.

**What it should do:**
1. Find agent in `list_agents()` by identifier
2. Reconstruct `internal_name = "{identifier}-{agent_type_suffix}"`
3. Close Zellij tab (matching by identifier or issue_id in tab name)
4. Remove `/tmp/exomonad-agents/{internal_name}/` (worker temp dir)
5. Remove `.exomonad/agents/{internal_name}/` (per-agent config)
6. Remove git worktree at `.exomonad/worktrees/{internal_name}`
7. Emit agent:stopped event

**Things to audit:**
- The agent type suffix is derived from `agent.agent_type.as_deref().unwrap_or("gemini")`. If `agent_type` is None (e.g. old agents, parsing failure), it defaults to "gemini". Is this safe? Could it reconstruct the wrong `internal_name`?
- `list_agents()` — what does this actually return? It seems to scan Zellij tabs via `parse_agent_tab`. Workers run as panes, not tabs. So workers can't be found by `list_agents`, and `cleanup_agent` will fail with "No agent found for identifier". Confirm this limitation.
- The Zellij tab close loop iterates `get_zellij_tabs()` and matches by `tab.contains(identifier)`. This is substring matching — could it match the wrong tab? E.g. identifier "auth" would match tab "🤖 auth-service" AND "🤖 oauth-handler".
- What happens if the worktree has uncommitted changes? `git worktree remove --force` is used. Does this lose work?
- Cleanup is non-fatal for individual steps (tmp dir, config dir, worktree each have their own error handling). But if the Zellij tab close fails, does it still proceed to clean up files? It should.

### 5. `detect_base_branch` — PR Target Resolution

**File:** `rust/exomonad-core/src/services/file_pr.rs`

Determines which branch a PR should target.

**Logic:**
1. If explicit base_branch provided and non-empty, use it
2. If branch name contains `/`, strip last `/` segment (git convention)
3. If branch name contains `.`, strip last `.` segment (exomonad convention)
4. Otherwise, default to `"main"`

**Things to audit:**
- Priority: `/` is checked before `.`. A branch like `feature/main.subtask` would resolve to `feature` (stripping at `/`), not `feature/main` (stripping at `.`). Is this correct? ExoMonad uses `.` for hierarchy, but git-flow uses `/`. Could they conflict?
- What if the base branch was deleted? (Parent merged into grandparent, cleaned up.) `file_pr` would try to create a PR against a nonexistent branch. How does `gh pr create` handle this?
- Edge case: branch name is just `main`. No `/` or `.`. Returns `"main"`. But `gh pr create --base main` when you're already on main is a no-op/error. Is this handled?

### 6. `file_pr` — PR Creation/Update

**File:** `rust/exomonad-core/src/services/file_pr.rs`

**What it should do:**
1. Get current branch and remote URL
2. Parse owner/repo from remote URL
3. Check if PR already exists for this branch
4. If exists: return existing PR info (idempotent)
5. If not: detect base branch, create PR via GitHub API

**Things to audit:**
- How does it check for existing PRs? Does it query by head branch? Could there be multiple PRs for the same branch (e.g. one closed, one open)?
- The GitHub API call — is it using `gh` CLI or direct API? Check auth handling.
- What if the branch hasn't been pushed yet? `gh pr create` would fail. Is there a push step, or does it assume the caller pushed?

### 7. Stop Hook — Auto-file PR on Exit

**File:** `haskell/wasm-guest/src/ExoMonad/Guest/Tool/Runtime.hs` (or wherever SessionEnd is handled)

**What it should do:**
1. On SessionEnd, check if there are uncommitted changes → warn
2. Check if there are unpushed commits → warn
3. Check if a PR exists for the current branch → if not, auto-file one
4. Check Copilot review status on the PR

**Things to audit:**
- Does it actually call the `file_pr` effect? Or does it just check PR status?
- What if the agent is on `main`? Filing a PR from main to main is nonsensical.
- What if the branch has no commits ahead of the base? Empty PR.
- Error handling: if `file_pr` fails in the stop hook, does the agent still exit cleanly?

### 8. `parse_agent_tab` — Agent Discovery

**File:** `rust/exomonad-core/src/services/agent_control.rs`

Parses Zellij tab names to discover running agents.

**Format:** `"{emoji} {agent_name}"` where emoji is 🤖 (Claude) or 💎 (Gemini).

**Things to audit:**
- Topology detection: `gh-*` prefix → WorktreePerAgent, everything else → SharedDir. But subtree agents spawned via `spawn_gemini_teammate` don't use `gh-` prefix — they use slugified names like `auth-service`. So they get SharedDir topology. Is this correct? Subtrees DO have their own worktrees.
- This only finds tabs, not panes. Workers (panes) are invisible to discovery.

### 9. Messaging — note, question, answer_question, get_messages

**File:** `rust/exomonad-core/src/services/inbox.rs` (or similar), `rust/exomonad-core/src/handlers/messaging.rs`

**What it should do:**
- Messages are file-based (JSON in agent inbox directories)
- `get_messages` supports long-poll with timeout (blocks until message or timeout)
- Messages flow child → parent only
- `question` blocks the child until `answer_question` is called by parent

**Things to audit:**
- Where are inbox directories? Are they per-agent? Per-worktree?
- What happens to messages when an agent is cleaned up? Are they preserved for debugging?
- Long-poll implementation: is it using filesystem watching (inotify/kqueue) or polling loop? Polling loops waste CPU. Filesystem watching can miss events.
- Can messages be lost? (e.g. written between poll cycles, or during cleanup)
- Identity: how does the messaging system know which agent is sending? From `EXOMONAD_AGENT_ID` env var? From the MCP endpoint URL? Are these consistent?

### 10. Agent Handler — Effect Dispatch

**File:** `rust/exomonad-core/src/handlers/agent.rs`

Bridges WASM effects to service methods.

**Things to audit:**
- `spawn_worker` handler hardcodes Gemini agent type. Is this enforced in the Haskell tool schema too? (i.e. no `agent_type` param in the tool)
- `worker_result_to_proto` sets `topology: SharedDir`. Verify this is correct — workers share parent's dir.
- `teammate_result_to_proto` also sets `topology: SharedDir`. But subtrees have their own worktree. Should this be `WorktreePerAgent`?
- All proto conversion functions set `worktree_path: String::new()` or similar empty defaults. Is any downstream code relying on these fields being populated?

## Cross-Cutting Concerns

### Naming Consistency

The system uses multiple naming schemes that MUST stay synchronized:

| Context | Format | Example |
|---------|--------|---------|
| Internal name | `{slug}-{suffix}` | `auth-service-claude` |
| Display name | `{emoji} {slug}` | `🤖 auth-service` |
| Branch name | `{base}.{slug}` | `main.auth-service` |
| Worktree path | `.exomonad/worktrees/{internal_name}` | `.exomonad/worktrees/auth-service-claude` |
| Tmp config | `/tmp/exomonad-agents/{internal_name}` | `/tmp/exomonad-agents/auth-service-gemini` |
| MCP endpoint | `/agents/{internal_name}/mcp` | `/agents/auth-service-claude/mcp` |

If any of these drift (e.g. cleanup reconstructs a different `internal_name` than spawn created), things silently break.

**Key question:** `parse_agent_tab` extracts `issue_id` from the display name (strip emoji prefix). Cleanup uses `issue_id` + `agent_type` suffix to reconstruct `internal_name`. But the display name uses `slug` (no suffix), while internal_name uses `slug-suffix`. So: `display_name = "🤖 auth-service"` → `issue_id = "auth-service"` → `internal_name = "auth-service-claude"`. Verify this chain works for all agent types and name formats.

### Error Propagation

- Spawn failures should be loud and clear — the TL needs to know why a spawn failed
- Cleanup failures are mostly non-fatal (best effort) — but should still log
- `file_pr` failures in stop hooks — should warn but not block agent exit
- WASM effect errors should surface to the MCP caller, not be swallowed

### Concurrency

- Two agents spawned simultaneously with similar names
- `list_agents` called while a spawn is in progress
- `cleanup_agent` called while the agent is still writing files
- Long-poll `get_messages` holding a file handle while cleanup deletes the inbox

## What to Produce

For each issue you find:
1. **Severity** — Critical (data loss, silent failure), Medium (incorrect behavior, recoverable), Low (style, robustness)
2. **Location** — exact file and function
3. **Description** — what's wrong and why it matters
4. **Fix** — concrete code change (can be a description or a snippet)

Group findings by component. Prioritize things that would break during actual recursive agent usage (spawn subtree → subtree spawns subtree → file PRs → fold).
