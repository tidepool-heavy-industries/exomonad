# Worktree Architecture Refactor

## Context

Parallel work-streams need compilation isolation. Agents editing Rust/Haskell in the same checkout break each other's builds. Git worktrees solve this — each agent gets its own checkout on its own branch. The current spawn infrastructure has the pieces (worktree creation, tab management, event coordination) but the identity model, directory layout, and handler separation need rework.

## Design Decisions (from review)

| Decision | Choice |
|----------|--------|
| Identity model | `(role, birth-branch)` for TLs, `(name, role, birth-branch)` for workers. TLs are nameless. |
| Session ID | Birth-branch (immutable, deterministic). Root TL = `"root"`. |
| Worktree location | Nested under `.exomonad/worktrees/`. Configurable base in `config.toml`. |
| Worker config location | Parent's `.exomonad/agents/{name}/` (not `/tmp/`) |
| Handler split | Separate Rust handlers + proto messages for subtree vs worker |
| Subtree agent type | Claude-only (to start) |
| Spawn fields | `task` only (collapse task + context) |
| Notify model | "Notify parent" — server resolves routing from caller identity. No explicit session_id. |
| Registry | Filesystem IS the registry. Scan `.exomonad/worktrees/` recursively to reconstruct tree. |
| Metadata | Derive everything (branch from git, agent type from config presence, role from config.local.toml) |
| Recursion depth | 2 levels max (TL → subtree → workers only, no sub-subtrees) |
| Server model | One server at project root, all agents connect via per-agent endpoints |
| `.exomonad` naming | Keep as-is for now |

## Directory Layout

```
project/
  .exomonad/
    config.toml              # worktree_base = ".exomonad/worktrees" (default)
    server.pid
    wasm/
    worktrees/
      feature-a/             # git worktree checkout (Claude TL, branch: main.feature-a)
        .exomonad/
          config.local.toml  # role = "tl"
          agents/
            rust-impl/       # worker config (Gemini pane, no own branch)
              settings.json  # Gemini MCP settings
            haskell-impl/    # worker config
              settings.json
        .mcp.json            # Claude MCP config (per-agent endpoint URL)
        src/                 # actual code checkout
        ...
      feature-b/             # another parallel subtree
        .exomonad/
          agents/
            ...
```

Workers live in their parent's `.exomonad/agents/`. Subtrees live in their parent's `.exomonad/worktrees/`. Depth-2 subtrees would nest further but are capped for now.

## Identity & Routing

### Session ID = birth-branch

```
Root TL (human):     session = "root"
  └─ subtree:        session = "main.feature-a"   (birth-branch)
       └─ worker:    routes to parent "main.feature-a"
  └─ subtree:        session = "main.feature-b"
       └─ worker:    routes to parent "main.feature-b"
```

### Notify parent (replaces notify_completion)

Worker calls `notify_parent(status, message)`. Server resolves:
1. Caller identity from URL path (`/agents/{name}/mcp`)
2. Parent session from caller's birth-branch (strip last segment, or lookup)
3. Routes event to parent's event queue

No `session_id` field needed in the tool. Chain of command enforced structurally.

## Changes

### Phase 1: Proto split

**`proto/effects/agent.proto`** (and vendored copy):

```protobuf
// NEW: dedicated subtree message
message SpawnSubtreeRequest {
  string task = 1;           // full prompt/instructions (collapsed task+context)
  string branch_name = 2;    // branch suffix
}

message SpawnSubtreeResponse {
  AgentInfo agent = 1;
}

// EXISTING: SpawnWorkerRequest stays as-is but worker config path changes
// SpawnGeminiTeammateRequest: DEPRECATE (replaced by SpawnSubtreeRequest)
```

Add `spawn_subtree` to the `AgentEffects` service alongside existing RPCs.

**Files:**
- `proto/effects/agent.proto`
- `rust/exomonad-proto/proto/effects/agent.proto` (vendored copy, must be identical)

### Phase 2: Rust handler split

**`rust/exomonad-core/src/services/agent_control.rs`**:

Split `spawn_gemini_teammate()` into:

1. `spawn_subtree(task, branch_name)` — Claude-only:
   - Creates worktree at `{worktree_base}/{slug}/`
   - Branch: `{current_branch}.{slug}`
   - Session ID (env var): birth-branch
   - Writes `.mcp.json` in worktree root
   - Creates Zellij tab
   - Depth check: if current agent's depth >= 2, reject

2. `spawn_worker(name, prompt)` — Gemini-only (unchanged logic but):
   - Config dir: `{project_dir}/.exomonad/agents/{slug}-gemini/` (not `/tmp/`)
   - Settings written there, pointed via `GEMINI_CLI_SYSTEM_SETTINGS_PATH`
   - Creates Zellij pane (unchanged)

**`rust/exomonad-core/src/handlers/agent.rs`**:
- Add `spawn_subtree` handler method
- Keep `spawn_worker` handler
- Deprecate `spawn_gemini_teammate` handler

### Phase 3: Notify parent

**`proto/effects/events.proto`**:

```protobuf
message NotifyParentRequest {
  string status = 1;    // "success" | "failure"
  string message = 2;   // completion message
}

message NotifyParentResponse {
  bool ack = 1;
}
```

**`rust/exomonad-core/src/handlers/events.rs`**:
- New `notify_parent` handler
- Resolves caller identity via task-local `get_agent_id()`
- Looks up parent session from agent registry (or derives from birth-branch)
- Creates `WorkerComplete` event, routes to parent's event queue

**`haskell/wasm-guest/src/ExoMonad/Guest/Tools/Events.hs`**:
- Replace `NotifyCompletion` tool (4 fields: session_id, worker_id, status, message)
- With `NotifyParent` tool (2 fields: status, message)
- Server fills in worker_id and session routing

### Phase 4: Haskell tool + effect updates

**`haskell/wasm-guest/src/ExoMonad/Guest/Tools/Spawn.hs`**:
- Update `SpawnSubtree` tool: remove `context` and `agent_type` fields, just `task` + `branch_name`
- Update effect call to use new `SpawnSubtreeRequest` proto
- `SpawnWorkers` stays mostly unchanged

**`haskell/wasm-guest/src/ExoMonad/Guest/Effects/AgentControl.hs`**:
- Split `SpawnSubtreeC` to use new proto message
- Keep `SpawnWorkerC` using existing `SpawnWorkerRequest`

### Phase 5: Config + discovery

**`config.toml`**:
```toml
worktree_base = ".exomonad/worktrees"  # default, configurable
```

**Agent discovery** (replaces `list_agents` Zellij-only scan):
- Scan `{worktree_base}/` for directories
- Each dir with a `.mcp.json` or `.exomonad/agents/` = active subtree
- Each entry in `.exomonad/agents/` = active worker
- Liveness: check if Zellij tab/pane exists (existing mechanism)

### Phase 6: Worker exit hook update

**`handleWorkerExit`** in `Runtime.hs`:
- Currently reads `session_id` from hook input and calls `Events.notifyEvent`
- Change to call `Events.notifyParent` instead (no session_id needed)
- Server resolves routing from `agent_id` in hook input

## Files Modified Summary

| File | Change |
|------|--------|
| `proto/effects/agent.proto` | Add `SpawnSubtreeRequest/Response`, keep `SpawnWorkerRequest` |
| `proto/effects/events.proto` | Add `NotifyParentRequest/Response` |
| `rust/exomonad-proto/proto/effects/agent.proto` | Vendored copy (identical) |
| `rust/exomonad-proto/proto/effects/events.proto` | Vendored copy (identical) |
| `rust/exomonad-core/src/services/agent_control.rs` | Split handlers, change worker config path, add depth check |
| `rust/exomonad-core/src/handlers/agent.rs` | Add `spawn_subtree` handler |
| `rust/exomonad-core/src/handlers/events.rs` | Add `notify_parent` handler |
| `haskell/wasm-guest/src/ExoMonad/Guest/Tools/Spawn.hs` | Simplify SpawnSubtree fields |
| `haskell/wasm-guest/src/ExoMonad/Guest/Tools/Events.hs` | Replace NotifyCompletion with NotifyParent |
| `haskell/wasm-guest/src/ExoMonad/Guest/Effects/AgentControl.hs` | Split effect constructors |
| `haskell/wasm-guest/src/ExoMonad/Effects/Events.hs` | Add notifyParent effect |

## NOT Changed

| File | Why |
|------|-----|
| Event queue (`event_queue.rs`) | Session-keyed queues unchanged. Birth-branch is just a different session ID value. |
| Messaging (`messaging.rs`, `inbox.rs`) | Orthogonal to spawn changes. |
| WASM runtime (`Runtime.hs`, `Continuations.hs`) | Unchanged trampoline. |
| Zellij plugin | Unchanged. |

## FIXME / Known Gaps (deferred)

- Subtree lifecycle: worktrees persist after agent exits. No auto-cleanup, no merge-then-clean flow yet.
- `list_agents` still can't see pane-based workers (Zellij tab scan only). Filesystem scan would fix this.
- Recursive depth > 2 deliberately capped. Revisit if needed.
- `.exomonad` → `.exo` rename deferred.
- Stop hook for subtree agents (as distinct from worker exit hook) not yet designed.

## Verification

1. `cargo test --workspace` — proto codegen, handler tests
2. `just wasm-all` — Haskell WASM compiles
3. Manual: `exomonad init` → spawn_subtree → verify worktree created at `.exomonad/worktrees/{slug}/`
4. Manual: spawn_workers inside subtree → verify config at `.exomonad/agents/{name}/`
5. Manual: worker exits → `notify_parent` routes to subtree TL's event queue
6. Manual: `wait_for_event` in subtree TL returns worker completion
