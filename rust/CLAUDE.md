# ExoMonad Rust Workspace

This workspace contains two coexisting architectures for augmenting Claude Code sessions.

1. **Classic Architecture** (`exomonad serve`): A central MCP server hosting Haskell WASM plugins. Designed for interactive, human-augmented sessions.
2. **Node-mode V2** (the `exo` binary: `exo init` / `exo node` / `exo hook`): A headless, serverless swarm where each agent has its own Rust sidecar. Designed for autonomous, distributed orchestration.

Both architectures share tmux isolation, git-worktree code isolation, the agent triad, and the scaffold-fork-converge lifecycle.

## Architecture: Classic

**100% WASM routing.** All tool/hook logic lives in Haskell WASM; Rust handles I/O only.

```
Claude Code (hook or MCP call)
       ↓
  exomonad (Rust)
       ↓
  PluginManager::call("handle_*", ...)
       ↓
  WASM guest (Haskell) ← PURE LOGIC ONLY
       ↓
  Yields effects (Git, GitHub, AgentControl, Log, etc.)
       ↓
  Rust host functions execute ALL I/O
       ↓
  Result marshalled back through WASM
```

### Key Components (Classic)

| Component | Purpose |
|-----------|---------|
| **exomonad** | Rust binary with WASM plugin support (hooks + MCP) |
| **exomonad-core** | Classic framework, handlers, services, UI protocol; depends on `exomonad-shared` |
| **exomonad-shared** | The lean seam both architectures share: `domain`, `protocol`, `error`/`util`/`ffi`/`hooks`/`logging`, `services::{tmux_ipc, resilience, agent_control}` (no classic link) |
| **exomonad-proto** | Proto-generated types (prost) for FFI + effects |

## Architecture: Node-mode (V2)

**Zero-link capability seam.** Tool/hook logic is plain Rust generic over the `exo-caps` trait boundary. No central server; filesystem as the bus; process tree as the topology.

| Component | Purpose |
|-----------|---------|
| [**exo-caps**](exo-caps/CLAUDE.md) | The capability seam: trait contract + validated domain types (no IO) |
| [**exo-runtime**](exo-runtime/CLAUDE.md) | IO impls of every cap on one `Runtime` (reuses `exomonad-shared` services; **never links classic core**) |
| [**exo-framework**](exo-framework/CLAUDE.md) | The reusable engine abstractions: `Tool`/`RoleDef`/hook contract + `PolicyCaps` + `RoleRegistry`, generic over caps; no concrete tools/roles/gates |
| [**exo**](exo/CLAUDE.md) | The domain (bin+lib): concrete tools / roles / gates + the `roster()` the binary injects; thin `main.rs` node entrypoint |
| [**exo-node**](exo-node/CLAUDE.md) | The per-node sidecar: outbound MCP + inbound inbox-watch + hook mode (resolves roles through an injected `RoleRegistry`, never depends on the domain) |
| [**exo-scry**](exo-scry/CLAUDE.md) | Derive a session's active team from live OS state (native Teams delivery) |
| **claude-teams-bridge** | Read/write messages through Claude Code's Teams filesystem |

### Deployment

**Local tmux-based orchestration:**

```
Human in tmux session
    └── Claude Code (main window, role=tl)
            ├── MCP server: exomonad mcp-stdio
            ├── WASM: loaded from .exo/wasm/ at runtime
            └── fork_wave / spawn_dev creates:
                ├── Window subtree-1 (Claude session-default, worktree off current branch, role=tl)
                ├── Window leaf-1 (Claude Sonnet, worktree off current branch, role=dev)
                ├── Pane worker-a (Claude Sonnet, in parent dir, ephemeral, role=worker)
                └── ... (recursive tree of worktrees + workers)
```

Each subtree agent (`spawn_subtree`):
- Runs in isolated git worktree at `.exo/worktrees/{slug}-{type}/`
- Branch naming: `{parent_branch}.{slug}-{type}` (dot separator, suffixed agent name)
- Gets `.mcp.json` with `{"type": "stdio", "command": "exomonad", "args": ["mcp-stdio", "--role", "tl", "--agent-id", "..."]}`
- Claude-only, gets TL role (can spawn workers, depth-capped at 2)
- Session ID = birth-branch (immutable, deterministic). Root TL = "root".
- PRs target parent branch, not main — merged via recursive fold
- Runs in tmux window with `claude 'task'` (positional arg), auto-closes on exit

Each leaf agent (`spawn_dev` with worktree isolation):
- Same worktree isolation as `spawn_subtree` (own branch, own directory)
- Sonnet Claude — dev role (no spawn tools)
- Runs in tmux window, commits and `submit_branch`es against parent branch

Each worker agent (`spawn_worker`, inline isolation):
- Runs in a tmux pane in the parent's directory (no branch, no worktree, ephemeral)
- Sonnet Claude — lightweight, focused execution
- `.mcp.json` + `.claude/settings.local.json` in the parent's worktree (CWD-discovered)

## Documentation Tree

```
rust/CLAUDE.md  ← YOU ARE HERE (router)
├── exomonad/CLAUDE.md  ← MCP + Hook handler via WASM (BINARY)
│   • Binary: exomonad
│   • hook subcommand: handles CC hooks via WASM
│
├── exomonad-shared/  ← Shared seam (classic + node-mode, no classic link)
│   • domain (validated newtypes), protocol (Runtime, HookEventType, hook/mcp/service)
│   • error / util / ffi / hooks / logging plumbing
│   • services::{tmux_ipc, resilience, agent_control::{AgentType, ClaudeSpawnFlags, launch, fork_session}}
│
├── exomonad-core/  ← Classic library (publishable); depends on exomonad-shared
│   • Framework: EffectHandler trait, EffectRegistry, RuntimeBuilder, Runtime
│   • PluginManager (single host fn: yield_effect)
│   • MCP types (ToolDefinition, tools module)
│   • Protocol types (hook, mcp, service)
│   • Handlers: GitHandler, GitHubHandler, LogHandler, AgentHandler,
│     FsHandler, FilePRHandler, CopilotHandler
│   • Services: GitService, GitHubService, AgentControlService, TmuxIpc, etc.
│   • External service clients: Anthropic, GitHub, Ollama, OTLP
│   • tmux IPC (via `std::process::Command`, buffer pattern for input injection)
│
├── exomonad-proto/  ← Proto-generated types (prost)
│   • FFI boundary types
│   • Effect request/response messages
│
├── exo-caps/CLAUDE.md      ← V2: The capability seam (trait contract)
├── exo-runtime/CLAUDE.md   ← V2: IO implementations of every cap
├── exo-framework/CLAUDE.md ← V2: Reusable engine abstractions (Tool/RoleDef/hook contract + RoleRegistry)
├── exo/CLAUDE.md           ← V2: The domain (tools/roles/gates/roster) + thin node entrypoint
├── exo-node/CLAUDE.md      ← V2: The per-node sidecar binary/lib
├── exo-scry/CLAUDE.md      ← V2: Live OS session derivation
└── claude-teams-bridge/    ← V2: Native Teams filesystem bridge
```

## Workspace Members

**Classic (server-based, Haskell-WASM routing):**

| Crate | Type | Purpose |
|-------|------|---------|
| [exomonad](exomonad/CLAUDE.md) | Binary (`exomonad`) | MCP + Hook handler via WASM; `serve`/`mcp-stdio`/`init`/`hook` modes |
| exomonad-core | Library | Classic framework, handlers, services, UI protocol; depends on `exomonad-shared` |
| exomonad-shared | Library | Shared seam: domain, protocol, error/util/ffi/hooks/logging, `services::{tmux_ipc, resilience, agent_control}`. No classic link; consumed by both `exomonad-core` and `exo-runtime` |
| exomonad-proto | Library | Proto-generated types (prost) for FFI + effects |

**Node-mode swarm (the `exo` binary — v2, no central server):** a per-agent
Rust sidecar, the filesystem as the bus, the process tree as the topology. Convergence
is on-disk (local `git merge`) — no GitHub/Copilot. Built beside classic, non-destructive.

| Crate | Type | Purpose |
|-------|------|---------|
| [exo-caps](exo-caps/CLAUDE.md) | Library | The capability seam: trait contract + validated domain types (no IO) |
| [exo-runtime](exo-runtime/CLAUDE.md) | Library | IO impls of every cap on one `Runtime` (reuses `exomonad-shared` services; never links classic core) |
| [exo-framework](exo-framework/CLAUDE.md) | Library | Reusable engine abstractions: `Tool`/`RoleDef`/hook contract + `PolicyCaps` + `RoleRegistry`, generic over caps; no concrete tools/roles/gates |
| [exo](exo/CLAUDE.md) | Lib + bin | The domain: concrete tools / roles / gates + `roster()` (the injected registry); thin `main.rs` node entrypoint |
| [exo-node](exo-node/CLAUDE.md) | Library | The per-node sidecar: outbound MCP + inbound inbox-watch + hook mode; resolves roles through an injected `RoleRegistry` (never depends on the domain) |
| [exo-scry](exo-scry/CLAUDE.md) | Lib + bin | Derive a CC session's active team from live OS state (native Teams delivery) |
| claude-teams-bridge | Library | Read/write messages through Claude Code's Teams filesystem |

### Feature Flags (exomonad-core)

| Feature | Default | Description |
|---------|---------|-------------|
| `runtime` | Yes | Full runtime: WASM hosting, effect handlers, services |

## Quick Reference

### Building

All `cargo` commands run from the repo root (workspace `Cargo.toml` lives there):

```bash
cargo build --release                    # Build all crates
cargo build -p exomonad                  # Build exomonad binary
cargo test --workspace                   # Run all tests

# Build WASM plugin (requires nix develop .#wasm)
nix develop .#wasm -c wasm32-wasi-cabal build --project-file=cabal.project.wasm wasm-guest
```

### Running
```bash
# MCP server (stdio)
exomonad mcp-stdio --role root --agent-id root

# Handle Claude Code hook (legacy, forwards to server)
echo '{"hook_event_name":"PreToolUse",...}' | exomonad hook pre-tool-use

# Handle Claude Code hook (v2 node mode, no server)
echo '{"hook_event_name":"PreToolUse",...}' | exo hook pre-tool-use --papers node.json
```

**Note:** WASM is loaded from `.exo/wasm/` at runtime. To update WASM, run `just wasm-all` or `exomonad recompile --role devswarm`.

### Environment Variables
| Variable | Used By | Purpose |
|----------|---------|---------|
| `GITHUB_TOKEN` | services | GitHub API access |
| `RUST_LOG` | all | Tracing log level |
| `EXOMONAD_AGENT_ID` | agent spawn | Agent identity for spawned agents (read at spawn time) |
| `EXOMONAD_SESSION_ID` | agent spawn | Parent's birth-branch, used for routing `notify_parent` |
| `EXOMONAD_ROLE` | agent spawn | Agent's role name (tl, dev, worker) |
| `EXOMONAD_TMUX_SESSION` | tmux_events, agent_control | tmux session name for IPC. Set globally via `tmux set-environment` during `exomonad init`; inherited by all windows/panes |
| `GEMINI_CLI_SYSTEM_SETTINGS_PATH` | agent spawn | Points Gemini at per-agent settings.json |
| `EXOMONAD_SWARM_RUN_ID` | agent spawn, logging | Swarm run ID (OTel resource attribute, propagated to children) |
| `EXOMONAD_PARENT_AGENT` | agent spawn, logging | Parent agent's birth branch (OTel resource attribute) |

### Agent Identity

In `mcp-stdio` mode, the agent's identity is passed via command-line flags: `--role {role} --agent-id {name}`. Role determines which WASM tool set. Identity is structural: each agent gets its own `PluginManager` with `EffectContext` (agent name + birth branch) baked in at construction. All effect handlers receive `&EffectContext` — identity is always present, no Option, no task-locals, no panic paths.

Roles are defined in Haskell WASM (`AllRoles.hs`). Adding a role is a Haskell-only change — Rust uses a lazy cache that creates a `PluginManager` per role on first request.

At spawn time, `fork_wave`/`spawn_gemini` writes per-agent MCP config with the agent's identity flags. Identity is unforgeable and visible in logs.

## MCP Tools (Classic)

All tools in the Classic architecture are defined in Haskell WASM and executed via host functions.

| Tool | Role | Description |
|------|------|-------------|
| `fork_wave` | root, tl | Fork N parallel Claude agents, each in its own worktree |
| `spawn_gemini` | root, tl | Spawn Gemini agent (worktree, inline, or standalone isolation) |
| `file_pr` | tl, dev | Create/update PR for current branch (auto-detects base branch from naming) |
| `merge_pr` | tl | Merge child PR (gh pr merge + git fetch) |
| `notify_parent` | all | Send message to parent agent (auto-routed via Teams inbox or tmux) |
| `send_message` | all | Send message to another exomonad-spawned agent (routes via Teams inbox, UDS, or tmux) |

## Effect System (Classic)

All WASM↔Rust communication in the Classic architecture flows through a single `yield_effect` host function. The Haskell guest sends protobuf-encoded `EffectEnvelope` messages, and the `EffectRegistry` dispatches to the appropriate handler by namespace prefix.

```
Haskell: runEffect @GitGetBranch request
    ↓ protobuf encode → EffectEnvelope { effect_type: "git.get_branch", payload: ... }
    ↓ yield_effect host function
    ↓ EffectRegistry::dispatch("git.get_branch", payload)
    ↓ GitHandler::handle(...)
    ↓ EffectResponse { payload | error }
    ↓ protobuf decode
Haskell: Either EffectError GetBranchResponse
```

### Error Handling Helpers

Handlers use shared ergonomic helpers from `effects/error.rs`:

- **`ResultExt::effect_err(namespace)`** — Converts any `Result<T, E: Display>` to `Result<T, EffectError>` with `EffectError::custom("{namespace}_error", e.to_string())`. Replaces verbose `.map_err(|e| EffectError::custom(...))` closures.
- **`spawn_blocking_effect(namespace, closure)`** — Runs a closure in `tokio::task::spawn_blocking` and maps both the `JoinError` and inner error to `EffectError`.

Proto field helpers in `handlers/mod.rs`: `non_empty(String) → Option<String>`, `working_dir_or_default(String) → String`, `working_dir_path_or_default(&str) → PathBuf`.

### Built-in Handlers

| Namespace | Handler | Effects |
|-----------|---------|---------|
| `git.*` | GitHandler | get_branch, get_status, get_recent_commits, get_worktree, has_unpushed_commits, get_remote_url, get_repo_info |
| `github.*` | GitHubHandler | list_issues, get_issue, create_pr, list_prs, get_pr_for_branch, get_pr_review_comments |
| `log.*` | LogHandler | info, error, emit_event |
| `agent.*` | AgentHandler | spawn_subtree, spawn_leaf_subtree, spawn_workers, cleanup_merged |
| `fs.*` | FsHandler | read_file, write_file |
| `file_pr.*` | FilePRHandler | file_pr |
| `copilot.*` | CopilotHandler | wait_for_copilot_review |
| `kv.*` | KvHandler | get, set |
| `session.*` | SessionHandler | register_claude_id, register_team, deregister_team |
| `tasks.*` | TasksHandler | list_tasks, get_task, update_task (shared task list with team auto-resolution) |
| `events.*` | EventHandler | wait_for_event (internal), notify_event, notify_parent, send_message |
| `merge_pr.*` | MergePRHandler | merge_pr (gh pr merge + git fetch) |
| `process.*` | ProcessHandler | run (execute command with args, env, working dir, timeout) |
| `coordination.*` | CoordinationHandler | acquire_mutex, release_mutex (in-memory mutex for parallel agents) |

**tmux Integration (CLI-based):**
- All tmux communication uses `std::process::Command::new("tmux")` — simple subprocess calls
- Window management: `new-window`, `kill-window`, `list-windows` with `-F` format strings for deterministic parsing
- Pane management: `split-window`, `kill-pane` for ephemeral workers
- Input injection: buffer pattern (`load-buffer` + `paste-buffer` + 150ms debounce + `send-keys Enter`), session-qualified targets (`{session}:{target}`), per-target `Mutex` serialization
- Stable addressing: `%N` pane IDs, `@N` window IDs via `-P -F "#{pane_id}"`

## Configuration

`exomonad init` auto-registers the Claude MCP server. For Gemini or custom setups, register manually in `.mcp.json`:
```json
{
  "mcpServers": {
    "exomonad": {
      "command": "exomonad",
      "args": ["mcp-stdio", "--role", "tl", "--agent-id", "root"]
    }
  }
}
```

`config.toml` is auto-created by `exomonad init` — all fields are optional.

## Testing

All commands run from repo root:

```bash
cargo test --workspace                  # All tests
cargo test -p exomonad                  # Binary tests only
cargo test -p exomonad-core             # All library tests (framework + handlers + services)
cargo test -p exomonad-proto            # Wire format compatibility tests
```

## Design Decisions

| Decision | Rationale | Scope |
|----------|-----------|-------|
| 100% WASM routing | All logic in Haskell, Rust handles I/O only | Classic |
| Capability Seam | Tool/hook logic in Rust, generic over IO traits | V2 |
| Single `yield_effect` host fn | One entry point, all effects dispatched by namespace | Classic |
| Protobuf binary encoding | Type-safe FFI boundary, generated types on both sides | Classic |
| Filesystem as the Bus | JSONL inboxes; zero-serialization soft wall | V2 |
| `runtime` feature flag | Plugin consumers get lightweight types without heavy deps | Classic |
| Local tmux orchestration | Git worktrees + tmux windows, no Docker containers | Both |
| CLI-based tmux IPC | `std::process::Command` calls to `tmux` binary | Both |
| Extism runtime | Mature WASM runtime with host function support | Classic |
| File-based devswarm WASM | Single WASM for all roles, loaded from disk, hot reload | Classic |

## Related Documentation

- [Root CLAUDE.md](../CLAUDE.md) - Project overview and documentation tree
- [Haskell wasm-guest](../haskell/wasm-guest/) - Haskell WASM plugin source
- [Haskell WASM guest](../haskell/wasm-guest/CLAUDE.md) - MCP tool definitions

## Classic Infrastructure & Observability

### Built Infrastructure

| Feature | Status |
|---------|--------|
| **Teams inbox delivery** | **Live.** `notify_parent` → Teams inbox → native `<teammate-message>` in parent conversation. Full E2E verified. |
| **HTTP-over-UDS delivery** (Shoal/custom agents) | **Built.** `notify_parent` → POST to `.exo/agents/{name}/notify.sock`. Fire-and-forget with 5s timeout. For custom binary agents that run their own HTTP server on a Unix socket. |
| **Event router** (tmux STDIN fallback) | Built. Fallback path: `notify_parent` → `inject_input` into parent pane via tmux buffer pattern. |
| **Event handlers** (WASM dispatch for world events) | **Built.** Third dispatch category alongside tools and hooks. GitHub poller calls `handle_event` on agent's PluginManager for PR review events (reviews, approvals, timeouts) and **sibling merge events**. Handlers return `EventAction` (InjectMessage, NotifyParent, NoAction). |
| **GitHub poller** (PR status → events) | Built. Background service polls PR/CI status, fires WASM event handlers, and injects notifications into agent panes. Tracks `first_seen`, `last_review_state`, and `notified_parent_timeout` per PR. |
| **OTel observability** | **Built.** Axum middleware auto-attributes every agent request span with `agent_id`, `agent.role`, `agent.parent`, `swarm.run_id`. `swarm.run_id` persisted to `.exo/run_id`, set as OTel resource attribute, propagated to children via env. Query all spans in a run: `resource.swarm.run_id = '{id}'`. Reconstruct spawn tree: `groupBy agent.parent, agent_id`. |
| **Coordination mutexes** | Built. In-memory `MutexRegistry` with FIFO wait queues, TTL auto-expiry, idempotent acquire. Effect-only (`coordination.acquire_mutex`, `coordination.release_mutex`) — no MCP tool exposed. |
| **Tempo observability** | **Built.** Grafana Tempo for lightweight trace storage (~100-200MB RAM). Agents query traces via `curl` + TraceQL against Tempo's HTTP API (port 3200). Optional Grafana UI at `http://localhost:3000`. |
| **NotebookLM MCP** (optional) | **Vendored.** `vendor/notebooklm-mcp/` — stdio MCP server that automates Google NotebookLM via browser automation. Source-grounded, citation-backed answers from uploaded documentation. Opt-in via `extra_mcp_servers` in `config.toml`. |

### Tempo Observability

Grafana Tempo provides lightweight trace storage with TraceQL query support. Agents query traces directly via `curl` against Tempo's HTTP API — no MCP tools needed.

```bash
# Start Tempo
docker compose -f .exo/otel/docker-compose.yml up -d

# Start Tempo + Grafana UI
docker compose -f .exo/otel/docker-compose.yml --profile grafana up -d

# Set otlp_endpoint in .exo/config.toml:
# otlp_endpoint = "http://localhost:4317"

# Endpoints:
#   OTLP:       localhost:4317 (gRPC), localhost:4318 (HTTP)
#   Tempo API:  http://localhost:3200 (TraceQL queries)
#   Grafana UI: http://localhost:3000 (optional, with --profile grafana)
```

**Querying traces (TraceQL via curl):**
```bash
# All spans in a run
curl -s 'http://localhost:3200/api/search?q=%7B+resource.swarm.run_id+%3D+%22abc%22+%7D&limit=50&spss=100'

# Find error spans for an agent
curl -s 'http://localhost:3200/api/search?q=%7B+span.agent_id+%3D+%22my-agent%22+%26%26+span%3Astatus+%3D+error+%7D'

# Parent-child structural query
curl -s 'http://localhost:3200/api/search?q=%7B+span.agent_id+%3D+%22tl%22+%7D+%3E%3E+%7B+span.agent_id+%3D+%22worker-1%22+%7D'

# Full trace by ID
curl -s 'http://localhost:3200/api/traces/{traceID}'
```

Without Tempo running, spans still appear in stderr via the tracing fmt layer.

### Key Design Decisions

1. **freer-simple for effects** — Standardized on freer-simple for reified continuations (WASM yield/resume)
2. **Haskell WASM as typed config DSL** — All tool/hook/event logic in Haskell, all I/O in Rust runners. The WASM yields effects; Rust executes them. Agents themselves have full tool access (bash, files, git).
3. **Haskell WASM = embedded DSL** — All logic in Haskell, Rust handles I/O only
