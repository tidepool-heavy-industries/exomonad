# Claude Code++: Human-Augmented Sessions

Rust workspace for augmenting human-driven Claude Code sessions with ExoMonad integrations.

**This is NOT a headless orchestration system.** Humans interact with Claude Code directly via TTY; this infrastructure adds superpowers.

## Architecture

**100% WASM routing.** All MCP tool logic lives in Haskell WASM; Rust handles I/O only.

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

### Key Components

| Component | Purpose |
|-----------|---------|
| **exomonad** | Rust binary with WASM plugin support (hooks + MCP) |
| **exomonad-core** | Everything: framework, handlers, services, protocol types, UI protocol |
| **exomonad-proto** | Proto-generated types (prost) for FFI + effects |
| **wasm-guest** | Haskell WASM plugin (pure logic, no I/O) |

### Deployment

**Local tmux-based orchestration:**

```
Human in tmux session
    └── Claude Code (main window, role=tl)
            ├── MCP server: exomonad mcp-stdio
            ├── WASM: loaded from .exo/wasm/ at runtime
            └── spawn_subtree / spawn_leaf_subtree / spawn_workers creates:
                ├── Window subtree-1 (Claude, worktree off current branch, role=tl)
                ├── Window leaf-1 (Gemini, worktree off current branch, role=dev)
                ├── Pane worker-a (Gemini, in parent dir, ephemeral, role=dev)
                └── ... (recursive tree of worktrees + workers)
```

Each subtree agent (`spawn_subtree`):
- Runs in isolated git worktree at `.exo/worktrees/{slug}/`
- Branch naming: `{parent_branch}.{slug}` (dot separator for hierarchy)
- Gets `.mcp.json` with `{"type": "stdio", "command": "exomonad", "args": ["mcp-stdio", "--role", "tl", "--agent-id", "..."]}`
- Claude-only, gets TL role (can spawn workers, depth-capped at 2)
- Session ID = birth-branch (immutable, deterministic). Root TL = "root".
- PRs target parent branch, not main — merged via recursive fold
- Runs in tmux window with `claude 'task'` (positional arg), auto-closes on exit

Each leaf subtree agent (`spawn_leaf_subtree`):
- Same worktree isolation as `spawn_subtree` (own branch, own directory)
- Gemini — dev role (no spawn tools)
- Runs in tmux window, files PR against parent branch

Each worker agent (`spawn_workers`):
- Runs in a tmux pane in the parent's directory (no branch, no worktree, ephemeral)
- Always Gemini — lightweight, focused execution
- MCP config in `.exo/agents/{name}/settings.json`, pointed via `GEMINI_CLI_SYSTEM_SETTINGS_PATH`

## Documentation Tree

```
rust/CLAUDE.md  ← YOU ARE HERE (router)
├── exomonad/CLAUDE.md  ← MCP + Hook handler via WASM (BINARY)
│   • Binary: exomonad
│   • hook subcommand: handles CC hooks via WASM
│
├── exomonad-core/  ← Unified library (publishable)
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
└── exomonad-proto/  ← Proto-generated types (prost)
    • FFI boundary types
    • Effect request/response messages
```

## Workspace Members

| Crate | Type | Purpose |
|-------|------|---------|
| [exomonad](exomonad/CLAUDE.md) | Binary (`exomonad`) | MCP + Hook handler via WASM |
| exomonad-core | Library | Framework, handlers, services, protocol types, UI protocol |
| exomonad-proto | Library | Proto-generated types (prost) for FFI + effects |

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
exomonad mcp-stdio --role tl --agent-id root

# Handle Claude Code hook
echo '{"hook_event_name":"PreToolUse",...}' | exomonad hook pre-tool-use
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

### Agent Identity

In `mcp-stdio` mode, the agent's identity is passed via command-line flags: `--role {role} --agent-id {name}`. Role determines which WASM tool set. Identity is structural: each agent gets its own `PluginManager` with `EffectContext` (agent name + birth branch) baked in at construction. All effect handlers receive `&EffectContext` — identity is always present, no Option, no task-locals, no panic paths.

Roles are defined in Haskell WASM (`AllRoles.hs`). Adding a role is a Haskell-only change — Rust uses a lazy cache that creates a `PluginManager` per role on first request.

At spawn time, `spawn_subtree`/`spawn_leaf_subtree`/`spawn_workers` writes per-agent MCP config with the agent's identity flags. Identity is unforgeable and visible in logs.

## MCP Tools

All tools are defined in Haskell WASM and executed via host functions.

| Tool | Role | Description |
|------|------|-------------|
| `spawn_subtree` | tl | Fork Claude agent into worktree + tmux window (TL role, can spawn children) |
| `spawn_leaf_subtree` | tl | Fork Gemini agent into worktree + tmux window (dev role, files PR) |
| `spawn_workers` | tl | Spawn ephemeral Gemini agents as panes in parent dir (no branch, no worktree) |
| `file_pr` | tl, dev | Create/update PR for current branch (auto-detects base branch from naming) |
| `merge_pr` | tl | Merge child PR (gh pr merge + git fetch) |
| `notify_parent` | all | Send message to parent agent (auto-routed via Teams inbox, ACP, or tmux) |
| `send_message` | all | Send message to another exomonad-spawned agent (routes via Teams inbox, ACP, UDS, or tmux) |

## Effect System

All WASM↔Rust communication flows through a single `yield_effect` host function. The Haskell guest sends protobuf-encoded `EffectEnvelope` messages, and the `EffectRegistry` dispatches to the appropriate handler by namespace prefix.

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
| `agent.*` | AgentHandler | spawn_subtree, spawn_leaf_subtree, spawn_workers, spawn_gemini_teammate, cleanup_merged |
| `fs.*` | FsHandler | read_file, write_file |
| `file_pr.*` | FilePRHandler | file_pr |
| `copilot.*` | CopilotHandler | wait_for_copilot_review |
| `kv.*` | KvHandler | get, set |
| `session.*` | SessionHandler | register_claude_id, register_team |
| `events.*` | EventHandler | wait_for_event (internal), notify_event, notify_parent, send_message |
| `merge_pr.*` | MergePRHandler | merge_pr (gh pr merge + git fetch) |
| `coordination.*` | CoordinationHandler | acquire_mutex, release_mutex (in-memory mutex for parallel agents) |

**tmux Integration (CLI-based):**
- All tmux communication uses `std::process::Command::new("tmux")` — simple subprocess calls
- Window management: `new-window`, `kill-window`, `list-windows` with `-F` format strings for deterministic parsing
- Pane management: `split-window`, `kill-pane` for ephemeral workers
- Input injection: buffer pattern (`load-buffer` + `paste-buffer` + `send-keys Enter`), session-qualified targets (`{session}:{target}`)
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

| Decision | Rationale |
|----------|-----------|
| 100% WASM routing | All logic in Haskell, Rust handles I/O only |
| Single `yield_effect` host fn | One entry point, all effects dispatched by namespace via EffectRegistry |
| Protobuf binary encoding | Type-safe FFI boundary, generated types on both sides |
| `runtime` feature flag | Plugin consumers get lightweight types without heavy deps |
| High-level effects | `SpawnAgent` not `CreateWorktree + OpenWindow` |
| Local tmux orchestration | Git worktrees + tmux windows, no Docker containers |
| CLI-based tmux IPC | `std::process::Command` calls to `tmux` binary |
| Extism runtime | Mature WASM runtime with host function support |
| File-based devswarm WASM | Single WASM for all roles, loaded from disk, hot reload in serve mode |

## Related Documentation

- [Root CLAUDE.md](../CLAUDE.md) - Project overview and documentation tree
- [Haskell wasm-guest](../haskell/wasm-guest/) - Haskell WASM plugin source
- [Haskell WASM guest](../haskell/wasm-guest/CLAUDE.md) - MCP tool definitions
