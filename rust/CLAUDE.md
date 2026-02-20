# Claude Code++: Human-Augmented Sessions

Rust workspace for augmenting human-driven Claude Code sessions with ExoMonad integrations.

**This is NOT a headless orchestration system.** Humans interact with Claude Code directly via TTY; this infrastructure adds superpowers.

## Architecture

**Tidepool backend (Cranelift-compiled Haskell Core).** All MCP tool logic lives in Haskell; Rust handles I/O only. The `RuntimeBackend` trait abstracts over execution engines — currently `TidepoolBackend` which compiles Haskell Core to native code via Cranelift JIT.

```
Claude Code (hook or MCP call)
       ↓
  exomonad (Rust)
       ↓
  Arc<dyn RuntimeBackend>::call_tool / handle_hook / list_tools
       ↓
  TidepoolBackend → EffectMachine::run_with_user(haskell_expr, hlist![handlers...])
       ↓
  Haskell Core evaluated (Cranelift JIT) ← PURE LOGIC ONLY
       ↓
  Multi-effect HList dispatch (positional: tag 0→handler 0, tag 1→handler 1, ...)
       ↓
  Shared handlers (ToolInputHandler, IdentityHandler) + per-tool domain ops
       ↓
  Result via FromCore/ToCore bridge
```

**Effect composition:** Each tool's `.hs` file defines `Eff '[Input, SharedEffect1, ..., DomainOp]`. Rust composes a matching `frunk::hlist!` where each handler corresponds positionally. Shared effects (Identity, Inbox) are reusable across tools; per-tool domain ops encapsulate tool-specific I/O.

### Key Components

| Component | Purpose |
|-----------|---------|
| **exomonad** | Rust binary: MCP server + hook handler |
| **exomonad-core** | Everything: Tidepool backend, services, protocol types, UI protocol |

### Deployment

**Local Zellij-based orchestration:**

```
Human in Zellij session
    └── Claude Code (main tab, role=tl)
            ├── MCP server: exomonad serve
            ├── TidepoolBackend: Cranelift-compiled Haskell Core
            └── spawn_subtree / spawn_leaf_subtree / spawn_workers creates:
                ├── Tab subtree-1 (Claude, worktree off current branch, role=tl)
                ├── Tab leaf-1 (Gemini, worktree off current branch, role=dev)
                ├── Pane worker-a (Gemini, in parent dir, ephemeral, role=dev)
                └── ... (recursive tree of worktrees + workers)
```

Each subtree agent (`spawn_subtree`):
- Runs in isolated git worktree at `.exo/worktrees/{slug}/`
- Branch naming: `{parent_branch}.{slug}` (dot separator for hierarchy)
- Gets `.mcp.json` with `{"type": "http", "url": "..."}` pointing to per-agent endpoint
- Claude-only, gets TL role (can spawn workers, depth-capped at 2)
- Session ID = birth-branch (immutable, deterministic). Root TL = "root".
- PRs target parent branch, not main — merged via recursive fold
- Runs in Zellij tab with `claude 'task'` (positional arg), auto-closes on exit

Each leaf subtree agent (`spawn_leaf_subtree`):
- Same worktree isolation as `spawn_subtree` (own branch, own directory)
- Gemini — dev role (no spawn tools)
- Runs in Zellij tab, files PR against parent branch

Each worker agent (`spawn_workers`):
- Runs in a Zellij pane in the parent's directory (no branch, no worktree, ephemeral)
- Always Gemini — lightweight, focused execution
- MCP config in `.exo/agents/{name}/settings.json`, pointed via `GEMINI_CLI_SYSTEM_SETTINGS_PATH`

## Documentation Tree

```
rust/CLAUDE.md  ← YOU ARE HERE (router)
├── exomonad/CLAUDE.md  ← MCP + Hook handler (BINARY)
│   • Binary: exomonad
│   • hook subcommand: handles CC hooks via HTTP forwarding
│
├── exomonad-core/  ← Unified library (publishable)
│   • TidepoolBackend (Cranelift-compiled Haskell Core)
│   • MCP server implementation (reusable)
│   • Protocol types (hook, mcp, service)
│   • Services: GitService, GitHubService, AgentControlService, etc.
│   • External service clients: Anthropic, GitHub, Ollama, OTLP
│   • UI protocol types (lightweight, available without runtime feature)
│   • Layout generation (KDL layouts for Zellij)
│   • Haskell tool definitions (rust/exomonad-core/haskell/)
│
└── exomonad-plugin/CLAUDE.md  ← Zellij WASM plugin (built separately)
    • Status display and popup UI rendering
    • Depends on exomonad-core (default-features=false, ui_protocol only)
```

## Workspace Members

| Crate | Type | Purpose |
|-------|------|---------|
| [exomonad](exomonad/CLAUDE.md) | Binary (`exomonad`) | MCP + Hook handler |
| exomonad-core | Library | Tidepool backend, services, protocol types, UI protocol |

**Note:** [exomonad-plugin](exomonad-plugin/CLAUDE.md) is built separately (wasm32-wasi target for Zellij) and not a workspace member. It depends on `exomonad-core` with `default-features = false` to get only the lightweight `ui_protocol` types.

### Feature Flags (exomonad-core)

| Feature | Default | Description |
|---------|---------|-------------|
| `runtime` | Yes | Full runtime: Tidepool backend, MCP server, services |

Without `runtime`: only `ui_protocol` module is available (serde + serde_json deps only). Used by `exomonad-plugin` which targets wasm32-wasi.

## Quick Reference

### Building

All `cargo` commands run from the repo root (workspace `Cargo.toml` lives there):

```bash
cargo build --release                    # Build all crates
cargo build -p exomonad                  # Build exomonad binary
cargo test --workspace                   # Run all tests
```

### Running
```bash
# MCP server (HTTP)
exomonad serve

# Handle Claude Code hook
echo '{"hook_event_name":"PreToolUse",...}' | exomonad hook pre-tool-use
```

### Environment Variables
| Variable | Used By | Purpose |
|----------|---------|---------|
| `GITHUB_TOKEN` | services | GitHub API access |
| `RUST_LOG` | all | Tracing log level |
| `EXOMONAD_AGENT_ID` | agent spawn | Agent identity for spawned agents (read at spawn time) |
| `GEMINI_CLI_SYSTEM_SETTINGS_PATH` | agent spawn | Points Gemini at per-agent settings.json |

### Agent Identity

In HTTP serve mode, multiple agents share one server process. Each agent hits a unique URL: `/agents/{role}/{name}/mcp`. The server extracts role and identity from the URL path. Role determines which tool set.

**Per-agent backend cache:** The server maintains a `HashMap<AgentName, Arc<dyn RuntimeBackend>>`. On first request from an agent, a new `TidepoolBackend` with the agent's `EffectContext` is created and cached.

**Route layout (single pattern):**
- `/agents/{role}/{name}/mcp` — unified route for all agents
  - Root TL: `/agents/tl/root/mcp`
  - Spawned Claude subtree: `/agents/tl/{name}/mcp`
  - Spawned Gemini leaf: `/agents/dev/{name}/mcp`
  - Spawned Gemini worker: `/agents/worker/{name}/mcp`

At spawn time, `spawn_subtree`/`spawn_leaf_subtree`/`spawn_workers` writes per-agent MCP config with the agent's endpoint URL. The URL IS the identity — unforgeable, visible in access logs.

## MCP Tools

All tools are defined in Haskell and executed via TidepoolBackend.

| Tool | Role | Description |
|------|------|-------------|
| `spawn_subtree` | tl | Fork Claude agent into worktree + Zellij tab (TL role) |
| `spawn_leaf_subtree` | tl | Fork Gemini agent into worktree + Zellij tab (dev role, files PR) |
| `spawn_workers` | tl | Spawn Gemini agents as panes (ephemeral, no worktree) |
| `file_pr` | tl, dev | Create/update PR for current branch (auto-detects base branch from naming) |
| `merge_pr` | tl | Merge child PR (gh merge + jj fetch) |
| `popup` | tl | Interactive UI in Zellij (choices, text, sliders) |
| `notify_parent` | all | Signal completion to parent (auto-routed, injects into parent pane) |

## Configuration

Use CLI-native config commands to register the MCP server:
```bash
# Claude Code
claude mcp add --transport http exomonad http://localhost:7432/agents/tl/root/mcp

# Gemini CLI (HTTP mode only)
gemini mcp add --transport http exomonad http://localhost:7432/agents/tl/root/mcp
```

And ensure `.exo/config.toml` and/or `config.local.toml` exists.

## Testing

All commands run from repo root:

```bash
cargo test --workspace                  # All tests
cargo test -p exomonad                  # Binary tests only
cargo test -p exomonad-core             # All library tests
```

## Design Decisions

| Decision | Rationale |
|----------|-----------|
| Tidepool backend | Cranelift-compiled Haskell Core — native speed, no WASM overhead |
| `runtime` feature flag | Plugin consumers get lightweight types without heavy deps |
| High-level effects | `SpawnAgent` not `CreateWorktree + OpenTab` |
| Local Zellij orchestration | Git worktrees + Zellij tabs, no Docker containers |
| KDL layouts | Declarative tab creation with proper environment inheritance |
| FromCore/ToCore bridge | Type-safe FFI between Haskell Core and Rust — no protobuf |

## Related Documentation

- [Root CLAUDE.md](../CLAUDE.md) - Project overview and documentation tree
- [Haskell DSL](../haskell/dsl/core/CLAUDE.md) - Graph DSL reference
- [Haskell effects](../haskell/effects/CLAUDE.md) - Effect interpreters
