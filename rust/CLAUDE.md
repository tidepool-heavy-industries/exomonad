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

**Local Zellij-based orchestration:**

```
Human in Zellij session
    └── Claude Code (main tab, role=tl)
            ├── MCP server: exomonad mcp-stdio
            ├── WASM: loaded from .exo/wasm/ at runtime
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
├── exomonad/CLAUDE.md  ← MCP + Hook handler via WASM (BINARY)
│   • Binary: exomonad
│   • hook subcommand: handles CC hooks via WASM
│
├── exomonad-core/  ← Unified library (publishable)
│   • Framework: EffectHandler trait, EffectRegistry, RuntimeBuilder, Runtime
│   • PluginManager (single host fn: yield_effect)
│   • MCP server implementation (reusable)
│   • Protocol types (hook, mcp, service)
│   • Handlers: GitHandler, GitHubHandler, LogHandler, AgentHandler,
│     FsHandler, PopupHandler, FilePRHandler, CopilotHandler
│   • Services: GitService, GitHubService, AgentControlService, etc.
│   • External service clients: Anthropic, GitHub, Ollama, OTLP
│   • UI protocol types (lightweight, available without runtime feature)
│   • Layout generation (KDL layouts for Zellij)
│
├── exomonad-proto/  ← Proto-generated types (prost)
│   • FFI boundary types
│   • Effect request/response messages
│
└── exomonad-plugin/CLAUDE.md  ← Zellij WASM plugin (built separately)
    • Status display and popup UI rendering
    • Depends on exomonad-core (default-features=false, ui_protocol only)
```

## Workspace Members

| Crate | Type | Purpose |
|-------|------|---------|
| [exomonad](exomonad/CLAUDE.md) | Binary (`exomonad`) | MCP + Hook handler via WASM |
| exomonad-core | Library | Framework, handlers, services, protocol types, UI protocol |
| exomonad-proto | Library | Proto-generated types (prost) for FFI + effects |

**Note:** [exomonad-plugin](exomonad-plugin/CLAUDE.md) is built separately (wasm32-wasi target for Zellij) and not a workspace member. It depends on `exomonad-core` with `default-features = false` to get only the lightweight `ui_protocol` types.

### Feature Flags (exomonad-core)

| Feature | Default | Description |
|---------|---------|-------------|
| `runtime` | Yes | Full runtime: WASM hosting, effect handlers, MCP server, services |

Without `runtime`: only `ui_protocol` module is available (serde + serde_json deps only). Used by `exomonad-plugin` which targets wasm32-wasi.

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
# MCP server (HTTP)
exomonad serve

# Handle Claude Code hook
echo '{"hook_event_name":"PreToolUse",...}' | exomonad hook pre-tool-use
```

**Note:** WASM is loaded from `.exo/wasm/` at runtime. To update WASM, run `just wasm-all` or `exomonad recompile --role unified`.

### Environment Variables
| Variable | Used By | Purpose |
|----------|---------|---------|
| `GITHUB_TOKEN` | services | GitHub API access |
| `RUST_LOG` | all | Tracing log level |
| `EXOMONAD_AGENT_ID` | messaging | Agent identity (env var fallback) |
| `GEMINI_CLI_SYSTEM_SETTINGS_PATH` | agent spawn | Points Gemini at per-agent settings.json |

### Agent Identity

In HTTP serve mode, multiple agents share one server process. Each agent hits a unique URL: `/agents/{name}/mcp`. The server extracts identity from the URL path and stores it in a tokio task-local via `mcp::agent_identity`.

Resolution order in `mcp::agent_identity::get_agent_id()`:
1. **Task-local** (HTTP mode): set by the `/agents/{name}/mcp` route handler
2. **Directory name** (last resort): current working directory basename

**Route layout:**
- `/tl/mcp` — TL endpoint (all tools including orchestration)
- `/dev/mcp` — generic dev endpoint (no agent identity)
- `/agents/{name}/mcp` — per-agent dev endpoint (identity from URL path)
- `/agents/{name}/tl/mcp` — per-agent TL endpoint (identity + orchestration tools)

At spawn time, `spawn_subtree`/`spawn_leaf_subtree`/`spawn_workers` (via WASM) writes per-agent MCP config with the agent's endpoint URL. The URL IS the identity — unforgeable, visible in access logs.

## MCP Tools

All tools are defined in Haskell WASM and executed via host functions.

| Tool | Description |
|------|-------------|
| `git_branch` | Get current git branch |
| `git_status` | Get dirty files |
| `git_log` | Get recent commits |
| `read_file` | Read file contents |
| `github_list_issues` | List GitHub issues |
| `github_get_issue` | Get single issue details |
| `github_list_prs` | List GitHub pull requests |
| `spawn_subtree` | Fork a worktree node off current branch (Claude-only, creates `.exo/worktrees/{slug}/`) |
| `spawn_leaf_subtree` | Spawn Gemini agent in own worktree + branch + tab (isolated, files PR) |
| `spawn_workers` | Spawn ephemeral Gemini agents as panes in parent dir (no branch, no worktree) |
| `file_pr` | Create/update PR for current branch (auto-detects base branch from naming) |
| `merge_pr` | Merge child PR (gh pr merge + jj git fetch). TL role only. |
| `note` | Send fire-and-forget note to TL (agent-side) |
| `question` | Send blocking question to TL (agent-side) |
| `get_agent_messages` | Read notes/questions from agents (TL messaging, supports long-poll) |
| `answer_question` | Answer pending agent question (TL messaging) |
| `notify_parent` | Notify parent session of completion (auto-routed, injects into parent pane) |

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

### Built-in Handlers

| Namespace | Handler | Effects |
|-----------|---------|---------|
| `git.*` | GitHandler | get_branch, get_status, get_recent_commits, get_worktree, has_unpushed_commits, get_remote_url, get_repo_info |
| `github.*` | GitHubHandler | list_issues, get_issue, create_pr, list_prs, get_pr_for_branch, get_pr_review_comments |
| `log.*` | LogHandler | info, error, emit_event |
| `agent.*` | AgentHandler | spawn_gemini_teammate, cleanup_merged |
| `fs.*` | FsHandler | read_file, write_file |
| `popup.*` | PopupHandler | show_popup |
| `file_pr.*` | FilePRHandler | file_pr |
| `copilot.*` | CopilotHandler | wait_for_copilot_review |
| `messaging.*` | MessagingHandler | send_note, send_question |
| `events.*` | EventHandler | wait_for_event (internal), notify_event, notify_parent |
| `jj.*` | JjHandler | bookmark_create, git_push, git_fetch, log, new, status |
| `merge_pr.*` | MergePRHandler | merge_pr (gh pr merge + jj git fetch) |

**Zellij Integration:**
- Uses declarative KDL layouts (not CLI flags)
- Includes tab-bar and status-bar plugins (native UI)
- Wraps command in shell (`sh -c "claude"`) for environment inheritance
- Sets `close_on_exit true` for automatic cleanup

## Configuration

Use CLI-native config commands to register the MCP server:
```bash
# Claude Code
claude mcp add --transport http exomonad http://localhost:7432/tl/mcp

# Gemini CLI (HTTP mode only)
gemini mcp add --transport http exomonad http://localhost:7432/tl/mcp
```

And ensure `.exo/config.toml` and/or `config.local.toml` exists.

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
| High-level effects | `SpawnAgent` not `CreateWorktree + OpenTab` |
| Local Zellij orchestration | Git worktrees + Zellij tabs, no Docker containers |
| Extism runtime | Mature WASM runtime with host function support |
| KDL layouts | Declarative tab creation with proper environment inheritance |
| File-based unified WASM | Single WASM for all roles, loaded from disk, hot reload in serve mode |

## Related Documentation

- [Root CLAUDE.md](../CLAUDE.md) - Project overview and documentation tree
- [Haskell wasm-guest](../haskell/wasm-guest/) - Haskell WASM plugin source
- [Haskell effects](../haskell/effects/CLAUDE.md) - Effect interpreters
