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
            ├── WASM: loaded from .exomonad/wasm/ at runtime
            └── spawn_subtree / spawn_worker creates:
                ├── Tab subtree-1 (worktree off current branch, role=tl)
                ├── Pane worker-a (in-place, Gemini, role=dev)
                └── ... (recursive tree of worktrees + workers)
```

Each subtree agent:
- Runs in isolated git worktree branching off the parent's current branch
- Branch naming: `{parent_branch}/{name}` — enables auto-detection of PR base branch
- Has own CLI-native MCP config pointing to HTTP server
- Gets TL role (can spawn their own children)
- PRs target parent branch, not main — merged via recursive fold
- Runs in Zellij tab with native UI, auto-closes on exit

Each worker agent:
- Runs in a Zellij pane in the parent's worktree (no branch, no worktree)
- Always Gemini — lightweight, focused execution
- Uses committed `.gemini/settings.json` (points to `/dev/mcp`)

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

**Note:** WASM is loaded from `.exomonad/wasm/` at runtime. To update WASM, run `just wasm-all` or `exomonad recompile --role unified`.

### Environment Variables
| Variable | Used By | Purpose |
|----------|---------|---------|
| `GITHUB_TOKEN` | services | GitHub API access |
| `RUST_LOG` | all | Tracing log level |
| `EXOMONAD_AGENT_ID` | messaging | Agent identity (env var fallback) |

### Agent Identity

In HTTP serve mode, multiple agents share one server process.

**Route layout:**
- `/tl/mcp` — TL endpoint (all tools including orchestration)
- `/dev/mcp` — dev endpoint (worker tools, no orchestration)

Workers use `/dev/mcp` via the committed `.gemini/settings.json`. Subtrees get per-agent MCP config written at spawn time pointing to `/agents/{name}/mcp` (used for agent identity in messaging).

Agent identity resolution in `mcp::agent_identity::get_agent_id()`:
1. **Task-local** (HTTP mode): set by per-agent route handler (subtrees only)
2. **`EXOMONAD_AGENT_ID` env var**: set at spawn time for all agents
3. **Directory name** (last resort): current working directory basename

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
| `spawn_subtree` | Fork a worktree node off current branch for sub-problems needing further decomposition |
| `spawn_worker` | Spawn Gemini agent for focused task in current worktree (no branch) |
| `file_pr` | Create/update PR for current branch (auto-detects base branch from naming) |
| `note` | Send fire-and-forget note to TL (agent-side) |
| `question` | Send blocking question to TL (agent-side) |
| `get_agent_messages` | Read notes/questions from agents (TL messaging, supports long-poll) |
| `answer_question` | Answer pending agent question (TL messaging) |
| `wait_for_event` | Block until a matching event occurs or timeout expires |
| `notify_completion` | Notify TL that this worker has completed |

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
| `events.*` | EventHandler | wait_for_event, notify_event |

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

And ensure `.exomonad/config.toml` and/or `config.local.toml` exists.

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
