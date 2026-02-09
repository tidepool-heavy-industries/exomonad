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
            ├── WASM: embedded in binary (role=tl selected via config)
            └── spawn_agents creates:
                ├── Tab gh-433 (worktree, role=dev, auto-starts Claude)
                ├── Tab gh-456 (worktree, role=dev, auto-starts Claude)
                └── ... (one tab per spawned agent)
```

Each agent:
- Runs in isolated git worktree (`.exomonad/worktrees/gh-{issue}-{title}-{agent}/`)
- Has own `.exomonad/config.toml` (default_role="dev")
- Has own `.mcp.json` (pointing to `exomonad mcp-stdio`)
- Gets full issue context via agent-specific CLI argument:
  - `claude --prompt '...'` for Claude agents
  - `gemini --prompt-interactive '...'` for Gemini agents
- Runs in Zellij tab with native UI (tab-bar, status-bar)
- Auto-closes when agent exits

## Documentation Tree

```
rust/CLAUDE.md  ← YOU ARE HERE (router)
├── exomonad/CLAUDE.md  ← MCP + Hook handler via WASM (BINARY)
│   • Binary: exomonad
│   • hook subcommand: handles CC hooks via WASM
│   • mcp-stdio subcommand: MCP stdio server for Claude Code
│
├── exomonad-core/  ← Unified library (publishable)
│   • Framework: EffectHandler trait, EffectRegistry, RuntimeBuilder, Runtime
│   • PluginManager (single host fn: yield_effect)
│   • MCP stdio server (reusable)
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
# MCP stdio server (Claude Code spawns this)
exomonad mcp-stdio

# MCP HTTP server on Unix socket (for testing)
exomonad serve

# Handle Claude Code hook
echo '{"hook_event_name":"PreToolUse",...}' | exomonad hook pre-tool-use
```

**Note:** WASM is embedded in the binary at compile time. The `role` field in config selects which embedded plugin to use. To update WASM, rebuild with `just install-all-dev`.

### Environment Variables
| Variable | Used By | Purpose |
|----------|---------|---------|
| `GITHUB_TOKEN` | services | GitHub API access |
| `RUST_LOG` | all | Tracing log level |

## MCP Tools

Most tools are implemented in Haskell WASM and executed via host functions.
TL-side messaging tools are direct Rust handlers (bypass WASM).

| Tool | Description | Implementation |
|------|-------------|----------------|
| `git_branch` | Get current git branch | WASM |
| `git_status` | Get dirty files | WASM |
| `git_log` | Get recent commits | WASM |
| `read_file` | Read file contents | WASM |
| `github_list_issues` | List GitHub issues | WASM |
| `github_get_issue` | Get single issue details | WASM |
| `github_list_prs` | List GitHub pull requests | WASM |
| `spawn_agents` | Create git worktrees + Zellij tabs, auto-start agents | WASM |
| `cleanup_agents` | Close Zellij tabs and delete worktrees | WASM |
| `list_agents` | List active agent worktrees | WASM |
| `note` | Send fire-and-forget note to TL (agent-side) | WASM |
| `question` | Send blocking question to TL (agent-side) | WASM |
| `get_agent_messages` | Read notes and questions from agent outboxes (TL-side) | Direct Rust |
| `answer_question` | Answer a pending agent question (TL-side) | Direct Rust |

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
| `agent.*` | AgentHandler | spawn, spawn_batch, cleanup, cleanup_batch, cleanup_merged, list |
| `fs.*` | FsHandler | read_file, write_file |
| `popup.*` | PopupHandler | show_popup |
| `file_pr.*` | FilePRHandler | file_pr |
| `copilot.*` | CopilotHandler | wait_for_copilot_review |
| `messaging.*` | MessagingHandler | send_note, send_question |

**Zellij Integration:**
- Uses declarative KDL layouts (not CLI flags)
- Includes tab-bar and status-bar plugins (native UI)
- Wraps command in shell (`sh -c "claude"`) for environment inheritance
- Sets `close_on_exit true` for automatic cleanup

## Configuration

Add to `.mcp.json` in project root:
```json
{
  "mcpServers": {
    "exomonad": {
      "command": "exomonad",
      "args": ["mcp-stdio"]
    }
  }
}
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
| Embedded WASM | `include_bytes!` at compile time, role selects which blob |

## Related Documentation

- [Root CLAUDE.md](../CLAUDE.md) - Project overview and documentation tree
- [Haskell wasm-guest](../haskell/wasm-guest/) - Haskell WASM plugin source
- [Haskell effects](../haskell/effects/CLAUDE.md) - Effect interpreters
