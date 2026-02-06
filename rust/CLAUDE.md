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
| **exomonad-runtime** | WASM plugin loading + host functions |
| **wasm-guest** | Haskell WASM plugin (pure logic, no I/O) |

### Deployment

**Local Zellij-based orchestration:**

```
Human in Zellij session
    └── Claude Code (main tab, role=tl)
            ├── MCP server: exomonad mcp-stdio
            ├── WASM: ~/.exomonad/wasm/wasm-guest-tl.wasm (loaded via config.local.toml)
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
├── exomonad/CLAUDE.md  ← MCP + Hook handler via WASM (CORE)
│   • Binary: exomonad
│   • hook subcommand: handles CC hooks via WASM
│   • mcp subcommand: MCP HTTP server via WASM
│   • mcp-stdio subcommand: MCP stdio server for Claude Code
│
├── exomonad-runtime/CLAUDE.md  ← WASM plugin loading + host functions
│   • PluginManager: loads WASM, routes calls
│   • Services: Git, GitHub, AgentControl, FileSystem
│   • Host functions: git_*, github_*, agent_*, fs_*
│
├── effector/CLAUDE.md  ← Stateless IO executor
│   • Cabal/Git/GH operations
│   • Returns structured JSON
│
├── exomonad-shared/CLAUDE.md  ← Shared types and utilities
│   • protocol.rs: HookInput, HookOutput, MCPCallInput, MCPCallOutput
│   • commands/hook.rs: handle_hook() implementation
│
├── exomonad-services/CLAUDE.md  ← External service clients (Library)
│   • Anthropic, GitHub, Ollama, OTLP
│   • ExternalService trait implementations
│
├── exomonad-ui-protocol/CLAUDE.md  ← Popup UI protocol types
│   • PopupDefinition, Component, VisibilityRule
│   • Shared between Haskell WASM and Zellij plugin
│
├── exomonad-plugin/CLAUDE.md  ← Zellij WASM plugin
│   • Status display and popup UI rendering
│   • Built separately (wasm32-wasi target)
│
└── zellij-gen/CLAUDE.md  ← KDL layout generator
    • Generates Zellij layouts with baked-in commands
    • Solves environment variable propagation
```

## Workspace Members

| Crate | Type | Purpose |
|-------|------|---------|
| [exomonad](exomonad/CLAUDE.md) | Binary (`exomonad`) | MCP + Hook handler via WASM |
| [exomonad-runtime](exomonad-runtime/CLAUDE.md) | Library | WASM plugin loading + host functions |
| [effector](effector/CLAUDE.md) | Binary | Stateless IO executor |
| [exomonad-shared](exomonad-shared/CLAUDE.md) | Library | Shared types, protocols |
| [exomonad-services](exomonad-services/CLAUDE.md) | Library | External service clients |
| [exomonad-ui-protocol](exomonad-ui-protocol/CLAUDE.md) | Library | Popup UI protocol types |
| [zellij-gen](zellij-gen/CLAUDE.md) | Binary | KDL layout generator |

**Note:** [exomonad-plugin](exomonad-plugin/CLAUDE.md) is built separately (wasm32-wasi target for Zellij) and not a workspace member.

## Quick Reference

### Building
```bash
cargo build --release                    # Build all crates
cargo build -p exomonad                  # Build exomonad binary
cargo test                               # Run all tests

# Build WASM plugin (requires nix develop .#wasm)
nix develop .#wasm -c wasm32-wasi-cabal build --project-file=cabal.project.wasm wasm-guest
```

### Running
```bash
# MCP stdio server (Claude Code spawns this)
exomonad mcp-stdio

# MCP HTTP server (for testing)
exomonad mcp --port 7432

# Handle Claude Code hook
echo '{"hook_event_name":"PreToolUse",...}' | exomonad hook pre-tool-use
```

**Note:** WASM plugin path is auto-resolved from `.exomonad/config.local.toml`'s `wasm_path` field.

### Environment Variables
| Variable | Used By | Purpose |
|----------|---------|---------|
| `GITHUB_TOKEN` | services | GitHub API access |
| `RUST_LOG` | all | Tracing log level |

## MCP Tools

All tools are implemented in Haskell WASM and executed via host functions:

| Tool | Description |
|------|-------------|
| `git_branch` | Get current git branch |
| `git_status` | Get dirty files |
| `git_log` | Get recent commits |
| `read_file` | Read file contents |
| `github_list_issues` | List GitHub issues |
| `github_get_issue` | Get single issue details |
| `github_list_prs` | List GitHub pull requests |
| `spawn_agents` | Create git worktrees + Zellij tabs, auto-start agents (Claude/Gemini) with KDL layout |
| `cleanup_agents` | Close Zellij tabs and delete worktrees |
| `list_agents` | List active agent worktrees |

## Host Functions (Effect Boundary)

Rust host functions exposed to WASM:

### FFI Boundary

Unified trait/typeclass for WASM/Rust communication.

**Rust:** `FFIBoundary` trait in `exomonad-shared`.
**Haskell:** `FFIBoundary` typeclass in `ExoMonad.Guest.FFI`.

**Contract:**
- All host function inputs/outputs must implement `FFIBoundary`.
- Serialization: JSON (via `serde` in Rust, `aeson` in Haskell).
- Error Handling: `FFIResult` envelope (Success/Error).
  - Rust returns `FFIResult<T>`.
  - Haskell receives `Either FFIError T`.

### Git Effects
| Effect | Host Function | Implementation |
|--------|---------------|----------------|
| `GitGetBranch` | `git_get_branch` | git subprocess |
| `GitGetDirtyFiles` | `git_get_dirty_files` | git subprocess |
| `GitGetRecentCommits` | `git_get_recent_commits` | git subprocess |

### GitHub Effects
| Effect | Host Function | Implementation |
|--------|---------------|----------------|
| `GitHubListIssues` | `github_list_issues` | HTTP API |
| `GitHubGetIssue` | `github_get_issue` | HTTP API |
| `GitHubListPRs` | `github_list_prs` | HTTP API |

### Agent Control Effects (High-Level)
| Effect | Host Function | Implementation |
|--------|---------------|----------------|
| `SpawnAgent` | `agent_spawn` | GitHub API + git worktree + Zellij KDL layout |
| `SpawnAgents` | `agent_spawn_batch` | Batch version of SpawnAgent |
| `CleanupAgent` | `agent_cleanup` | Zellij close + git worktree remove |
| `CleanupAgents` | `agent_cleanup_batch` | Batch version of CleanupAgent |
| `ListAgents` | `agent_list` | git worktree list |

**Zellij Integration:**
- Uses declarative KDL layouts (not CLI flags)
- Includes tab-bar and status-bar plugins (native UI)
- Wraps command in shell (`sh -c "claude"`) for environment inheritance
- Sets `close_on_exit true` for automatic cleanup

### Filesystem Effects
| Effect | Host Function | Implementation |
|--------|---------------|----------------|
| `ReadFile` | `fs_read_file` | tokio::fs |
| `WriteFile` | `fs_write_file` | tokio::fs |

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

```bash
cargo test                              # All tests
cargo test -p exomonad                  # Sidecar tests only
cargo test -p exomonad-runtime          # Runtime tests only

# E2E test with WASM
./target/debug/exomonad mcp --port 17432 &
curl http://localhost:17432/health
curl http://localhost:17432/mcp/tools
pkill exomonad
```

## Design Decisions

| Decision | Rationale |
|----------|-----------|
| 100% WASM routing | All logic in Haskell, Rust handles I/O only |
| High-level effects | `SpawnAgent` not `CreateWorktree + OpenTab` |
| Local Zellij orchestration | Git worktrees + Zellij tabs, no Docker containers |
| Extism runtime | Mature WASM runtime with host function support |
| KDL layouts | Declarative tab creation with proper environment inheritance |
| Config-based WASM resolution | `role` field in `.exomonad/config.local.toml` |

## Related Documentation

- [Root CLAUDE.md](../CLAUDE.md) - Project overview and documentation tree
- [Haskell wasm-guest](../haskell/wasm-guest/) - Haskell WASM plugin source
- [Haskell effects](../haskell/effects/CLAUDE.md) - Effect interpreters