# Claude Code++: Human-Augmented Sessions

Rust workspace for augmenting human-driven Claude Code sessions with ExoMonad integrations.

**This is NOT a headless orchestration system.** Humans interact with Claude Code directly via TTY; this infrastructure adds superpowers.

## Architecture

**100% WASM routing.** All MCP tool logic lives in Haskell WASM; Rust handles I/O only.

```
Claude Code (hook or MCP call)
       ↓
  exomonad-sidecar (Rust)
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
| **exomonad-sidecar** | Rust binary with WASM plugin support (hooks + MCP) |
| **exomonad-runtime** | WASM plugin loading + host functions |
| **wasm-guest** | Haskell WASM plugin (pure logic, no I/O) |

### Deployment

```
┌─────────────────────────────────────────────────────────────────┐
│ zellij container (human attaches here via docker attach)        │
│  • Minimal: Zellij + Docker CLI                                 │
│  • Panes: docker attach tl, docker attach pm                    │
└─────────────────────────────────────────────────────────────────┘
              │ docker attach
              ▼
┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐
│ tl (claude-agent)│  │ pm (claude-agent)│  │ subagents...     │
│ ROLE=tl          │  │ ROLE=pm          │  │ (dynamic)        │
│ Claude Code CLI  │  │ Claude Code CLI  │  │                  │
│ exomonad-sidecar │  │ exomonad-sidecar │  │ exomonad-sidecar │
└──────────────────┘  └──────────────────┘  └──────────────────┘
```

Each agent runs its own `exomonad-sidecar` with WASM plugin. No central control-server.

## Documentation Tree

```
rust/CLAUDE.md  ← YOU ARE HERE (router)
├── exomonad-sidecar/CLAUDE.md  ← MCP + Hook handler via WASM (CORE)
│   • hook subcommand: handles CC hooks via WASM
│   • mcp subcommand: MCP HTTP server via WASM
│   • mcp-stdio subcommand: MCP stdio server for Claude Code
│
├── exomonad-runtime/CLAUDE.md  ← WASM plugin loading + host functions
│   • PluginManager: loads WASM, routes calls
│   • Services: Git, GitHub, AgentControl, FileSystem
│   • Host functions: git_*, github_*, agent_*, fs_*
│
├── docker-ctl/CLAUDE.md  ← Container lifecycle management
│   • spawn - Create new agent containers
│   • exec {id} - Execute commands in containers
│   • status {id} - Container status
│   • stop {id} - Stop containers
│
├── effector/CLAUDE.md  ← Stateless IO executor
│   • Cabal/Git/GH operations
│   • Returns structured JSON
│
├── exomonad-shared/CLAUDE.md  ← Shared types and utilities
│   • protocol.rs: HookInput, HookOutput, MCPCallInput, MCPCallOutput
│   • commands/hook.rs: handle_hook() implementation
│
├── agent-status/           ← TUI Dashboard
│   • Renders Status, Logs, Controls
│
├── exomonad-services/      ← External Service Clients (Library)
│   • Anthropic, GitHub, Ollama, OTLP
│
├── tui-popup/CLAUDE.md  ← Floating pane popup
│   • Renders popup UI to /dev/tty
│
└── tui-spawner/CLAUDE.md  ← Cross-container popup spawner
    • Creates FIFO, spawns floating Zellij pane
```

## Workspace Members

| Crate | Type | Purpose |
|-------|------|---------|
| [exomonad-sidecar](exomonad-sidecar/CLAUDE.md) | Binary | MCP + Hook handler via WASM |
| [exomonad-runtime](exomonad-runtime/) | Library | WASM plugin loading + host functions |
| [docker-ctl](docker-ctl/CLAUDE.md) | Binary | Container lifecycle + remote exec |
| [effector](effector/CLAUDE.md) | Binary | Stateless IO executor |
| [exomonad-shared](exomonad-shared/CLAUDE.md) | Library | Shared types, protocols |
| [agent-status](agent-status/) | Binary | TUI Dashboard |
| [exomonad-services](exomonad-services/) | Library | External service clients |
| [tui-popup](tui-popup/CLAUDE.md) | Binary | Floating pane UI renderer |
| [tui-spawner](tui-spawner/CLAUDE.md) | Binary | FIFO-based popup spawning |

## Quick Reference

### Building
```bash
cargo build --release                    # Build all crates
cargo build -p exomonad-sidecar          # Build sidecar only
cargo test                               # Run all tests

# Build WASM plugin (requires nix develop .#wasm)
nix develop .#wasm -c wasm32-wasi-cabal build --project-file=cabal.project.wasm wasm-guest
```

### Running
```bash
# MCP stdio server (Claude Code spawns this)
exomonad-sidecar mcp-stdio

# MCP HTTP server (for testing)
exomonad-sidecar mcp --port 7432

# Handle Claude Code hook
echo '{"hook_event_name":"PreToolUse",...}' | exomonad-sidecar hook pre-tool-use
```

**Note:** WASM plugin path is auto-resolved from `.exomonad/config.toml`'s `role` field (defaults to `dev`).

### Environment Variables
| Variable | Used By | Purpose |
|----------|---------|---------|
| `GITHUB_TOKEN` | services | GitHub API access |
| `RUST_LOG` | all | Tracing log level |
| `EXOMONAD_WASM_PATH` | (deprecated) | **DEPRECATED** - Use `.exomonad/config.toml` with `role` field |

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
| `spawn_agents` | Spawn agents in Zellij tabs |
| `cleanup_agents` | Clean up agent worktrees |
| `list_agents` | List active agent worktrees |

## Host Functions (Effect Boundary)

Rust host functions exposed to WASM:

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
| `SpawnAgent` | `agent_spawn` | GitHub API + git worktree + Zellij |
| `CleanupAgent` | `agent_cleanup` | Zellij close + git worktree remove |
| `ListAgents` | `agent_list` | git worktree list |

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
      "command": "exomonad-sidecar",
      "args": ["mcp-stdio"]
    }
  }
}
```

And ensure `.exomonad/config.toml` exists with the `role` field:
```toml
role = "dev"  # or "tl", "pm", etc.
project_dir = "."
```

The sidecar will load `~/.exomonad/wasm/wasm-guest-{role}.wasm` automatically.

## Testing

```bash
cargo test                              # All tests
cargo test -p exomonad-sidecar          # Sidecar tests only
cargo test -p exomonad-runtime          # Runtime tests only

# E2E test with WASM
./target/debug/exomonad-sidecar mcp --port 17432 &
curl http://localhost:17432/health
curl http://localhost:17432/mcp/tools
pkill exomonad-sidecar
```

## Design Decisions

| Decision | Rationale |
|----------|-----------|
| 100% WASM routing | All logic in Haskell, Rust handles I/O only |
| High-level effects | `SpawnAgent` not `CreateWorktree + OpenTab` |
| Sidecar per agent | No central control-server, each agent independent |
| Extism runtime | Mature WASM runtime with host function support |

## Migration Notes

### control-server is DEPRECATED

The Haskell `control-server` package has been retired. All functionality is now in:
- **exomonad-sidecar** - Rust binary that hosts WASM
- **wasm-guest** - Haskell WASM plugin with tool logic

To use legacy control-server (not recommended):
```bash
docker compose --profile legacy up control-server
```
