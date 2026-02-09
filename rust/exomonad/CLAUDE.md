# exomonad

Unified sidecar binary: Rust host with Haskell WASM plugin.

## Architecture

**All logic is in Haskell WASM. Rust handles I/O only.**

Two WASM loading modes:
- **Embedded** (`include_bytes!`): WASM compiled into binary, used by `mcp-stdio` and `hook`
- **File-based** (`from_file`): WASM loaded from `.exomonad/wasm/`, used by `serve`, enables hot reload

```
# Stdio mode (single session, embedded WASM)
Claude Code → exomonad mcp-stdio → Embedded WASM → effects → I/O

# HTTP mode (multi-agent, hot-reloadable WASM)
N agents → exomonad serve → Unix socket (.exomonad/server.sock) → File WASM (auto-reload) → effects → I/O
```

### Hook Mode

For Claude Code hooks:
```
Claude Code → exomonad hook → WASM handle_pre_tool_use → HookOutput
```

### MCP Stdio Mode

For single-session MCP tools:
```
Claude Code → stdio → exomonad mcp-stdio → WASM handle_mcp_call → Result
```

### MCP HTTP Mode (Singleton Server)

For multi-agent scenarios — one server, all agents connect:
```
exomonad serve [--socket PATH]
    ├── Unix socket at .exomonad/server.sock (default)
    ├── POST /mcp → JSON-RPC (same dispatch as stdio)
    ├── GET /health → version info
    └── WASM hot-reloaded on mtime change per tool call
```

## CLI Usage

```bash
exomonad hook pre-tool-use        # Handle Claude Code hook
exomonad mcp-stdio                # Stdio MCP server (single session)
exomonad serve [--socket PATH]    # Unix socket MCP server (multi-agent, hot reload)
exomonad recompile [--role ROLE]  # Build WASM from Haskell source
exomonad init [--session NAME]    # Initialize Zellij session (Server tab + TL tab)
```

### Init Command

`exomonad init` creates a two-tab Zellij session:
- **Server tab**: Runs `exomonad serve --socket .exomonad/server.sock` (stays open on exit)
- **TL tab**: Runs `nix develop` for the dev environment (focused by default)

It also writes `.mcp.json` pointing the TL role to the unix socket endpoint (`unix://<abs-path>/tl/mcp`).

Use `--recreate` to delete an existing session and create fresh (e.g., after binary updates).

**Example `.exomonad/config.toml`:**
```toml
default_role = "tl"
project_dir = "."
```

**Config hierarchy:**
- `config.toml` uses `default_role` (project-wide default)
- `config.local.toml` uses `role` (worktree-specific override)

To update WASM: rebuild the Rust binary with `just install-all-dev`. The `build.rs` copies WASM files into the binary via `include_bytes!`, so WASM and binary are always in sync.

## MCP Server

The MCP server provides Claude Code with tools via stdio (JSON-RPC).

### Configuration

Add `.mcp.json` to your project root:

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

### Available Tools

| Tool | Description | Arguments |
|------|-------------|-----------|
| `git_branch` | Get current git branch | `path?`: directory |
| `git_status` | Get dirty files (git status --porcelain) | `path?`: directory |
| `git_log` | Get recent commits | `path?`: directory, `limit?`: count |
| `read_file` | Read file contents | `path`: file path, `max_lines?`: limit |
| `github_list_issues` | List GitHub issues | `owner`, `repo`, `state?`, `labels?` |
| `github_get_issue` | Get single issue details | `owner`, `repo`, `number` |
| `github_list_prs` | List GitHub pull requests | `owner`, `repo`, `state?`, `limit?` |
| `spawn_agents` | Spawn agents in Zellij tabs | `issues[]`, `owner`, `repo`, `worktree_dir?` |
| `cleanup_agents` | Clean up agent worktrees | `issues[]`, `force?` |
| `list_agents` | List active agent worktrees | (none) |

### Spawn Tools Requirements

The spawn tools (`spawn_agents`, `cleanup_agents`, `list_agents`) require:
- **Zellij session**: Must be running inside a Zellij terminal multiplexer
- **GITHUB_TOKEN**: Environment variable for GitHub API access
- **Git repository**: The project directory must be a git repository


## Environment Variables

| Variable | Required | Purpose |
|----------|----------|---------|
| `RUST_LOG` | No | Tracing log level |

## Effect Boundary (WASM)

All effects flow through a single `yield_effect` host function using protobuf binary encoding. Haskell calls `runEffect @EffectType request`, which encodes an `EffectEnvelope` and dispatches to the appropriate handler in the `EffectRegistry` by namespace prefix.

### Registered Handlers

| Namespace | Handler | Implementation |
|-----------|---------|----------------|
| `git.*` | GitHandler | git subprocess |
| `github.*` | GitHubHandler | HTTP API |
| `agent.*` | AgentHandler | GitHub API + git worktree + Zellij |
| `fs.*` | FsHandler | tokio::fs |
| `log.*` | LogHandler | tracing |
| `popup.*` | PopupHandler | Zellij plugin IPC |
| `file_pr.*` | FilePRHandler | gh CLI |
| `copilot.*` | CopilotHandler | GitHub API polling |
| `messaging.*` | MessagingHandler | JSONL mailbox files (outbox/inbox) |

All handlers are registered by `exomonad_core::register_builtin_handlers()`.

## Building

WASM must be built before Rust, since the Rust binary embeds WASM at compile time:

```bash
# Full install (recommended): builds WASM, then Rust, installs everything
just install-all-dev

# Or manually:
just wasm-all                    # Build WASM plugins first
cargo build -p exomonad          # Then build Rust (embeds WASM via build.rs)
```

## Testing

```bash
# Unit tests
cargo test -p exomonad

# E2E hook test (binary has embedded WASM)
echo '{"session_id":"test","hook_event_name":"PreToolUse","tool_name":"Write","transcript_path":"/tmp/t.jsonl","cwd":"/","permission_mode":"default"}' | \
  ./target/debug/exomonad hook pre-tool-use
```

## Data Flow

### Hook Flow
```
Claude Code hook JSON (stdin)
         ↓
    exomonad hook pre-tool-use
         ↓
    Use embedded WASM (role from config)
         ↓
    Parse HookInput
         ↓
    Call WASM handle_pre_tool_use
         ↓
    Haskell yields effects (GitGetBranch, LogInfo, etc.)
         ↓
    Rust executes effects via host functions
         ↓
    Haskell returns HookOutput
         ↓
    Serialize and print to stdout
         ↓
    Claude Code receives response
```

### MCP Tool Flow
```
Claude Code MCP request
         ↓
    exomonad mcp-stdio
         ↓
    Use embedded WASM (role from config)
         ↓
    stdio receives JSON-RPC request
         ↓
    Call WASM handle_mcp_call
         ↓
    Haskell dispatchTool routes to handler
         ↓
    Handler yields effects (AgentControl, FileSystem, etc.)
         ↓
    Rust executes effects via host functions
         ↓
    Haskell returns MCPCallOutput
         ↓
    stdio JSON-RPC response
```

## Related Documentation

- **[exomonad-core](../exomonad-core/)** - Framework, handlers, services, protocol types, UI protocol
- **[wasm-guest](../../haskell/wasm-guest/)** - Haskell WASM plugin source
