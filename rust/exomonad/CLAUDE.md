# exomonad

Unified sidecar binary: Rust host with Haskell WASM plugin.

## Architecture

**All logic is in Haskell WASM. Rust handles I/O only.**

```
Claude Code → exomonad [hook|mcp|mcp-stdio]
                     ↓
              Load WASM via config (role field)
                     ↓
              WASM plugin (Haskell)
                     ↓ yields effects
              Rust host functions execute I/O
                     ↓
              Result returned via WASM
```

### Hook Mode

For Claude Code hooks:
```
Claude Code → exomonad hook → WASM handle_pre_tool_use → HookOutput
```

### MCP Mode

For MCP tools:
```
Claude Code → stdio → exomonad mcp-stdio → WASM handle_mcp_call → Result
```

## CLI Usage

The sidecar auto-discovers WASM based on `.exomonad/config.toml`:

```bash
# Config file specifies role, resolves .exomonad/wasm/wasm-guest-{role}.wasm
exomonad hook pre-tool-use
exomonad mcp-stdio
```

No `--wasm` argument needed! The `role` field in `.exomonad/config.toml` determines which WASM plugin to load.

**Example `.exomonad/config.toml`:**
```toml
default_role = "tl"
project_dir = "."
```

This will load `.exomonad/wasm/wasm-guest-tl.wasm`.

**Config hierarchy:**
- `config.toml` uses `default_role` (project-wide default)
- `config.local.toml` uses `role` (worktree-specific override)

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

**Note:** The WASM plugin path is auto-resolved from the config's role (from `config.local.toml`) or default_role (from `config.toml`).

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
| `EXOMONAD_WASM_PATH` | No | **DEPRECATED** - Use `.exomonad/config.toml` with `default_role` field instead |

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

All handlers are registered by `exomonad_contrib::register_builtin_handlers()`.

## Building

```bash
# Build sidecar
cargo build -p exomonad

# Build WASM plugin (requires nix develop .#wasm)
nix develop .#wasm -c wasm32-wasi-cabal build --project-file=cabal.project.wasm wasm-guest
```

## Testing

```bash
# Unit tests
cargo test -p exomonad

# E2E hook test (requires .exomonad/config.toml with default_role field and built WASM)
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
    Load WASM via config (role field)
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
    Load WASM via config (role field)
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

- **[exomonad-core](../exomonad-core/)** - Framework: EffectHandler, RuntimeBuilder, PluginManager, MCP server
- **[exomonad-contrib](../exomonad-contrib/)** - Built-in handlers and services
- **[wasm-guest](../../haskell/wasm-guest/)** - Haskell WASM plugin source
