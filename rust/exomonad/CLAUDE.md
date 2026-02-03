# exomonad-sidecar

Unified sidecar binary: Rust host with Haskell WASM plugin.

## Architecture

**All logic is in Haskell WASM. Rust handles I/O only.**

```
Claude Code → exomonad-sidecar [hook|mcp|mcp-stdio]
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
Claude Code → exomonad-sidecar hook → WASM handle_pre_tool_use → HookOutput
```

### MCP Mode

For MCP tools:
```
Claude Code → stdio → exomonad-sidecar mcp-stdio → WASM handle_mcp_call → Result
```

## CLI Usage

The sidecar auto-discovers WASM based on `.exomonad/config.toml`:

```bash
# Config file specifies role, sidecar resolves ~/.exomonad/wasm/wasm-guest-{role}.wasm
exomonad-sidecar hook pre-tool-use
exomonad-sidecar mcp-stdio
```

No `--wasm` argument needed! The `role` field in `.exomonad/config.toml` determines which WASM plugin to load.

**Example `.exomonad/config.toml`:**
```toml
role = "tl"
project_dir = "."
```

This will load `~/.exomonad/wasm/wasm-guest-tl.wasm`.

## MCP Server

The MCP server provides Claude Code with tools via stdio (JSON-RPC).

### Configuration

Add `.mcp.json` to your project root:

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

**Note:** The WASM plugin path is auto-resolved from `.exomonad/config.toml`'s `role` field.

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
| `EXOMONAD_WASM_PATH` | No | **DEPRECATED** - Use `.exomonad/config.toml` with `role` field instead |

## Effect Boundary (WASM)

Haskell WASM calls these host functions (high-level semantic effects):

### Git Effects
| Effect | Host Function | Rust implements via |
|--------|---------------|---------------------|
| `GitGetBranch` | `git_get_branch` | local git subprocess |
| `GitGetWorktree` | `git_get_worktree` | local git subprocess |
| `GitGetDirtyFiles` | `git_get_dirty_files` | local git subprocess |
| `GitGetRecentCommits` | `git_get_recent_commits` | local git subprocess |

### GitHub Effects
| Effect | Host Function | Rust implements via |
|--------|---------------|---------------------|
| `GitHubListIssues` | `github_list_issues` | HTTP API |
| `GitHubGetIssue` | `github_get_issue` | HTTP API |
| `GitHubCreatePR` | `github_create_pr` | HTTP API |
| `GitHubListPRs` | `github_list_prs` | HTTP API |

### Agent Control Effects (High-Level)
| Effect | Host Function | Rust implements via |
|--------|---------------|---------------------|
| `SpawnAgent` | `agent_spawn` | GitHub API + git worktree + Zellij |
| `SpawnAgents` | `agent_spawn_batch` | Batch version of spawn |
| `CleanupAgent` | `agent_cleanup` | Zellij close + git worktree remove |
| `CleanupAgents` | `agent_cleanup_batch` | Batch version of cleanup |
| `ListAgents` | `agent_list` | git worktree list |

### Filesystem Effects
| Effect | Host Function | Rust implements via |
|--------|---------------|---------------------|
| `ReadFile` | `fs_read_file` | tokio::fs |
| `WriteFile` | `fs_write_file` | tokio::fs |

### Log Effects
| Effect | Host Function | Rust implements via |
|--------|---------------|---------------------|
| `LogInfo` | `log_info` | tracing |
| `LogError` | `log_error` | tracing |
| `EmitEvent` | `emit_event` | event bus |

**NOT exposed to Haskell** (Rust internals):
- HTTP client implementation details
- Subprocess management

## Building

```bash
# Build sidecar
cargo build -p exomonad-sidecar

# Build WASM plugin (requires nix develop .#wasm)
nix develop .#wasm -c wasm32-wasi-cabal build --project-file=cabal.project.wasm wasm-guest
```

## Testing

```bash
# Unit tests
cargo test -p exomonad-sidecar

# E2E hook test (requires .exomonad/config.toml with role field and built WASM)
echo '{"session_id":"test","hook_event_name":"PreToolUse","tool_name":"Write","transcript_path":"/tmp/t.jsonl","cwd":"/","permission_mode":"default"}' | \
  ./target/debug/exomonad-sidecar hook pre-tool-use
```

## Data Flow

### Hook Flow
```
Claude Code hook JSON (stdin)
         ↓
    exomonad-sidecar hook pre-tool-use
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
    exomonad-sidecar mcp-stdio
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

- **[exomonad-runtime](../exomonad-runtime/)** - Plugin loading and host functions
- **[wasm-guest](../../haskell/wasm-guest/)** - Haskell WASM plugin source
