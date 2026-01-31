# exomonad-sidecar

Unified sidecar binary: Rust host with Haskell WASM plugin + local MCP server.

## Architecture

Two operating modes:

### 1. Hook Mode (requires WASM)

For Claude Code hooks, the sidecar loads a Haskell WASM plugin:

```
Claude Code → exomonad-sidecar hook → WASM plugin → HookOutput
```

### 2. MCP Mode (pure Rust)

For MCP tools, the sidecar runs a local HTTP server with Rust-native tools:

```
Claude Code → HTTP → exomonad-sidecar mcp → Local git/file operations
```

## CLI Usage

```bash
# Handle a hook (reads JSON from stdin, writes response to stdout)
exomonad-sidecar --wasm /path/to/plugin.wasm hook pre-tool-use

# Start MCP server (no WASM needed)
exomonad-sidecar mcp --port 7432

# MCP server with custom project directory
exomonad-sidecar mcp --port 7432 --project-dir /path/to/project
```

## MCP Server

The MCP server provides Claude Code with local tools via HTTP.

### Configuration

Add `.mcp.json` to your project root:

```json
{
  "mcpServers": {
    "exomonad": {
      "type": "http",
      "url": "http://localhost:7432/mcp"
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

### Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/mcp/tools` | GET | List available tools |
| `/mcp/call` | POST | Execute a tool |
| `/health` | GET | Health check |

### Example Usage

```bash
# Start the server
exomonad-sidecar mcp --port 7432

# In another terminal:
curl http://localhost:7432/mcp/tools
curl -X POST http://localhost:7432/mcp/call \
  -H "Content-Type: application/json" \
  -d '{"tool_name":"git_branch","arguments":{}}'
```

## Environment Variables

| Variable | Required | Purpose |
|----------|----------|---------|
| `EXOMONAD_WASM_PATH` | For hooks | Path to WASM plugin file |
| `EXOMONAD_PROJECT_DIR` | No | Project directory for MCP operations |
| `EXOMONAD_ROLE` | No | Agent role (dev, tl, pm) |
| `RUST_LOG` | No | Tracing log level |

## Effect Boundary (Hook Mode)

Haskell WASM calls these host functions (high-level semantic effects):

| Effect | Host Function | Rust implements via |
|--------|---------------|---------------------|
| `GitGetBranch` | `git_get_branch` | docker exec + git |
| `GitGetWorktree` | `git_get_worktree` | docker exec + git |
| `GitGetDirtyFiles` | `git_get_dirty_files` | docker exec + git |
| `GitGetRecentCommits` | `git_get_recent_commits` | docker exec + git |
| `GitHubListIssues` | `github_list_issues` | HTTP |
| `GitHubGetIssue` | `github_get_issue` | HTTP |
| `GitHubCreatePR` | `github_create_pr` | HTTP |
| `GitHubListPRs` | `github_list_prs` | HTTP |
| `LogInfo` | `log_info` | stdout |
| `LogError` | `log_error` | stderr |
| `EmitEvent` | `emit_event` | event bus |

**NOT exposed to Haskell** (Rust internals):
- Docker operations (exec, spawn, kill)
- File operations
- HTTP client details

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

# E2E hook test (requires built WASM)
echo '{"session_id":"test","hook_event_name":"PreToolUse","tool_name":"Write","transcript_path":"/tmp/t.jsonl","cwd":"/","permission_mode":"default"}' | \
  ./target/debug/exomonad-sidecar --wasm rust/exomonad-runtime/tests/fixtures/wasm-guest.wasm hook pre-tool-use

# MCP server test
./target/debug/exomonad-sidecar mcp --port 17432 &
curl http://localhost:17432/health
curl http://localhost:17432/mcp/tools
pkill exomonad-sidecar
```

## Hook Flow

```
Claude Code hook JSON (stdin)
         ↓
    exomonad-sidecar
         ↓
    Parse HookInput
         ↓
    Call WASM handle_pre_tool_use
         ↓
    Haskell yields effects (GitGetBranch, LogInfo)
         ↓
    Rust executes effects via host functions
         ↓
    Haskell returns HookOutput
         ↓
    Serialize and print to stdout
         ↓
    Claude Code receives response
```

## Related Documentation

- **[exomonad-runtime](../exomonad-runtime/)** - Plugin loading and host functions
- **[wasm-guest](../../haskell/wasm-guest/)** - Haskell WASM plugin source
- **[Plan](../../../.claude/plans/)** - Migration plan from Haskell control-server
