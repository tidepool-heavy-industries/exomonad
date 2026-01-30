# exomonad-sidecar

Unified sidecar binary: Rust host with Haskell WASM plugin.

## Architecture

Each agent container runs this sidecar alongside Claude Code:

```
┌─────────────────────────┐
│ Agent Container         │
│ ┌─────────────────────┐ │
│ │ Claude Code         │ │
│ │   ↓ hooks           │ │
│ │ exomonad-sidecar    │ │
│ │   + WASM plugin     │ │
│ └─────────────────────┘ │
└─────────────────────────┘
```

## What This Does

- Loads Haskell WASM plugin via Extism
- Handles Claude Code hooks by calling WASM `handle_pre_tool_use`
- Provides host functions for high-level effects (Git, GitHub, Log)
- All IO handled in Rust; Haskell is IO-blind

## CLI Usage

```bash
# Handle a hook (reads JSON from stdin, writes response to stdout)
exomonad-sidecar --wasm /path/to/plugin.wasm hook pre-tool-use

# MCP server (TODO - Phase 4)
exomonad-sidecar --wasm /path/to/plugin.wasm mcp --port 7432
```

## Environment Variables

| Variable | Required | Purpose |
|----------|----------|---------|
| `EXOMONAD_WASM_PATH` | Yes (or `--wasm`) | Path to WASM plugin file |
| `EXOMONAD_ROLE` | No | Agent role (dev, tl, pm) |
| `RUST_LOG` | No | Tracing log level |

## Effect Boundary

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

# E2E test (requires built WASM)
echo '{"session_id":"test","hook_event_name":"PreToolUse","tool_name":"Write"}' | \
  ./target/debug/exomonad-sidecar --wasm rust/exomonad-runtime/tests/fixtures/wasm-guest.wasm hook pre-tool-use
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
