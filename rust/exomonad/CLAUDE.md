# exomonad

Unified sidecar binary: Rust host with Haskell WASM plugin.

## Architecture

**All logic is in Haskell WASM. Rust handles I/O only.**

WASM is loaded from `.exo/wasm/wasm-guest-unified.wasm` at runtime by `exomonad serve`. The `exomonad hook` command is a thin HTTP client that forwards hook events to the running server — it does NOT load WASM itself.

```
# Hook mode (thin HTTP client → server)
Claude Code → exomonad hook → HTTP POST localhost:{port}/hook → server WASM → HookEnvelope → stdout

# HTTP mode (multi-agent, unified WASM)
N agents → exomonad serve → TCP (default: localhost:7432) → Unified WASM (handles all roles) → effects → I/O
```

**Fail-open:** If the server is unreachable when a hook fires, `exomonad hook` prints `{"continue":true}` and exits 0. This prevents blocking the human's session.

## CLI Usage

```bash
exomonad hook pre-tool-use        # Handle Claude Code hook
exomonad serve [--port PORT]      # TCP MCP server (multi-agent, hot reload)
exomonad recompile [--role ROLE]  # Build WASM from Haskell source
exomonad init [--session NAME]    # Initialize Zellij session (Server tab + TL tab)
```

### Init Command

`exomonad init` creates a two-tab Zellij session:
- **Server tab**: Runs `exomonad serve --port <port>` (stays open on exit)
- **TL tab**: Runs `nix develop` for the dev environment (focused by default)

MCP config is managed by the CLI tools (`claude mcp add` / `gemini mcp add`), not by `exomonad init`.

Use `--recreate` to delete an existing session and create fresh (e.g., after binary updates).

**Example `.exo/config.toml`:**
```toml
default_role = "tl"
project_dir = "."
shell_command = "nix develop"  # optional: environment wrapper for TL tab + server
wasm_dir = ".exo/wasm"    # optional: override WASM location (default: ~/.exo/wasm/)
```

**Bootstrap:** `exomonad init` auto-creates `.exo/config.toml` and `.gitignore` entries if missing. Works in any project directory.

**Config hierarchy:**
- `config.toml` uses `default_role` (project-wide default)
- `config.local.toml` uses `role` (worktree-specific override)

**WASM resolution:** `wasm_dir` in config > `~/.exo/wasm/` (global default, installed by `just install-all`)

To update WASM, run `just wasm-all` or `exomonad recompile --role unified`.

## MCP Server

The MCP server provides tools via HTTP (rmcp).

### Configuration

Use CLI-native config commands:
```bash
# Claude Code
claude mcp add --transport http exomonad http://localhost:7432/agents/tl/root/mcp

# Gemini CLI (HTTP mode only)
gemini mcp add --transport http exomonad http://localhost:7432/agents/tl/root/mcp
```

### Available Tools

| Tool | Role | Description |
|------|------|-------------|
| `spawn_subtree` | tl | Fork Claude agent into worktree + Zellij tab (TL role) |
| `spawn_leaf_subtree` | tl | Fork Gemini agent into worktree + Zellij tab (dev role, files PR) |
| `spawn_workers` | tl | Spawn Gemini agents as panes (ephemeral, no worktree) |
| `file_pr` | tl, dev | Create/update PR for current branch |
| `merge_pr` | tl | Merge child PR (gh merge + git fetch) |
| `popup` | tl | Interactive UI in Zellij |
| `notify_parent` | all | Signal completion to parent |


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
| `events.*` | EventHandler | Event queue (blocking wait) |
| `merge_pr.*` | MergePRHandler | gh pr merge + git fetch |
| `kv.*` | KvHandler | Key-value store (.exo/kv/) |
| `session.*` | SessionHandler | Claude session registry |

Handlers are registered via composable group functions: `core_handlers()`, `git_handlers()`, `orchestration_handlers()`.

## Building

WASM must be built before running hooks or serve mode:

```bash
# Full install (recommended): builds WASM, then Rust, installs everything
just install-all-dev

# Or manually:
just wasm-all                    # Build unified WASM plugin
cargo build -p exomonad          # Build Rust binary
```

## Testing

```bash
# Unit tests
cargo test -p exomonad

# MCP integration tests (wrapper script manages server lifecycle)
just test-mcp

# E2E hook test (requires running server: exomonad serve)
echo '{"session_id":"test","hook_event_name":"PreToolUse","tool_name":"Write","transcript_path":"/tmp/t.jsonl","cwd":"/","permission_mode":"default"}' | \
  ./target/debug/exomonad hook pre-tool-use
```

## Data Flow

### Hook Flow
```
Claude Code hook JSON (stdin)
         ↓
    exomonad hook pre-tool-use (thin HTTP client)
         ↓
    HTTP POST localhost:{port}/hook?event=pre-tool-use&runtime=claude
         ↓
    Server: parse HookInput from body
         ↓
    Server: call WASM handle_pre_tool_use (in-process)
         ↓
    Haskell yields effects (GitGetBranch, LogInfo, etc.)
         ↓
    Server executes effects via host functions (in-process)
         ↓
    Server returns HookEnvelope { stdout, exit_code }
         ↓
    CLI prints stdout, exits with exit_code
         ↓
    Claude Code receives response
```

## Related Documentation

- **[exomonad-core](../exomonad-core/)** - Framework, handlers, services, protocol types, UI protocol
- **[wasm-guest](../../haskell/wasm-guest/)** - Haskell WASM plugin source
