# exomonad

Unified sidecar binary: Rust host with Haskell WASM plugin.

## Architecture

**All logic is in Haskell WASM. Rust handles I/O only.**

WASM is loaded from `.exo/wasm/wasm-guest-devswarm.wasm` at runtime by `exomonad serve`. The `exomonad hook` command is a thin UDS client that forwards hook events to the running server — it does NOT load WASM itself.

```
# Hook mode (thin UDS client → server)
Claude Code → exomonad hook → UDS (.exo/server.sock) → server WASM → HookEnvelope → stdout

# MCP mode (multi-agent, devswarm WASM)
N agents → exomonad serve → UDS (.exo/server.sock) → Unified WASM (handles all roles) → effects → I/O
```

**Fail-open:** If the server is unreachable when a hook fires, `exomonad hook` prints `{"continue":true}` and exits 0. This prevents blocking the human's session.

## CLI Usage

```bash
exomonad hook pre-tool-use        # Handle Claude Code hook
exomonad mcp-stdio                # Stdio MCP server (single agent)
exomonad serve                    # UDS MCP server (multi-agent, hot reload)
exomonad recompile [--role ROLE]  # Build WASM from Haskell source
exomonad init [--session NAME]    # Initialize tmux session (Server window + TL window)
exomonad reload                   # Clear WASM plugin cache (hot reload)
exomonad shutdown                 # Gracefully shut down the running server
```

**Observability:** Structured events (agent spawning, tool calls, PR activity) are emitted as OTel span events via `tracing::info!` with the `otel.name` field, replacing the legacy JSONL event log.

### Init Command

`exomonad init` creates a two-window tmux session:
- **Server window**: Runs `exomonad serve` (binds .exo/server.sock)
- **TL window**: Runs the configured shell command (focused by default)

Claude MCP is auto-registered during init. For Gemini, register manually (`gemini mcp add ...`).

Use `--recreate` to delete an existing session and create fresh (e.g., after binary updates).

**Example `.exo/config.toml`:**
```toml
# All fields are optional — auto-detection handles the common case
default_role = "tl"
project_dir = "."
shell_command = "nix develop"  # environment wrapper for TL tab + server
wasm_dir = ".exo/wasm"        # project-local default
wasm_name = "devswarm"        # auto-detected from .exo/roles/ if exactly one exists
```

**Bootstrap:** `exomonad init` auto-creates `.exo/config.toml` (empty, all defaults) and `.gitignore` entries if missing. Works in any project directory.

**WASM resolution:** project `.exo/wasm/` → build from `.exo/roles/` → copy from `~/.exo/wasm/` (global install via `just install-all`).

**Config hierarchy:**
- `config.toml` uses `default_role` (project-wide default)
- `config.local.toml` uses `role` (worktree-specific override)

To update WASM, run `just wasm-all` or `exomonad recompile --role devswarm`.

## MCP Server

The server exposes a REST API over UDS. `exomonad mcp-stdio` is a translation layer that speaks MCP JSON-RPC on stdio and converts to REST calls over UDS.

### Configuration

Register manually in `.mcp.json`:
```json
{
  "mcpServers": {
    "exomonad": {
      "command": "exomonad",
      "args": ["mcp-stdio", "--role", "tl", "--agent-id", "root"]
    }
  }
}
```

### Available Tools

| Tool | Role | Description |
|------|------|-------------|
| `spawn_subtree` | tl | Fork Claude agent into worktree + tmux window (TL role) |
| `spawn_leaf_subtree` | tl | Fork Gemini agent into worktree + tmux window (dev role, files PR) |
| `spawn_workers` | tl | Spawn Gemini agents as panes (ephemeral, no worktree) |
| `file_pr` | tl, dev | Create/update PR for current branch |
| `merge_pr` | tl | Merge child PR (gh merge + git fetch) |
| `notify_parent` | all | Send message to parent agent |
| `send_message` | all | Send message to another agent (routes via Teams inbox / ACP / UDS / tmux fallback) |

### Debugging

You can probe the UDS server using `curl`:
```bash
# List tools for an agent
curl --unix-socket .exo/server.sock http://localhost/agents/tl/root/tools

# Call a tool
curl --unix-socket .exo/server.sock -X POST http://localhost/agents/tl/root/tools/call \
  -H 'Content-Type: application/json' \
  -d '{"name":"notify_parent","arguments":{"status":"success","message":"test"}}'

# Check if server is alive
curl --unix-socket .exo/server.sock http://localhost/health
```


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
| `agent.*` | AgentHandler | GitHub API + git worktree + tmux |
| `fs.*` | FsHandler | tokio::fs |
| `log.*` | LogHandler | tracing |
| `file_pr.*` | FilePRHandler | gh CLI |
| `copilot.*` | CopilotHandler | GitHub API polling |
| `events.*` | EventHandler | Event queue, notify_parent, send_message |
| `merge_pr.*` | MergePRHandler | gh pr merge + git fetch |
| `kv.*` | KvHandler | Key-value store (.exo/kv/) |
| `session.*` | SessionHandler | Claude session registry |
| `coordination.*` | CoordinationHandler | In-memory mutex (FIFO wait queue, TTL expiry) |

## Building

WASM must be built before running hooks or serve mode:

```bash
# Full install (recommended): builds WASM, then Rust, installs everything
just install-all-dev

# Or manually:
just wasm-all                    # Build devswarm WASM plugin
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

## Server Routes

| Route | Method | Purpose |
|-------|--------|---------|
| `/health` | GET | Health check |
| `/hook` | POST | Hook events (pre-tool-use, session-start, etc.) |
| `/agents/{role}/{name}/tools` | GET | List tools for an agent |
| `/agents/{role}/{name}/tools/call` | POST | Call a tool (body: `{name, arguments}`) |
| `/events` | POST | Event notifications |
| `/reload` | POST | Clear WASM plugin cache (next call loads fresh from disk) |
| `/shutdown` | POST | Graceful server shutdown |

## Data Flow

### MCP Tool Call Flow
```
Claude Code → stdio (JSON-RPC) → exomonad mcp-stdio (translates JSON-RPC → REST)
         ↓
    UDS GET .exo/server.sock /agents/{role}/{name}/tools        (tools/list)
    UDS POST .exo/server.sock /agents/{role}/{name}/tools/call  (tools/call)
         ↓
    Server: REST handler → WASM handle_list_tools / handle_mcp_call
         ↓
    Haskell dispatches to tool handler → yields effects
         ↓
    Server executes effects via host functions (in-process)
         ↓
    Server returns JSON response
         ↓
    mcp-stdio translates REST response → JSON-RPC response → stdout
         ↓
    Claude Code receives response
```

### Hook Flow
```
Claude Code hook JSON (stdin)
         ↓
    exomonad hook pre-tool-use (thin UDS client)
         ↓
    UDS POST .exo/server.sock /hook?event=pre-tool-use&runtime=claude
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
