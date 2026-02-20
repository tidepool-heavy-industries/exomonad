# exomonad

Unified sidecar binary: Rust host with Tidepool backend.

## Architecture

**All logic is in Haskell via Tidepool. Rust handles I/O only.**

TidepoolBackend compiles Haskell Core to native code via Cranelift JIT. Tool definitions live in `rust/exomonad-core/haskell/`. The `exomonad hook` command is a thin HTTP client that forwards hook events to the running server.

```
# Hook mode (thin HTTP client → server)
Claude Code → exomonad hook → HTTP POST localhost:{port}/hook → server → HookEnvelope → stdout

# HTTP mode (multi-agent, Tidepool backend)
N agents → exomonad serve → TCP (default: localhost:7432) → TidepoolBackend (per-agent) → effects → I/O
```

**Fail-open:** If the server is unreachable when a hook fires, `exomonad hook` prints `{"continue":true}` and exits 0. This prevents blocking the human's session.

## CLI Usage

```bash
exomonad hook pre-tool-use        # Handle Claude Code hook
exomonad serve [--port PORT]      # HTTP MCP server (multi-agent)
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
```

**Bootstrap:** `exomonad init` auto-creates `.exo/config.toml` and `.gitignore` entries if missing. Works in any project directory.

**Config hierarchy:**
- `config.toml` uses `default_role` (project-wide default)
- `config.local.toml` uses `role` (worktree-specific override)

## MCP Server

The MCP server provides tools via HTTP.

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
| `merge_pr` | tl | Merge child PR (gh merge + jj fetch) |
| `popup` | tl | Interactive UI in Zellij |
| `notify_parent` | all | Signal completion to parent |


## Environment Variables

| Variable | Required | Purpose |
|----------|----------|---------|
| `RUST_LOG` | No | Tracing log level |

## Building

```bash
just install-all-dev             # Build + install everything (debug)
just install-all                 # Build + install everything (release)
cargo build -p exomonad          # Build Rust binary only
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
    Server: TidepoolBackend::handle_hook (in-process)
         ↓
    Server returns HookEnvelope { stdout, exit_code }
         ↓
    CLI prints stdout, exits with exit_code
         ↓
    Claude Code receives response
```

## Related Documentation

- **[exomonad-core](../exomonad-core/)** - Tidepool backend, services, protocol types, UI protocol
