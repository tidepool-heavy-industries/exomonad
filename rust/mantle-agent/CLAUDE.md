# mantle-agent

Hook handler for Claude Code++ sessions.

## What This Does

Bridges Claude Code hooks to the Tidepool control server via HTTP over Unix Domain Socket.

**Note:** MCP tools are now handled directly by Claude Code using the HTTP MCP transport (`http+unix://`). The `mantle-agent mcp` subcommand has been removed as it's no longer needed.

## Subcommands

### `mantle-agent hook <event_type>`

Handles Claude Code hook events. Called by hook commands in `.claude/settings.local.json`.

**Flow:**
```
Claude Code                    mantle-agent hook               Control Server
    │                              │                               │
    │  hook JSON (stdin)           │                               │
    │─────────────────────────────▶│                               │
    │                              │  HTTP POST /hook (Unix socket)│
    │                              │──────────────────────────────▶│
    │                              │                               │
    │                              │  HTTP response (JSON)         │
    │                              │◀──────────────────────────────│
    │  hook response (stdout)      │                               │
    │◀─────────────────────────────│                               │
```

**Transport:** HTTP over Unix Domain Socket using `curl` subprocess.

**Event types** (from `HookEventType` enum):
| Event | Purpose |
|-------|---------|
| `pre-tool-use` | Before tool execution - can allow, deny, or modify |
| `post-tool-use` | After tool execution - can inject context |
| `notification` | Status updates from Claude |
| `stop` | Claude wants to stop |
| `subagent-stop` | Subagent (Task tool) stopping |
| `pre-compact` | Before context compaction |
| `session-start` | Session beginning |
| `session-end` | Session ending |
| `permission-request` | Permission dialog shown |
| `user-prompt-submit` | User submitted a prompt |

**Exit codes:**
- `0` = allow/continue
- `2` = deny/error (blocks Claude Code)

**Fail-closed behavior:** If `TIDEPOOL_CONTROL_SOCKET` not set or connection fails, the hook command exits with error. This ensures configuration issues are caught during development.

### `mantle-agent health`

Check control server health via ping/pong request.

```bash
mantle-agent health
```

Sends a `GET /ping` request to the control server and verifies response. Useful for troubleshooting socket connectivity.

## Environment Variables

| Variable | Required | Purpose |
|----------|----------|---------|
| `TIDEPOOL_CONTROL_SOCKET` | Yes | Unix socket path for control server (e.g., `.tidepool/sockets/control.sock`) |
| `RUST_LOG` | No | Tracing log level (e.g., `debug`, `mantle_agent=trace`) |

## Module Reference

| File | Purpose |
|------|---------|
| `main.rs` | CLI entry point, subcommand dispatch |
| `health.rs` | Health check implementation |

The `hook` subcommand uses `mantle_shared::handle_hook()` directly, which implements the HTTP client logic in `mantle_shared::socket`.

## Testing

```bash
cargo test -p mantle-agent
```

## Claude Code Configuration

To use mantle-agent hooks with Claude Code, add to `.claude/settings.local.json`:

```json
{
  "hooks": {
    "PreToolUse": "mantle-agent hook pre-tool-use",
    "PostToolUse": "mantle-agent hook post-tool-use"
  }
}
```

The `TIDEPOOL_CONTROL_SOCKET` environment variable is set by `start-augmented.sh`.

## MCP Tools

**MCP tools are now accessed directly via HTTP MCP transport.** Claude Code connects to the control-server's HTTP API at:

```
http+unix://.tidepool/sockets/control.sock
```

Configure MCP in `.mcp.json`:
```json
{
  "mcpServers": {
    "tidepool": {
      "transport": {
        "type": "http",
        "url": "http+unix://.tidepool/sockets/control.sock"
      }
    }
  }
}
```

This eliminates the need for the `mantle-agent mcp` proxy that was previously used.

## Troubleshooting

### Missing TIDEPOOL_CONTROL_SOCKET

**Problem:** mantle-agent fails with "Connection refused" or "No such file"

**Cause:** `TIDEPOOL_CONTROL_SOCKET` environment variable not set, or control server socket doesn't exist.

**Solution:**
```bash
# Verify socket exists
ls -la .tidepool/sockets/control.sock

# Ensure started via start-augmented.sh, which sets the env var
./start-augmented.sh
```

### Hook Timeout

**Problem:** Hook command times out after 300 seconds

**Cause:** Control server is unresponsive or handler is stuck

**Solution:**
1. Check control server logs in `.tidepool/logs/control-server.log`
2. Verify process-compose shows control-server as healthy
3. Check for deadlocks or infinite loops in hook handler

### curl Not Found

**Problem:** mantle-agent fails with "curl: command not found"

**Cause:** The socket client uses `curl` subprocess for HTTP-over-Unix-socket requests

**Solution:**
```bash
# Install curl (macOS)
brew install curl

# Install curl (Linux)
apt-get install curl
```

## Related Documentation

- **[rust/mantle-shared/CLAUDE.md](../mantle-shared/CLAUDE.md)** - Socket client implementation, protocol types
- **[haskell/control-server/CLAUDE.md](../../haskell/control-server/CLAUDE.md)** - HTTP server implementation, hook handlers
- **[ADR-003: MCP Tool Design Patterns](../../docs/architecture/ADR-003-MCP-Tool-Design-Patterns.md)** - Tool tier architecture

## What's Missing (TODO)

### Daemon Mode
Currently, hooks connect directly to control server. Goal: Add `mantle-agent daemon` that:
- Provides connection pooling for hooks
- Collects metrics to mantle-hub
- Graceful degradation if control server unavailable

### Metrics Collection
Goal: Every hook sends metrics to mantle-hub before/after handling.

### Configurable Fail Mode
Goal: Add `MANTLE_FAIL_MODE` environment variable to support fail-open in production deployments where Claude Code should work even if control server is down.
