# Tidepool Control Server

TCP server for Claude Code++ integration. Receives hook events and MCP tool calls from mantle-agent via NDJSON over TCP.

## Architecture

```
Claude Code (TTY) ──► mantle-agent (hook/mcp) ──► TCP :7432 ──► Control Server (Haskell)
                                                                         │
                                                                         ├─ Hook handlers
                                                                         └─ MCP handlers
```

## What It Does

- Listens on TCP (default `127.0.0.1:7432`)
- Accepts NDJSON connections (JSON + newline framing)
- Parses `ControlMessage` (HookEvent | McpToolCall)
- Routes to appropriate handlers
- Returns `ControlResponse` (HookResponse | McpToolResponse)

## Protocol

Matches rust/mantle-shared/src/protocol.rs exactly. See that file for the full spec.

**Request** (Client → Server):
```json
{"type":"HookEvent","input":{...}}\n
```

**Response** (Server → Client):
```json
{"type":"HookResponse","output":{...},"exit_code":0}\n
```

## Current Behavior

### Hook Handler (passthrough)

All hooks are logged and allowed. No real logic yet.

- PreToolUse → allow with `permissionDecision: "allow"`
- PostToolUse → allow with no additional context
- Other hooks → continue

**TODO**: Wire to Tidepool effect stack for real hook logic.

### MCP Handler (stub)

All MCP tool calls return an error: "no tools available".

**TODO**: Expose Tidepool agents as MCP tools (e.g., semantic-scout).

## Running

```bash
# Via nix shell (recommended for Claude Code++)
nix develop .#claude-code-plus

# Or manually
export MANTLE_CONTROL_HOST=127.0.0.1
export MANTLE_CONTROL_PORT=7432
cabal run tidepool-control-server
```

Server logs to stdout:
```
Control server starting on 127.0.0.1:7432
Control server listening...
Connection from V4(127.0.0.1:50123)
[HOOK] PreToolUse tool=Read
  session=abc123
  cwd=/tmp/project
[HOOK] -> continue=true decision=allow exit=0
```

## Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `MANTLE_CONTROL_HOST` | 127.0.0.1 | TCP bind address |
| `MANTLE_CONTROL_PORT` | 7432 | TCP port |

## Module Structure

| File | Purpose |
|------|---------|
| `Protocol.hs` | ControlMessage/Response types (matches Rust) |
| `Server.hs` | TCP listener, NDJSON framing, connection handling |
| `Handler.hs` | Message routing |
| `Handler/Hook.hs` | Hook event handler (passthrough) |
| `Handler/MCP.hs` | MCP tool handler (stub) |
| `Main.hs` | Entry point |

## Integration with Claude Code

1. mantle-agent hook commands read from Claude Code stdin
2. Forward to control server via TCP (NDJSON)
3. Control server returns response
4. mantle-agent writes response to Claude Code stdout

## Adding Real Hook Logic

To wire hooks to the Tidepool effect stack:

1. Add effect dependencies to `tidepool-control-server.cabal`
2. Import effect runner from `tidepool-native-server` or create new one
3. Replace passthrough logic in `Handler/Hook.hs` with effect dispatch
4. Use shared state (STM TVars) for cross-call communication

## Adding MCP Tools

To expose Tidepool agents as MCP tools:

1. Import agents (e.g., `tidepool-semantic-scout`)
2. Define tool schemas in `Handler/MCP.hs`
3. Parse tool arguments and dispatch to agent
4. Return agent results as MCP responses

See `haskell/effects/mcp-server/CLAUDE.md` for the existing MCP server pattern (exposes to Claude Code as MCP server over stdio, not TCP).

## Next Steps

1. Wire hook handlers to effect stack
2. Add semantic-scout as MCP tool
3. Implement shared state (TVars) between hooks and MCP
4. Add metrics collection to mantle-hub
5. Implement daemon mode in mantle-agent (long-lived process)
