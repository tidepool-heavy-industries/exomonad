# mantle-agent

Hook handler and MCP server for Claude Code++ sessions.

## What This Does

Two subcommands that bridge Claude Code to the Tidepool control server:

1. **`hook`** - Handle Claude Code hook events (stdin JSON → TCP → stdout JSON)
2. **`mcp`** - MCP stdio server that forwards tool calls via TCP

Both forward requests to a TCP control server and return responses.

## Subcommands

### `mantle-agent hook <event_type>`

Handles Claude Code hook events. Called by shell commands in `.claude/settings.local.json`.

**Flow:**
```
Claude Code                    mantle-agent hook               Control Server
    │                              │                               │
    │  hook JSON (stdin)           │                               │
    │─────────────────────────────▶│                               │
    │                              │  TCP: ControlMessage::HookEvent
    │                              │──────────────────────────────▶│
    │                              │                               │
    │                              │  TCP: ControlResponse::HookResponse
    │                              │◀──────────────────────────────│
    │  hook response (stdout)      │                               │
    │◀─────────────────────────────│                               │
```

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

**Fail-open behavior:** If `MANTLE_CONTROL_HOST`/`PORT` not set or connection fails, returns a default "allow" response. This ensures Claude Code works without the control server.

### `mantle-agent mcp`

MCP (Model Context Protocol) stdio server for decision tools.

**Usage:**
```bash
mantle-agent mcp [--tools <TOOL1>,<TOOL2>,...]
```

**Options:**
- `--tools` - Comma-separated allowlist of tool names. If omitted, all tools from the control server are exposed. When specified, at least one tool name must be provided.

**Flow:**
```
Claude Code                    mantle-agent mcp                Control Server
    │                              │                               │
    │  JSON-RPC request (stdio)    │                               │
    │─────────────────────────────▶│                               │
    │                              │  (tools/list: filtered)       │
    │                              │                               │
    │                              │  (tools/call: forward via Unix socket)│
    │                              │──────────────────────────────▶│
    │                              │◀──────────────────────────────│
    │  JSON-RPC response (stdio)   │                               │
    │◀─────────────────────────────│                               │
```

**Protocol:** JSON-RPC 2.0 over stdio (MCP spec)

**Supported methods:**
- `initialize` - Handshake, returns server capabilities
- `initialized` - Notification acknowledgment
- `tools/list` - Returns tool definitions (filtered by `--tools` if provided)
- `tools/call` - Executes tool by forwarding to control server (rejects non-allowlisted tools)

**Role-based filtering:**
The `--tools` flag enables different roles (PM, TL) to connect to the same control server with different tool visibility:
- PM sees: `pm_propose`, `pm_approve_expansion`, `pm_prioritize`, `pm_status`, `pm_review_dag`, `exo_status`
- TL sees: `find_callers`, `show_type`, `spawn_agents`, `exo_*`, `file_pr`, etc.

**Backwards compatibility:**
If `--tools` is not specified, all tools from the control server are exposed (existing behavior).

## Environment Variables

| Variable | Required | Purpose |
|----------|----------|---------|
| `TIDEPOOL_CONTROL_SOCKET` | For forwarding | Unix socket path for control server (REQUIRED) |
| `RUST_LOG` | No | Tracing log level (e.g., `debug`, `mantle_agent=trace`) |

## Module Reference

| File | Purpose |
|------|---------|
| `main.rs` | CLI entry point, subcommand dispatch |
| `mcp.rs` | MCP stdio server (JSON-RPC 2.0, tool routing, TCP forwarding) |

The `hook` subcommand uses `mantle_shared::handle_hook()` directly.

## Code Walkthrough

### mcp.rs

**Key types:**
```rust
// Tool definition (from MANTLE_DECISION_TOOLS_FILE)
pub struct ToolDefinition {
    pub name: String,
    pub description: String,
    pub input_schema: Value,  // JSON Schema
}

// Server state
pub struct McpServer {
    tools: Vec<ToolDefinition>,
    control_addr: Option<(String, u16)>,  // TCP addr for forwarding
}
```

**Request handling:**
```rust
fn handle_request(&mut self, request: JsonRpcRequest) -> JsonRpcResponse {
    match request.method.as_str() {
        "initialize" => self.handle_initialize(request.id),
        "tools/list" => self.handle_tools_list(request.id),
        "tools/call" => self.handle_tools_call(request.id, request.params),
        _ => JsonRpcResponse::method_not_found(request.id, &request.method),
    }
}
```

**Tool call forwarding:**
```rust
fn handle_tools_call(&mut self, id: Value, params: Value) -> JsonRpcResponse {
    // 1. Parse params to get tool name + arguments
    // 2. Verify tool exists in self.tools
    // 3. Connect to control server via TCP
    // 4. Send ControlMessage::McpToolCall
    // 5. Receive ControlResponse::McpToolResponse
    // 6. Return as JSON-RPC response
}
```

## Testing

```bash
cargo test -p mantle-agent

# Test MCP server manually:
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' | cargo run -p mantle-agent -- mcp
```

## Claude Code Configuration

To use mantle-agent with Claude Code, add to `.claude/settings.local.json`:

### Without tool filtering (all tools exposed):
```json
{
  "mcpServers": {
    "tidepool": {
      "command": "mantle-agent",
      "args": ["mcp"],
      "env": {
        "TIDEPOOL_CONTROL_SOCKET": "/path/to/.tidepool/sockets/control.sock"
      }
    }
  },
  "hooks": {
    "PreToolUse": "mantle-agent hook pre-tool-use",
    "PostToolUse": "mantle-agent hook post-tool-use"
  }
}
```

### With tool filtering (PM role example):
```json
{
  "mcpServers": {
    "tidepool-pm": {
      "command": "/path/to/tidepool/rust/target/release/mantle-agent",
      "args": [
        "mcp",
        "--tools", "pm_propose,pm_approve_expansion,pm_prioritize,pm_status,pm_review_dag,exo_status"
      ],
      "env": {
        "TIDEPOOL_CONTROL_SOCKET": "/path/to/.tidepool/sockets/control.sock"
      }
    }
  }
}
```

This configuration ensures PM users only see PM-specific tools, while TL users can configure a different allowlist or omit `--tools` for full access.

## Troubleshooting

### MCP Tool Visibility Issues

**Problem:** MCP shows only a subset of tools (e.g., PM tools not visible)

**Cause:** `--tools` flag is filtering the tool list. Verify your Claude Code configuration in `.claude/settings.local.json`.

**Solution:**
1. Check current `--tools` setting in MCP server configuration
2. Compare against intended role (PM, TL, Developer)
3. Update allowlist to include desired tools

```bash
# Verify tools are exported by control server
echo '{"type":"ToolsListRequest"}' | nc -U /path/to/.tidepool/sockets/control.sock

# Test mantle-agent --tools filtering directly
mantle-agent mcp --tools pm_propose,pm_status --help
```

### Tool Call Rejected with Error

**Problem:** Claude calls a tool, but mantle-agent returns "Tool not in allowlist"

**Cause:** The tool name is not included in the `--tools` flag, or the tool doesn't exist.

**Solution:**
1. Verify tool name matches control server export (check logs)
2. Add to `--tools` allowlist if tool should be visible
3. If tool doesn't exist, check control server logs for `exportMCPTools` errors

### Missing TIDEPOOL_CONTROL_SOCKET

**Problem:** mantle-agent fails with "Connection refused" or "No such file"

**Cause:** `TIDEPOOL_CONTROL_SOCKET` environment variable not set, or control server socket doesn't exist.

**Solution:**
```bash
# Verify socket exists
ls -la .tidepool/sockets/control.sock

# Set in .claude/settings.local.json:
{
  "env": {
    "TIDEPOOL_CONTROL_SOCKET": "/absolute/path/to/.tidepool/sockets/control.sock"
  }
}
```

### Tool List Changes Not Reflected

**Problem:** Added new tool to control server, but mantle-agent still shows old list

**Cause:** mantle-agent caches tool list at startup. Changes to control server require restart.

**Solution:**
1. Stop current Claude Code session (`Ctrl+P` → `q`)
2. Restart: `./start-augmented.sh` or reconnect MCP in Claude Code
3. Use `/mcp` → `Reconnect` in Claude Code to refresh tool cache

### Schema Validation Errors

**Problem:** Tool call fails with "Invalid arguments" or schema error

**Cause:** Claude generated arguments that don't match tool's JSON schema

**Solution:**
1. Check tool schema in control server MCP tool discovery logs
2. Report to control server maintainer if schema is incorrect
3. In Claude: try rephrasing the request to match schema constraints

## Related Documentation

- **[ADR-003: MCP Tool Design Patterns](../docs/architecture/ADR-003-MCP-Tool-Design-Patterns.md)** - Tool tier architecture, design rationale
- **[haskell/control-server/CLAUDE.md](../haskell/control-server/CLAUDE.md)** - Tool implementation details, tier descriptions
- **[haskell/dsl/core/CLAUDE.md](../haskell/dsl/core/CLAUDE.md)** - Graph DSL reference for tool developers

## What's Missing (TODO)

### Daemon Mode
Currently, hooks/MCP forward directly to a control server that must exist elsewhere. Goal: Add `mantle-agent daemon` that:
- Listens on TCP for hook/MCP requests
- Always collects metrics to hub
- Forwards to Haskell native-server for effect handling

### Metrics Collection
Goal: Every hook/tool call sends metrics to mantle-hub before/after handling.
