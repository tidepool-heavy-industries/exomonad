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

**Flow:**
```
Claude Code                    mantle-agent mcp                Control Server
    │                              │                               │
    │  JSON-RPC request (stdio)    │                               │
    │─────────────────────────────▶│                               │
    │                              │  (tools/list: local response) │
    │                              │                               │
    │                              │  (tools/call: forward via TCP)│
    │                              │──────────────────────────────▶│
    │                              │◀──────────────────────────────│
    │  JSON-RPC response (stdio)   │                               │
    │◀─────────────────────────────│                               │
```

**Protocol:** JSON-RPC 2.0 over stdio (MCP spec)

**Supported methods:**
- `initialize` - Handshake, returns server capabilities
- `initialized` - Notification acknowledgment
- `tools/list` - Returns tool definitions (from `MANTLE_DECISION_TOOLS_FILE`)
- `tools/call` - Executes tool by forwarding to control server

**Tool definition format** (in `MANTLE_DECISION_TOOLS_FILE`):
```json
[
  {
    "name": "decision::approve",
    "description": "Approve the request",
    "inputSchema": {
      "type": "object",
      "properties": {
        "notes": { "type": "string" }
      }
    }
  }
]
```

## Environment Variables

| Variable | Required | Purpose |
|----------|----------|---------|
| `MANTLE_CONTROL_HOST` | For forwarding | TCP host for control server |
| `MANTLE_CONTROL_PORT` | For forwarding | TCP port for control server |
| `MANTLE_DECISION_TOOLS_FILE` | For `mcp` | Path to JSON file with tool definitions |
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

```json
{
  "mcpServers": {
    "mantle-decision": {
      "command": "mantle-agent",
      "args": ["mcp"],
      "env": {
        "MANTLE_DECISION_TOOLS_FILE": "/path/to/tools.json",
        "MANTLE_CONTROL_HOST": "127.0.0.1",
        "MANTLE_CONTROL_PORT": "7432"
      }
    }
  },
  "hooks": {
    "PreToolUse": "mantle-agent hook pre-tool-use",
    "PostToolUse": "mantle-agent hook post-tool-use"
  }
}
```

## What's Missing (TODO)

### Daemon Mode
Currently, hooks/MCP forward directly to a control server that must exist elsewhere. Goal: Add `mantle-agent daemon` that:
- Listens on TCP for hook/MCP requests
- Always collects metrics to hub
- Forwards to Haskell native-server for effect handling

### Metrics Collection
Goal: Every hook/tool call sends metrics to mantle-hub before/after handling.
