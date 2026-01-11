# mantle-agent

Container-side agent for Claude Code sessions. Runs inside Docker containers to handle Claude Code hooks and serve decision tools via MCP.

## Usage

```bash
# Handle a Claude Code hook event (called by generated hook scripts)
mantle-agent hook <event_type>   # Reads JSON from stdin, outputs response to stdout

# Run as MCP stdio server for decision tools
mantle-agent mcp
```

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          Docker Container                                    │
│                                                                              │
│  ┌──────────────────────────────────────────────────────────────────────┐   │
│  │                       Claude Code                                     │   │
│  │                                                                       │   │
│  │  hooks/pre-tool-use.sh ────▶ mantle-agent hook pre-tool-use          │   │
│  │  hooks/post-tool-use.sh ───▶ mantle-agent hook post-tool-use         │   │
│  │  hooks/stop.sh ────────────▶ mantle-agent hook stop                  │   │
│  │  ...                                                                  │   │
│  │                                                                       │   │
│  │  MCP server config ────────▶ mantle-agent mcp (stdio)                │   │
│  └──────────────────────────────────────────────────────────────────────┘   │
│                             │                                                │
│                             │ Unix socket                                    │
│                             ▼                                                │
│  ┌────────────────────────────────────────────┐                             │
│  │  /tmp/mantle.sock (control socket)         │                             │
│  └────────────────────────────────────────────┘                             │
│                             │                                                │
└─────────────────────────────│────────────────────────────────────────────────┘
                              │ Mounted socket
                              ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                    Haskell Orchestrator (host)                               │
│                    • Receives hook events                                    │
│                    • Makes allow/deny decisions                              │
│                    • Records decision tool calls                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Subcommands

### `hook <event_type>`

Handles Claude Code hook events. Called by generated shell scripts in `.claude/settings.local.json`.

**Event types:**
| Event | Purpose |
|-------|---------|
| `pre-tool-use` | Before tool execution - can allow, deny, or modify |
| `post-tool-use` | After tool execution - can inject context |
| `notification` | Status updates from Claude |
| `stop` | Claude wants to stop |
| `subagent-stop` | Subagent stopping |
| `pre-compact` | Before context compaction |
| `session-start` | Session beginning |
| `session-end` | Session ending |
| `permission-request` | Permission needed for action |
| `user-prompt-submit` | User submitted a prompt |

**Flow:**
```
Claude Code                    mantle-agent                     Haskell
    │                              │                               │
    │  hook JSON (stdin)           │                               │
    │─────────────────────────────▶│                               │
    │                              │  ControlMessage::HookEvent    │
    │                              │──────────────────────────────▶│
    │                              │                               │
    │                              │  ControlResponse::HookResponse│
    │                              │◀──────────────────────────────│
    │  hook response (stdout)      │                               │
    │◀─────────────────────────────│                               │
```

**Exit codes:**
- `0` = allow/continue
- `2` = deny/error (blocks Claude Code)

### `mcp`

Runs as MCP (Model Context Protocol) stdio server for decision tools.

**What are decision tools?**

Decision tools enable typed sum-type outputs from Claude. Instead of free-form text, Claude can call a structured tool like `decision::approve` or `decision::request_changes`.

**Protocol:** JSON-RPC 2.0 over stdio

**Supported methods:**
| Method | Purpose |
|--------|---------|
| `initialize` | Protocol handshake |
| `tools/list` | Return available decision tools |
| `tools/call` | Execute a decision tool (forwarded to control socket) |

**Tool definition format:**
```json
{
  "name": "decision::approve",
  "description": "Approve the proposed changes",
  "inputSchema": {
    "type": "object",
    "properties": {
      "notes": { "type": "string" }
    }
  }
}
```

**Flow:**
```
Claude Code MCP client          mantle-agent mcp               Haskell
    │                               │                              │
    │ {"method":"initialize",...}   │                              │
    │──────────────────────────────▶│                              │
    │◀──────────────────────────────│ initialize response          │
    │                               │                              │
    │ {"method":"tools/list",...}   │                              │
    │──────────────────────────────▶│                              │
    │◀──────────────────────────────│ [decision::approve, ...]     │
    │                               │                              │
    │ {"method":"tools/call",       │                              │
    │  "params":{"name":...}}       │                              │
    │──────────────────────────────▶│                              │
    │                               │  ControlMessage::McpToolCall │
    │                               │─────────────────────────────▶│
    │                               │◀─────────────────────────────│
    │◀──────────────────────────────│ tool result                  │
```

## Environment Variables

| Variable | Purpose |
|----------|---------|
| `MANTLE_HOOK_SOCKET` | Path to control socket (default: from CLI `--socket`) |
| `MANTLE_DECISION_TOOLS` | JSON array of decision tool definitions |
| `RUST_LOG` | Tracing log level |

## Module Reference

| File | Purpose |
|------|---------|
| `main.rs` | CLI entry point with `hook` and `mcp` subcommands |
| `mcp.rs` | MCP stdio server implementation |

## Protocol Details

### Control Message (→ Haskell)
```rust
pub enum ControlMessage {
    HookEvent { input: Box<HookInput> },
    McpToolCall { id: String, tool_name: String, arguments: Value },
}
```

### Control Response (← Haskell)
```rust
pub enum ControlResponse {
    HookResponse { output: HookOutput, exit_code: i32 },
    McpToolResponse { id: String, result: Option<Value>, error: Option<McpError> },
}
```

### Hook Input (from Claude Code)
```rust
pub struct HookInput {
    pub session_id: String,
    pub hook_event_name: String,
    pub tool_name: Option<String>,
    pub tool_input: Option<Value>,
    // ... additional hook-specific fields
}
```

### Hook Output (to Claude Code)
```rust
pub struct HookOutput {
    pub continue_: bool,
    pub stop_reason: Option<String>,
    pub hook_specific_output: Option<HookSpecificOutput>,
}
```

## Testing

```bash
cargo test -p mantle-agent
```

Tests cover JSON-RPC parsing, tool definition handling, and response generation.

## Integration Notes

### Container Setup

The container's entrypoint configures MCP if decision tools are provided:

```bash
# If MANTLE_DECISION_TOOLS is set, register MCP server
if [ -n "$MANTLE_DECISION_TOOLS" ]; then
  # Add to .claude/settings.local.json mcpServers section
fi
```

### Hook Script Generation

`mantle-shared/hooks.rs` generates `.claude/settings.local.json` with hook scripts:

```json
{
  "hooks": {
    "PreToolUse": "mantle-agent hook pre-tool-use",
    "PostToolUse": "mantle-agent hook post-tool-use",
    ...
  },
  "mcpServers": {
    "mantle-decisions": {
      "command": "mantle-agent",
      "args": ["mcp"],
      "env": {
        "MANTLE_HOOK_SOCKET": "/tmp/mantle.sock"
      }
    }
  }
}
```

### Decision Tool Names

By convention, decision tools use `decision::` prefix:
- `decision::approve`
- `decision::reject`
- `decision::request_changes`
- Custom: `decision::<variant_name>`

Haskell generates these from sum type constructors and parses the tool calls back.
