# mantle-shared

Shared library for the mantle workspace. Provides protocol types, TCP socket client, and hook handling utilities.

## Module Overview

| Module | File | Purpose |
|--------|------|---------|
| `protocol` | `protocol.rs` | Control envelope types (HookInput, HookOutput, ControlMessage, ControlResponse) |
| `socket` | `socket.rs` | TCP client for control server communication |
| `commands` | `commands/` | CLI command implementations (hook handler, signal sender) |
| `error` | `error.rs` | Typed error types (`MantleError`) |
| `events` | `events.rs` | Stream event types for Claude Code `--stream-json` output |
| `hooks` | `hooks.rs` | Hook configuration generator for Claude Code settings |
| `logging` | `logging.rs` | Tracing/logging setup with env filter |
| `util` | `util.rs` | Shell quoting, binary path resolution |
| `fifo` | `fifo.rs` | Named pipe abstractions (legacy, for headless mode) |
| `supervisor` | `supervisor.rs` | Process supervision (legacy, for headless mode) |
| `humanize` | `humanize.rs` | Human-readable output formatting |

## Key Types

### Control Protocol (`protocol.rs`)

**HookInput** - What Claude Code sends to hook commands via stdin:
```rust
pub struct HookInput {
    pub session_id: String,
    pub transcript_path: String,
    pub cwd: String,
    pub permission_mode: String,  // default, plan, acceptEdits, etc.
    pub hook_event_name: String,  // PreToolUse, PostToolUse, etc.

    // Tool-related (PreToolUse, PostToolUse, PermissionRequest)
    pub tool_name: Option<String>,
    pub tool_input: Option<Value>,
    pub tool_use_id: Option<String>,
    pub tool_response: Option<Value>,  // PostToolUse only

    // Other hook-specific fields
    pub prompt: Option<String>,           // UserPromptSubmit
    pub message: Option<String>,          // Notification
    pub stop_hook_active: Option<bool>,   // Stop, SubagentStop
    // ... more fields
}
```

**HookOutput** - What hook commands return to Claude Code via stdout:
```rust
pub struct HookOutput {
    pub continue_: bool,  // false = stop processing
    pub stop_reason: Option<String>,
    pub suppress_output: Option<bool>,
    pub system_message: Option<String>,
    pub hook_specific_output: Option<HookSpecificOutput>,
}

pub enum HookSpecificOutput {
    PreToolUse {
        permission_decision: String,  // "allow", "deny", "ask"
        permission_decision_reason: Option<String>,
        updated_input: Option<Value>,  // Modified tool input
    },
    PostToolUse {
        additional_context: Option<String>,
    },
    // ... other variants
}
```

**ControlMessage** - Sent over TCP to control server:
```rust
pub enum ControlMessage {
    HookEvent { input: Box<HookInput> },
    McpToolCall { id: String, tool_name: String, arguments: Value },
}
```

**ControlResponse** - Received from control server:
```rust
pub enum ControlResponse {
    HookResponse { output: HookOutput, exit_code: i32 },
    McpToolResponse { id: String, result: Option<Value>, error: Option<McpError> },
}
```

**Builder helpers:**
```rust
impl HookOutput {
    pub fn pre_tool_use_allow(reason: Option<String>, modified_input: Option<Value>) -> Self;
    pub fn pre_tool_use_deny(reason: String) -> Self;
    pub fn post_tool_use_allow(context: Option<String>) -> Self;
    pub fn block(reason: String) -> Self;
}
```

### TCP Socket Client (`socket.rs`)

Synchronous TCP client for NDJSON protocol:

```rust
pub struct ControlSocket {
    stream: TcpStream,
}

impl ControlSocket {
    /// Connect to control server. Timeout: 30s.
    pub fn connect(host: &str, port: u16) -> Result<Self>;

    /// Send message, receive response (NDJSON: JSON + newline)
    pub fn send(&mut self, message: &ControlMessage) -> Result<ControlResponse>;
}

/// Get (host, port) from MANTLE_CONTROL_HOST/PORT env vars
pub fn control_server_addr() -> Option<(String, u16)>;
```

**Why synchronous?** Hook commands block Claude Code anyway. Async adds complexity without benefit for this use case.

### Hook Command (`commands/hook.rs`)

The `handle_hook` function implements the full hook handling flow:

```rust
pub fn handle_hook(event_type: HookEventType) -> Result<()> {
    // 1. Read hook JSON from stdin
    // 2. Parse as HookInput
    // 3. Get control server addr from env (or fail open)
    // 4. Connect and send ControlMessage::HookEvent
    // 5. Receive ControlResponse::HookResponse
    // 6. Print HookOutput JSON to stdout
    // 7. Exit with response's exit_code
}

/// Default "allow" response when no control server available
pub fn default_allow_response(event_type: HookEventType) -> HookOutput;
```

**Fail-open behavior:** If `MANTLE_CONTROL_HOST`/`PORT` not set or connection fails, returns `default_allow_response()` instead of erroring. This ensures Claude Code works without the control server.

### Hook Event Types (`commands/hook.rs`)

```rust
pub enum HookEventType {
    PreToolUse,        // Before tool execution
    PostToolUse,       // After tool completion
    Notification,      // Status updates
    Stop,              // Claude wants to stop
    SubagentStop,      // Subagent stopping
    PreCompact,        // Before context compaction
    SessionStart,      // Session beginning
    SessionEnd,        // Session ending
    PermissionRequest, // Permission dialog
    UserPromptSubmit,  // User submitted prompt
}
```

### Error Types (`error.rs`)

```rust
pub enum MantleError {
    Io(std::io::Error),
    JsonSerialize(serde_json::Error),
    JsonParse { source: serde_json::Error },
    TcpConnect { addr: String, source: std::io::Error },
    SocketConfig { source: std::io::Error },
    SocketWrite { source: std::io::Error },
    SocketRead { source: std::io::Error },
    McpServer(String),
    // ... more variants
}
```

## Re-exports

The crate root re-exports commonly used types:

```rust
pub use error::{MantleError, Result};
pub use protocol::{ControlMessage, ControlResponse, HookInput, HookOutput};
pub use socket::ControlSocket;
pub use commands::{handle_hook, HookEventType};
pub use logging::init_logging;
```

## Usage Example

```rust
use mantle_shared::{handle_hook, HookEventType};

fn main() -> mantle_shared::Result<()> {
    mantle_shared::init_logging();
    handle_hook(HookEventType::PreToolUse)
}
```

## Testing

```bash
cargo test -p mantle-shared

# Key test in socket.rs: starts echo server, sends ControlMessage, verifies response
```

## Design Notes

- **Synchronous socket:** Hooks block Claude Code; async unnecessary
- **Fail-open:** Default allow when control server unavailable
- **Boxed HookInput:** Reduces `ControlMessage` enum size (HookInput is large)
- **NDJSON protocol:** JSON + newline, human-readable for debugging

## Legacy Modules (from headless mode)

These modules were used for Docker-based headless orchestration (now archived):

- `fifo.rs` - Named pipes for result/signal IPC
- `supervisor.rs` - Process lifecycle with timeout/signals
- `events.rs` - Parsing Claude Code `--stream-json` output
- `humanize.rs` - Terminal output formatting

The headless code is preserved in git tag `headless-mantle-archive`.
