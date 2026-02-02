# exomonad-shared

Shared library for the exomonad workspace. Provides protocol types, HTTP socket client, and hook handling utilities.

## Module Overview

| Module | File | Purpose |
|--------|------|---------|
| `protocol` | `protocol.rs` | Control envelope types (HookInput, HookOutput, ControlMessage, ControlResponse) |
| `domain` | `domain.rs` | Domain types with parse-at-edge validation (SessionId, ToolName, Role, WasmPath, etc.) |
| `error` | `error.rs` | Typed error types (`ExoMonadError`) |
| `hooks` | `hooks.rs` | Hook configuration generator for Claude Code settings |
| `logging` | `logging.rs` | Tracing/logging setup with env filter |
| `util` | `util.rs` | Shell quoting, binary path resolution |

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

**ControlMessage** - Sent to control server via HTTP:
```rust
pub enum ControlMessage {
    HookEvent { input: Box<HookInput>, runtime: Runtime },
    McpToolCall { id: String, tool_name: String, arguments: Value },
    ToolsListRequest,
    Ping,
}
```

**ControlResponse** - Received from control server:
```rust
pub enum ControlResponse {
    HookResponse { output: HookOutput, exit_code: i32 },
    McpToolResponse { id: String, result: Option<Value>, error: Option<McpError> },
    ToolsListResponse { tools: Vec<ToolDefinition> },
    Pong,
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

### HTTP Socket Client (`socket.rs`)

Synchronous HTTP client using `curl` subprocess for Unix Domain Socket communication:

```rust
pub struct ControlSocket {
    socket_path: PathBuf,
    timeout: Duration,
}

impl ControlSocket {
    /// Connect to control server. Stores socket path (actual connection happens per-request via curl).
    pub fn connect<P: AsRef<Path>>(path: P) -> Result<Self>;

    /// Send message, receive response (HTTP over Unix socket)
    pub fn send(&mut self, message: &ControlMessage) -> Result<ControlResponse>;
}

/// Get socket path from EXOMONAD_CONTROL_SOCKET env var
pub fn control_socket_path() -> Result<PathBuf>;
```

**Implementation:** Uses `curl --unix-socket` subprocess to perform HTTP requests over Unix Domain Socket. This avoids adding async dependencies to the synchronous exomonad CLI.

**Endpoints:**
- `POST /hook` - HookEvent
- `POST /mcp/call` - McpToolCall
- `GET /mcp/tools` - ToolsListRequest
- `GET /ping` - Ping

**Why curl subprocess?** Hook commands block Claude Code anyway. Using `curl` avoids pulling in async Rust HTTP libraries (hyper/reqwest) for a synchronous CLI tool.

### Hook Command (`commands/hook.rs`)

The `handle_hook` function implements the full hook handling flow:

```rust
pub fn handle_hook(event_type: HookEventType, runtime: Runtime) -> Result<()> {
    // 1. Read hook JSON from stdin
    // 2. Parse as HookInput
    // 3. Get control server socket path from env
    // 4. Connect and send ControlMessage::HookEvent via HTTP POST /hook
    // 5. Receive ControlResponse::HookResponse
    // 6. Print HookOutput JSON to stdout
    // 7. Exit with response's exit_code
}

/// Default "allow" response (for reference - not used in fail-closed mode)
pub fn default_allow_response(event_type: HookEventType) -> HookOutput;
```

**Fail-closed behavior:** If `EXOMONAD_CONTROL_SOCKET` not set or connection fails, the hook command exits with an error. This ensures configuration issues are caught during development.

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
pub enum ExoMonadError {
    Io(std::io::Error),
    JsonSerialize(serde_json::Error),
    JsonParse { source: serde_json::Error },
    UnixConnect { path: PathBuf, source: std::io::Error },
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
pub use error::{ExoMonadError, Result};
pub use protocol::{ControlMessage, ControlResponse, HookInput, HookOutput};
pub use socket::ControlSocket;
pub use commands::{handle_hook, HookEventType};
pub use logging::init_logging;
```

## Usage Example

```rust
use exomonad_shared::{handle_hook, HookEventType};
use exomonad_shared::protocol::Runtime;

fn main() -> exomonad_shared::Result<()> {
    exomonad_shared::init_logging();
    handle_hook(HookEventType::PreToolUse, Runtime::Claude)
}
```

## Testing

```bash
cargo test -p exomonad-shared

# Key test in socket.rs: starts echo server, sends ControlMessage, verifies response
```

## Design Notes

- **HTTP over Unix socket:** Standard protocol, works with Claude Code's HTTP MCP transport
- **curl subprocess:** Avoids async dependencies for synchronous CLI tool
- **Fail-closed:** Errors immediately if server missing; catches config issues during development
- **Boxed HookInput:** Reduces `ControlMessage` enum size (HookInput is large)
