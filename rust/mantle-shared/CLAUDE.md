# mantle-shared

Shared library providing core types, protocols, and IPC utilities for the mantle workspace. Used by both `mantle` (host) and `mantle-agent` (container).

## Module Overview

| Module | Purpose |
|--------|---------|
| `error` | Typed error types (`MantleError`) with source attribution |
| `events` | Stream event types for parsing Claude Code's `--stream-json` output |
| `protocol` | Control envelope protocol (hook input/output, socket messages) |
| `fifo` | Named pipe abstractions for IPC with RAII cleanup |
| `socket` | Unix socket client for control envelope communication |
| `hub` | Hub client for session registration and event streaming |
| `hooks` | Hook configuration generator for Claude Code settings |
| `supervisor` | Process lifecycle with timeout and signal forwarding |
| `humanize` | Human-readable terminal output formatting |
| `commands` | CLI command implementations (hook handling, signals) |
| `util` | Shell quoting, binary path resolution |

## Core Types

### Error Handling
```rust
pub type Result<T> = std::result::Result<T, MantleError>;

pub enum MantleError {
    Io(std::io::Error),
    Json(serde_json::Error),
    Fifo(String),
    Socket(String),
    Process(String),
    Timeout,
    Hub(String),
    McpServer(String),
}
```

### Stream Events (Claude Code Output)
```rust
// Parsed from Claude Code --stream-json output
pub enum StreamEvent {
    System(SystemEvent),      // Init event: session_id, model, tools
    Assistant(AssistantEvent),// Claude's messages (text, tool_use)
    User(UserEvent),          // User/tool responses
    Result(ResultEvent),      // Final result: cost, turns, structured_output
}

pub enum ContentBlock {
    Text { text: String },
    ToolUse { name, id, input },
    ToolResult { tool_use_id, content, is_error },
}
```

### Run Result (mantle → Haskell)
```rust
pub struct RunResult {
    pub exit_code: i32,
    pub is_error: bool,
    pub result: Option<String>,
    pub structured_output: Option<Value>,  // --json-schema output
    pub session_id: String,
    pub session_tag: Option<String>,       // Worktree correlation
    pub total_cost_usd: f64,
    pub num_turns: i64,
    pub events: Vec<StreamEvent>,
    pub permission_denials: Vec<PermissionDenial>,
    pub model_usage: HashMap<String, ModelUsage>,
    pub interrupts: Vec<InterruptSignal>,  // Out-of-band signals
    pub tool_calls: Option<Vec<ToolCall>>, // Decision tool calls
}
```

### Control Protocol
```rust
// Hook event payload from Claude Code
pub struct HookInput {
    pub session_id: String,
    pub hook_event_name: String,  // PreToolUse, PostToolUse, etc.
    pub tool_name: Option<String>,
    pub tool_input: Option<Value>,
    // ... additional hook-specific fields
}

// Hook response to Claude Code
pub struct HookOutput {
    pub continue_: bool,           // false = stop processing
    pub stop_reason: Option<String>,
    pub hook_specific_output: Option<HookSpecificOutput>,
}

// Socket messages (NDJSON)
pub enum ControlMessage {
    HookEvent { input: Box<HookInput> },
    McpToolCall { id, tool_name, arguments },
}

pub enum ControlResponse {
    HookResponse { output: HookOutput, exit_code: i32 },
    McpToolResponse { id, result, error },
}
```

## IPC Abstractions

### Control Socket
Synchronous Unix socket client for hook decisions:
```rust
pub struct ControlSocket { /* ... */ }

impl ControlSocket {
    // Connect to socket at path (or TIDEPOOL_CONTROL_SOCKET env)
    pub fn connect(path: Option<&Path>) -> Result<Self>;

    // Send message, receive response
    pub fn send_and_receive(&mut self, msg: &ControlMessage) -> Result<ControlResponse>;
}
```

### FIFOs (Named Pipes)
RAII wrappers for inter-process communication:
```rust
pub struct ResultFifo { /* ... */ }
pub struct SignalFifo { /* ... */ }

impl ResultFifo {
    pub fn create(path: &Path) -> Result<Self>;
    pub fn write(&self, result: &RunResult) -> Result<()>;
    pub fn read(&self) -> Result<Option<RunResult>>;
}

impl SignalFifo {
    pub fn create(path: &Path) -> Result<Self>;
    pub fn send(&self, signal: &InterruptSignal) -> Result<()>;
    pub fn receive(&self) -> Result<Option<InterruptSignal>>;
}
```

FIFOs automatically clean up on drop via `FifoGuard`.

### Hub Client
HTTP + WebSocket client for session coordination:
```rust
pub struct HubClient { /* ... */ }

impl HubClient {
    pub async fn new() -> Result<Self>;

    // Session management
    pub async fn create_session(&self, session: &SessionRegister) -> Result<SessionCreateResponse>;
    pub async fn get_session(&self, session_id: &str) -> Result<SessionInfo>;

    // Node management
    pub async fn register_node(&self, session_id: &str, node: &NodeRegister) -> Result<NodeInfo>;
    pub async fn submit_result(&self, session_id: &str, node_id: &str, result: &NodeResult) -> Result<()>;

    // WebSocket streaming
    pub async fn connect_ws(&self) -> Result<WebSocketStream>;
}
```

## Hook System

### Hook Event Types
```rust
pub enum HookEventType {
    PreToolUse,       // Before tool execution (can deny/modify)
    PostToolUse,      // After tool execution (can inject context)
    Notification,     // Status updates from Claude
    Stop,             // Claude wants to stop
    SubagentStop,     // Subagent stopping
    PreCompact,       // Before context compaction
    SessionStart,     // Session beginning
    SessionEnd,       // Session ending
    PermissionRequest,// Permission needed for action
    UserPromptSubmit, // User submitted a prompt
}
```

### Hook Configuration Generator
```rust
pub struct HookConfig { /* ... */ }

impl HookConfig {
    // Generate settings.local.json for Claude Code
    pub fn generate(
        socket_path: &Path,
        agent_binary: &Path,
        enabled_hooks: &[HookEventType],
    ) -> Value;
}
```

### Hook Output Builders
```rust
impl HookOutput {
    pub fn pre_tool_use_allow(reason: Option<String>, modified_input: Option<Value>) -> Self;
    pub fn pre_tool_use_deny(reason: String) -> Self;
    pub fn post_tool_use_allow(context: Option<String>) -> Self;
    pub fn block(reason: String) -> Self;
}
```

## Process Supervision

```rust
pub struct Supervisor { /* ... */ }

impl Supervisor {
    pub fn new(timeout: Option<Duration>) -> Self;

    // Run command with signal forwarding and timeout
    pub fn run(&self, cmd: &mut Command) -> Result<(i32, String)>;

    // Check if interrupted
    pub fn was_interrupted(&self) -> bool;
}
```

Features:
- Global signal handlers (SIGINT, SIGTERM, SIGTSTP)
- Timeout enforcement
- Clean process termination on interrupt

## Utilities

### Shell Quoting
```rust
pub fn shell_quote(s: &str) -> String;  // Safe shell escaping
pub fn build_prompt(parts: &[&str]) -> String;  // Join with newlines
```

### Binary Resolution
```rust
pub fn find_mantle_binary() -> Result<PathBuf>;       // Find mantle in PATH
pub fn find_mantle_agent_binary() -> Result<PathBuf>; // Find mantle-agent
```

## Testing

```rust
cargo test -p mantle-shared

// Key test files:
// - tests/fifo_integration.rs    FIFO creation/cleanup
// - tests/socket_integration.rs  Control socket communication
// - tests/supervisor_integration.rs  Process supervision
```

## Dependencies

| Crate | Purpose |
|-------|---------|
| serde/serde_json | Serialization |
| thiserror | Typed errors |
| nix | Unix syscalls (FIFOs, signals) |
| tracing | Structured logging |
| reqwest | HTTP client (hub) |
| tokio-tungstenite | WebSocket client (hub) |
| shell-escape | Shell quoting |
| clap | CLI argument parsing |

## Design Notes

- **Synchronous socket client**: Hook commands block Claude Code anyway; async adds complexity without benefit.
- **RAII FIFO cleanup**: `FifoGuard` ensures cleanup even on panic/error paths.
- **Boxed HookInput**: Reduces enum size for `ControlMessage` (HookInput is large).
- **Always serialize interrupts**: Haskell expects the field present, even if empty.
