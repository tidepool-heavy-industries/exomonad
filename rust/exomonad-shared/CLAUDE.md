# exomonad-shared: Shared Types and Protocols

Shared library for the ExoMonad workspace. Provides protocol types, domain validation, and common utilities.

## Module Overview

| Module | Purpose |
|--------|---------|
| `protocol` | FFI and MCP protocol types (JSON-RPC, Hook events) |
| `domain` | Domain types with parse-at-edge validation (SessionId, ToolName, Role, etc.) |
| `ffi` | Core FFI types shared with Haskell WASM |
| `error` | Typed error types (`ExoMonadError`) |
| `hooks` | Hook configuration generator for Claude Code/Gemini |
| `logging` | Tracing/logging setup with env filter |
| `telemetry` | Real-time progress and event tracking types |
| `util` | Shell quoting, binary path resolution |

## Key Concepts

### Protocol Types (`protocol/`)

**HookInput** - Unified input for Claude Code and Gemini hooks:
```rust
pub struct HookInput {
    pub session_id: String,
    pub hook_event_name: HookEventType,
    pub tool_name: Option<String>,
    pub tool_input: Option<Value>,
    // ...
}
```

**HookSpecificOutput** - Responses returned to the CLI host:
```rust
pub enum HookSpecificOutput {
    PreToolUse {
        permission_decision: PermissionDecision,
        permission_decision_reason: Option<String>,
        updated_input: Option<Value>,
    },
    // ...
}
```

### Domain Types (`domain.rs`)

Strongly typed wrappers for string identifiers with validation at the edge:

- `SessionId` - Non-empty string
- `ToolName` - Identifier for MCP tools
- `Role` - Agent role (dev, tl, pm)
- `GithubOwner` / `GithubRepo` - GitHub identifiers
- `IssueNumber` - Positive integer
- `ToolPermission` - allow, deny, ask
- `AbsolutePath` / `WasmPath` - Validated filesystem paths

### FFI Boundary (`ffi.rs`)

Types that must match exactly between Rust and Haskell WASM (FFI):

- `FfiResult<T>` - Result type for FFI calls
- `FfiError` - Detailed error information (code, message, context)
- `ErrorCode` - Stable error codes across the FFI boundary

## Re-exports

The crate root re-exports commonly used types:

```rust
pub use domain::*;
pub use error::{ExoMonadError, Result};
pub use ffi::{ErrorCode, ErrorContext, FfiError, FfiResult};
pub use protocol::*;
pub use logging::{init_logging, init_logging_with_default};
pub use util::{shell_quote, find_exomonad_binary};
```

## Usage Example

```rust
use exomonad_shared::{SessionId, ToolName, ToolPermission};

let session = SessionId::try_from("session-123".to_string())?;
let tool = ToolName::try_from("git_branch".to_string())?;
let perm = ToolPermission::Allow;
```

## Testing

```bash
cargo test -p exomonad-shared
```