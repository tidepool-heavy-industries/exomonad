# exomonad-proto (Rust)

Generated Rust types from protocol buffers using prost.

## Structure

```
rust/exomonad-proto/
├── Cargo.toml
├── CLAUDE.md
├── build.rs        # prost-build configuration
├── proto/          # Vendored proto files (for crates.io publishing)
│   ├── exomonad/   # Core types (ffi, common, hook, agent, popup)
│   └── effects/    # Effect message types (git, github, agent, etc.)
└── src/
    └── lib.rs      # Module declarations and re-exports
```

Generated code goes to `$OUT_DIR/exomonad.*.rs`.

## Code Generation

Types are generated at build time via `prost-build`:

```rust
// build.rs — uses vendored proto files within the crate
config.compile_protos(&[
    "proto/exomonad/ffi.proto",
    "proto/exomonad/common.proto",
    "proto/exomonad/hook.proto",
    "proto/exomonad/agent.proto",
    "proto/exomonad/popup.proto",
], &["proto/"])?;
```

Proto files are vendored into `proto/` (copied from the repo root `proto/` directory) so that `cargo publish` can include them in the crate tarball.

## JSON Wire Format

All types include serde derives for JSON compatibility:

```rust
config.type_attribute(".", "#[derive(serde::Serialize, serde::Deserialize)]");
```

### Enum Serialization

Enums use snake_case to match Haskell expectations:

```rust
config.type_attribute(".exo.ffi.ErrorCode", "#[serde(rename_all = \"snake_case\")]");
```

Example:
```json
{"code": "not_found"}
```

### Optional Fields

Optional fields are omitted when `None`:

```rust
config.field_attribute(
    ".exomonad.ffi.FfiError.context",
    "#[serde(skip_serializing_if = \"Option::is_none\")]",
);
```

## Features

- `default` - Only core FFI types
- `full` - All proto modules (common, hook, agent, popup)

## Module Reference

### ffi

Core FFI types:
- `ErrorCode` - Error classification
- `ErrorContext` - Rich debugging context
- `FfiError` - Structured error
- `FfiResult` - Success/error envelope with oneof

### common (feature = "full")

Shared primitives:
- `SessionId` - Session identifier
- `Role` - Agent role
- `ToolPermission` - Permission decision
- `GitBranch`, `GitCommit` - Git references
- `FilePath`, `DirectoryPath` - Path types
- `IssueRef`, `PullRequestRef` - GitHub references

### hook (feature = "full")

Claude Code hooks:
- `HookType` - Hook event type
- `StopDecision` - Stop decision
- Input/Output types for each hook

### agent (feature = "full")

Agent management:
- `AgentType` - Claude/Gemini
- `AgentStatus` - Running/stopped/failed
- `SpawnOptions`, `AgentInfo`, etc.

### popup (feature = "full")

UI popup protocol:
- `PopupDefinition` - Dialog specification
- `PopupComponent` - UI component (oneof)
- Component types and responses

## Usage

```rust
use exomonad_proto::ffi::{ErrorCode, FfiError, ErrorContext};

let err = FfiError {
    message: "Not found".into(),
    code: ErrorCode::NotFound as i32,
    context: None,
    suggestion: None,
};

// Serialize to JSON (snake_case enums)
let json = serde_json::to_string(&err)?;
// {"message":"Not found","code":"not_found"}
```

## Build

```bash
# Build (requires protoc)
cargo build -p exomonad-proto

# With nix (provides protoc)
nix develop -c bash -c "cd rust && cargo build -p exomonad-proto"
```

## Dependencies

- `prost` - Protobuf runtime
- `serde` - JSON serialization
- `serde_json` - JSON encoding

Build dependencies:
- `prost-build` - Code generation
- `protoc` (system) - Proto compiler
