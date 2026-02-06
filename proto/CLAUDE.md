# Protocol Buffers

Single source of truth for types that cross the Rust/Haskell WASM boundary.

## Structure

```
proto/
└── exomonad/
    ├── ffi.proto       # Core FFI types (ErrorCode, FfiError, FfiResult)
    ├── common.proto    # Shared primitives (SessionId, Role, etc.)
    ├── hook.proto      # Claude Code hook types
    ├── agent.proto     # Agent management types
    └── popup.proto     # UI popup types
```

## Codegen

Both Rust and Haskell types are generated from `.proto` files:

| Language | Generator | Output |
|----------|-----------|--------|
| Rust | `prost-build` | `rust/exomonad-proto/` (build.rs) |
| Haskell | `proto3-suite compile-proto-file` | `haskell/proto/src/Proto/` (via just) |

**Generate both:**
```bash
just proto-gen
```

### Haskell Codegen

Uses `proto3-suite` (GHC 9.12 compatible):
- `compile-proto-file` generates pure Haskell code
- Types include `ToJSONPB`/`FromJSONPB` instances
- Compatible with WASM builds

### Rust Codegen

Uses `prost-build` in `build.rs`:
- Generated at build time
- Output in `OUT_DIR` (not checked in)
- JSON wire format via serde derives

## Wire Format

Types use JSON for cross-language compatibility (not protobuf binary):

### Enum Serialization

**Rust (prost + serde):** snake_case
```json
{"code": "not_found"}
```

**Haskell (proto3-suite):** SCREAMING_SNAKE_CASE
```json
{"code": "ERROR_CODE_NOT_FOUND"}
```

**Compatibility:** Use `Proto.Compat` module to convert between formats:
```haskell
import Proto.Compat (toWireJSON, fromWireJSON, protoToWire)

-- Converts SCREAMING_SNAKE_CASE to snake_case
protoToWire "ERROR_CODE_NOT_FOUND" == "not_found"
```

### Optional Fields

Both sides omit `None` values:
```json
{"message": "error", "code": "not_found"}
```
(context and suggestion omitted when None)

### Oneof Encoding

Proto `oneof` fields use nested objects:
```json
{"result": {"successPayload": "..."}}
```

## Proto File Reference

### ffi.proto

Core FFI boundary types:
- `ErrorCode` (enum): Error classification
- `ErrorContext` (message): Debugging context
- `FfiError` (message): Structured error
- `FfiResult` (message): Success/error envelope with oneof

### common.proto

Shared primitives:
- `SessionId`: Unique session identifier
- `Role` (enum): Agent role (dev, tl, pm, etc.)
- `ToolPermission` (enum): Allow/deny/ask
- `GitBranch`, `GitCommit`: Git references
- `FilePath`, `DirectoryPath`: Path types
- `IssueRef`, `PullRequestRef`: GitHub references

### hook.proto

Claude Code hooks:
- `HookType` (enum): Pre/post tool use, session events
- `StopDecision` (enum): Allow/block/ask
- `PreToolUseInput/Output`: Pre-execution hook
- `PostToolUseInput/Output`: Post-execution hook
- `SessionStartInput/Output`: Session initialization
- `SessionEndInput/Output`: Session cleanup
- `SubagentStopInput/Output`: Subagent termination
- `HookInput/Output`: Generic envelopes

### agent.proto

Agent management:
- `AgentType` (enum): Claude/Gemini
- `AgentStatus` (enum): Running/stopped/failed
- `SpawnOptions`: Agent creation parameters
- `AgentInfo`: Agent state and metadata
- `WorktreeInfo`: Git worktree state
- `IssueAssignment`: Issue-to-agent mapping
- `SpawnAgentsRequest/Result`: Batch spawning
- `CleanupAgentsRequest/Result`: Batch cleanup
- `ListAgentsRequest/Response`: Active agent listing

### popup.proto

UI popup protocol:
- `PopupDefinition`: Dialog specification
- `PopupComponent`: UI component (oneof)
- `TextInput`, `TextArea`, `Checkbox`, etc.
- `FormSubmission`: User submission
- `PopupResponse`: Response envelope
- `VisibilityRule`: Conditional display

## Usage

### Rust

```rust
use exomonad_proto::ffi::{ErrorCode, FfiError, ErrorContext};

let err = FfiError {
    message: "Not found".into(),
    code: ErrorCode::NotFound as i32,
    context: None,
    suggestion: None,
};
```

### Haskell

```haskell
import Proto.Exomonad.Ffi
import Proto.Compat (toWireJSON)

-- Create error using proto3-suite types
let err = FfiError
    { ffiErrorMessage = "Not found"
    , ffiErrorCode = ErrorCodeNotFound
    , ffiErrorContext = Nothing
    , ffiErrorSuggestion = Nothing
    }

-- Convert to wire format (snake_case enums)
let json = toWireJSON err
```

## Adding New Types

1. Edit `proto/exomonad/*.proto`
2. Run `just proto-gen`
3. Add serde attributes in `rust/exomonad-proto/build.rs` if needed
4. Update `Proto.Compat` if new enums need wire format conversion
5. Add wire format test in `rust/exomonad-proto/tests/wire_compat.rs`
6. Verify: `just proto-test`

## Build Commands

```bash
just proto-gen   # Generate Rust + Haskell from proto
just proto-test  # Run wire format compatibility tests
```

## Migration Status

| Proto File | Rust | Haskell | Wire Tests |
|------------|------|---------|------------|
| ffi.proto | ✅ | ✅ | ✅ |
| common.proto | ✅ | ✅ | Pending |
| hook.proto | ✅ | ✅ | Pending |
| agent.proto | ✅ | ✅ | Pending |
| popup.proto | ✅ | ✅ | Pending |

## Related Files

- `haskell/proto/CLAUDE.md` - Haskell proto package details
- `rust/exomonad-proto/CLAUDE.md` - Rust proto crate details
- `haskell/wasm-guest/src/ExoMonad/Guest/FFI.hs` - FFI boundary (to be migrated)
