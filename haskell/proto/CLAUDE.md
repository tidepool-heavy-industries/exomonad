# exomonad-proto (Haskell)

Generated Haskell types from protocol buffers using proto3-suite.

## Structure

```
haskell/proto/
├── exomonad-proto.cabal
├── CLAUDE.md
└── src/
    └── Proto/
        ├── Compat.hs           # Wire format compatibility layer
        └── Exomonad/
            ├── Ffi.hs          # Generated from ffi.proto
            ├── Common.hs       # Generated from common.proto
            ├── Hook.hs         # Generated from hook.proto
            ├── Agent.hs        # Generated from agent.proto
            └── Popup.hs        # Generated from popup.proto
```

## Code Generation

Types are generated via `proto3-suite`'s `compile-proto-file`:

```bash
just proto-gen
```

Generated files include:
- Message types with record fields
- Enum types with `Bounded`/`Enum` instances
- `ToJSONPB`/`FromJSONPB` instances for JSON serialization
- `Message` instances for protobuf binary encoding

## Wire Format Compatibility

proto3-suite uses SCREAMING_SNAKE_CASE for enum JSON values, but prost+serde uses snake_case. The `Proto.Compat` module provides conversion:

```haskell
import Proto.Compat (toWireJSON, fromWireJSON)

-- Encode for Rust consumption (converts to snake_case)
toWireJSON :: ToJSONPB a => a -> ByteString

-- Decode from Rust (converts from snake_case)
fromWireJSON :: FromJSONPB a => ByteString -> Either String a
```

### Enum Conversion

```haskell
import Proto.Compat (protoToWire, wireToProto)

-- SCREAMING_SNAKE_CASE -> snake_case
protoToWire "ERROR_CODE_NOT_FOUND" == "not_found"

-- snake_case -> SCREAMING_SNAKE_CASE (with prefix)
wireToProto "ERROR_CODE" "not_found" == "ERROR_CODE_NOT_FOUND"
```

## WASM Integration

This package is included in WASM builds via `cabal.project.wasm`:

```cabal
packages:
    haskell/proto
    haskell/wasm-guest
    ...
```

Dependencies:
- `proto3-suite` (runtime)
- `proto3-wire` (binary encoding)
- `aeson` (JSON integration)

## Usage in wasm-guest

```haskell
-- Import generated types
import Proto.Exomonad.Ffi (ErrorCode(..), FfiError(..), ErrorContext(..))
import Proto.Compat (toWireJSON)

-- Use with wire format conversion
mkError :: Text -> ErrorCode -> ByteString
mkError msg code = toWireJSON $ FfiError
    { ffiErrorMessage = msg
    , ffiErrorCode = code
    , ffiErrorContext = Nothing
    , ffiErrorSuggestion = Nothing
    }
```

## Module Reference

### Proto.Exomonad.Ffi

Core FFI types:
- `ErrorCode` - Error classification enum
- `ErrorContext` - Rich debugging context
- `FfiError` - Structured error message
- `FfiResult` - Success/error envelope

### Proto.Exomonad.Common

Shared primitives:
- `SessionId` - Session identifier
- `Role` - Agent role enum
- `ToolPermission` - Permission decision enum
- `GitBranch`, `GitCommit` - Git references
- `FilePath`, `DirectoryPath` - Path wrappers
- `IssueRef`, `PullRequestRef` - GitHub references

### Proto.Exomonad.Hook

Claude Code hooks:
- `HookType` - Hook event type enum
- `StopDecision` - Stop decision enum
- `PreToolUseInput/Output` - Pre-tool hook
- `PostToolUseInput/Output` - Post-tool hook
- `SessionStartInput/Output` - Session start hook
- `SessionEndInput/Output` - Session end hook
- `SubagentStopInput/Output` - Subagent stop hook
- `HookInput/Output` - Generic envelopes

### Proto.Exomonad.Agent

Agent management:
- `AgentType` - Claude/Gemini enum
- `AgentStatus` - Running/stopped/failed enum
- `SpawnOptions` - Agent creation parameters
- `AgentInfo` - Agent state
- `WorktreeInfo` - Git worktree state
- Batch operations: `SpawnAgentsRequest`, `CleanupAgentsRequest`, etc.

### Proto.Exomonad.Popup

UI popup protocol:
- `PopupDefinition` - Dialog specification
- `PopupComponent` - UI component sum type
- Component types: `TextInput`, `Checkbox`, `Button`, etc.
- `FormSubmission`, `PopupResponse` - User interaction

### Proto.Compat

Wire format compatibility:
- `toWireJSON` - Encode with snake_case enums
- `fromWireJSON` - Decode from snake_case
- `protoToWire` - SCREAMING_SNAKE → snake_case
- `wireToProto` - snake_case → SCREAMING_SNAKE

## Build Commands

```bash
# Regenerate from proto files
just proto-gen

# Build
cabal build exomonad-proto

# Test wire format compatibility
just proto-test
```

## Dependencies

- `proto3-suite >= 0.9.4` - GHC 9.12 compatible
- `proto3-wire` - Protobuf wire encoding
- `aeson >= 2.0` - JSON integration
- `text >= 2.0` - Text handling
- `vector` - Repeated fields
