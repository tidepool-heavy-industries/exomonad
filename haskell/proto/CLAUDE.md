# exomonad-proto (Haskell)

Generated Haskell types from protocol buffers using proto3-suite.

## Structure

```
haskell/proto/
├── exomonad-proto.cabal
├── CLAUDE.md
└── src/
    ├── ExoMonad/
    │   ├── Compat.hs       # Wire format compatibility layer (JSON)
    │   ├── Ffi.hs          # Generated from ffi.proto
    │   ├── Common.hs       # Generated from common.proto
    │   ├── Hook.hs         # Generated from hook.proto
    │   ├── Agent.hs        # Generated from agent.proto
    │   └── Popup.hs        # Generated from popup.proto
    └── Effects/
        ├── Agent.hs         # Generated from effects/agent.proto
        ├── Copilot.hs       # Generated from effects/copilot.proto
        ├── EffectError.hs   # Generated from effects/effect_error.proto
        ├── Egregore.hs      # Generated from effects/egregore.proto
        ├── Envelope.hs      # Generated from effects/envelope.proto
        ├── Events.hs        # Generated from effects/events.proto
        ├── FilePr.hs        # Generated from effects/file_pr.proto
        ├── Fs.hs            # Generated from effects/fs.proto
        ├── Git.hs           # Generated from effects/git.proto
        ├── Github.hs        # Generated from effects/github.proto
        ├── Kv.hs            # Generated from effects/kv.proto
        ├── Log.hs           # Generated from effects/log.proto
        ├── MergePr.hs       # Generated from effects/merge_pr.proto
        ├── Messaging.hs     # Generated from effects/messaging.proto
        ├── Popup.hs         # Generated from effects/popup.proto
        └── Session.hs       # Generated from effects/session.proto
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

Post-processing strips gRPC service code (not compatible with WASM) and handles macOS case-insensitive filesystem issues.

## Two Wire Formats

### Core Types (ExoMonad.*) — JSON

Core FFI types use JSON via JSONPB. The `ExoMonad.Compat` module handles enum format conversion between proto3-suite (SCREAMING_SNAKE_CASE) and prost+serde (snake_case).

```haskell
import ExoMonad.Compat (toWireJSON, fromWireJSON)
```

### Effect Types (Effects.*) — Protobuf Binary

Effect types use native protobuf binary encoding via `Proto3.Suite.Class`:

```haskell
import Proto3.Suite.Class (toLazyByteString, fromByteString)

-- Encode: Message a => a -> BL.ByteString
let bytes = toLazyByteString request

-- Decode: Message a => B.ByteString -> Either ParseError a
case fromByteString bytes of
  Right response -> ...
  Left err -> ...
```

Effects are dispatched through `EffectEnvelope` (wraps request with effect_type string) and `EffectResponse` (wraps response or error). See `ExoMonad.Guest.Effect` for the encoding layer.

## WASM Integration

Included in WASM builds via `cabal.project.wasm`:

```cabal
packages:
    haskell/proto
    haskell/wasm-guest
    haskell/vendor/proto3-runtime
    ...
```

Dependencies:
- `proto3-runtime` (vendored, provides Proto3.Suite.Class and JSONPB)
- `proto3-wire` (protobuf wire encoding, from hackage)
- `aeson` (JSON integration for core types)

## Module Reference

### ExoMonad.Ffi

Core FFI types:
- `ErrorCode` - Error classification enum
- `ErrorContext` - Rich debugging context
- `FfiError` - Structured error message
- `FfiResult` - Success/error envelope

### ExoMonad.Common

Shared primitives:
- `SessionId` - Session identifier
- `Role` - Agent role enum
- `ToolPermission` - Permission decision enum
- `GitBranch`, `GitCommit` - Git references
- `FilePath`, `DirectoryPath` - Path wrappers
- `IssueRef`, `PullRequestRef` - GitHub references

### ExoMonad.Hook

Claude Code hooks:
- `HookType` - Hook event type enum
- `StopDecision` - Stop decision enum
- `PreToolUseInput/Output` - Pre-tool hook
- `PostToolUseInput/Output` - Post-tool hook
- `SessionStartInput/Output` - Session start hook
- `SessionEndInput/Output` - Session end hook
- `SubagentStopInput/Output` - Subagent stop hook
- `HookInput/Output` - Generic envelopes

### ExoMonad.Agent

Agent management:
- `AgentType` - Claude/Gemini enum
- `AgentStatus` - Running/stopped/failed enum
- `SpawnOptions` - Agent creation parameters
- `AgentInfo` - Agent state
- `WorktreeInfo` - Git worktree state
- Batch operations: `SpawnAgentsRequest`, `CleanupAgentsRequest`, etc.

### ExoMonad.Popup

UI popup protocol:
- `PopupDefinition` - Dialog specification
- `PopupComponent` - UI component sum type
- Component types: `TextInput`, `Checkbox`, `Button`, etc.
- `FormSubmission`, `PopupResponse` - User interaction

### ExoMonad.Compat

Wire format compatibility (JSON only):
- `toWireJSON` - Encode with snake_case enums
- `fromWireJSON` - Decode from snake_case
- `protoToWire` - SCREAMING_SNAKE → snake_case
- `wireToProto` - snake_case → SCREAMING_SNAKE

### Effects.Envelope

Binary wire envelope:
- `EffectEnvelope` - `{ effectEnvelopeEffectType :: Text, effectEnvelopePayload :: ByteString }`
- `EffectResponse` - `{ effectResponseResult :: Maybe EffectResponseResult }`
- `EffectResponseResult` - `EffectResponseResultPayload ByteString | EffectResponseResultError EffectError`

### Effects.EffectError

Unified error type:
- `EffectError` - `{ effectErrorKind :: Maybe EffectErrorKind }`
- `EffectErrorKind` - Sum type: NotFound, InvalidInput, NetworkError, PermissionDenied, Timeout, Custom

### Effects.Git

Git operations (proto-generated request/response types):
- `GetBranchRequest/Response`, `GetStatusRequest/Response`
- `GetCommitsRequest/Response`, `HasUnpushedCommitsRequest/Response`
- `GetRemoteUrlRequest/Response`, `GetRepoInfoRequest/Response`
- `GetWorktreeRequest/Response`

### Effects.Github

GitHub API operations (proto-generated):
- `ListIssuesRequest/Response`, `GetIssueRequest/Response`
- `ListPullRequestsRequest/Response`, `GetPullRequestRequest/Response`
- `GetPullRequestForBranchRequest/Response`, `CreatePullRequestRequest/Response`

### Effects.Log

Logging operations (proto-generated):
- `InfoRequest`, `ErrorRequest`, `DebugRequest`, `WarnRequest` → `LogResponse`
- `EmitEventRequest` → `EmitEventResponse`

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

- `proto3-runtime` (vendored, GHC 9.12 compatible subset of proto3-suite)
- `proto3-wire` - Protobuf wire encoding
- `aeson >= 2.0` - JSON integration
- `text >= 2.0` - Text handling
- `vector` - Repeated fields
