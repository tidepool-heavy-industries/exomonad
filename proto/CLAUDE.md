# Protocol Buffers

Single source of truth for types that cross the Rust/Haskell WASM boundary.

## Structure

```
proto/
├── exomonad/           # Core boundary types
│   ├── ffi.proto       # Core FFI types (ErrorCode, FfiError, FfiResult)
│   ├── common.proto    # Shared primitives (SessionId, Role, etc.)
│   ├── hook.proto      # Claude Code hook types
│   ├── agent.proto     # Agent management types (legacy)
│   └── popup.proto     # UI popup types
└── effects/            # Extensible effects system (namespace-based)
    ├── agent.proto         # agent.* effects
    ├── coordination.proto  # coordination.* effects
    ├── copilot.proto       # copilot.* effects
    ├── effect_error.proto  # Unified effect error type
    ├── egregore.proto      # egregore.* effects (experimental)
    ├── envelope.proto      # Wire envelope types
    ├── events.proto        # events.* effects
    ├── file_pr.proto       # file_pr.* effects
    ├── fs.proto            # fs.* effects
    ├── git.proto           # git.* effects
    ├── github.proto        # github.* effects
    ├── jj.proto            # jj.* effects (Jujutsu VCS)
    ├── kv.proto            # kv.* effects (Key-Value)
    ├── log.proto           # log.* effects
    ├── merge_pr.proto      # merge_pr.* effects
    ├── messaging.proto     # messaging.* effects
    ├── popup.proto         # popup.* effects (UI)
    └── session.proto       # session.* effects
```

## Codegen

Both Rust and Haskell types are generated from `.proto` files:

| Language | Generator | Output |
|----------|-----------|--------|
| Rust (core) | `prost-build` | `rust/exomonad-proto/` (build.rs) |
| Rust (effects) | `prost-build` + `exomonad-core/build.rs` | Typed effect traits + binary dispatch |
| Haskell | `proto3-suite compile-proto-file` | `haskell/proto/src/` (via generate.sh) |

**Generate Haskell types:**
```bash
just proto-gen
```

### Haskell Codegen

Uses `proto3-suite` (GHC 9.12 compatible):
- `compile-proto-file` generates pure Haskell code
- Types include `Message`, `ToJSONPB`/`FromJSONPB`, `Named`, `HasDefault` instances
- Compatible with WASM builds
- Post-processing strips gRPC service code (not compatible with WASM)
- Handles macOS case-insensitive filesystem for module paths

### Rust Codegen

Two build.rs files:
- **`exomonad-proto/build.rs`**: Compiles all proto files with prost-build
- **`exomonad-core/build.rs`**: Generates typed effect traits (`GitEffects`, `GitHubEffects`, etc.) and binary dispatch functions from proto service definitions

## Wire Formats

Two wire formats are in use:

### Core Types (exomonad/) — JSON

Core FFI types use JSON via proto3-suite's JSONPB encoding for cross-language compatibility.

**Enum Serialization:**
- **Rust (prost + serde):** snake_case (`"not_found"`)
- **Haskell (proto3-suite):** SCREAMING_SNAKE_CASE (`"ERROR_CODE_NOT_FOUND"`)
- **Compatibility:** Use `ExoMonad.Compat` module to convert between formats

### Effects (effects/) — Protobuf Binary

Effect types use protobuf binary encoding via `EffectEnvelope`/`EffectResponse`:

```
Haskell: runEffect @GitGetBranch req
  → EffectEnvelope { effect_type="git.get_branch", payload=encode(req) }
  → protobuf binary bytes via yield_effect host function
  → Rust: EffectEnvelope::decode → dispatch → encode response
  → EffectResponse { result: Payload(bytes) | Error(EffectError) }
```

Both sides use proto-generated types with native protobuf `Message` encode/decode:
- **Haskell:** `Proto3.Suite.Class.toLazyByteString` / `fromByteString`
- **Rust:** `prost::Message::encode_to_vec` / `decode`

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

## Effects Proto Reference

The `effects/` directory defines the extensible effects system. Each file defines effects for a namespace (e.g., `git.proto` defines `git.get_branch`, `git.get_status`, etc.). Proto `service` definitions are used by Rust codegen to generate typed traits and dispatch functions.

### effects/envelope.proto

Wire envelope for the single `yield_effect` host function:
- `EffectEnvelope`: `{ effect_type: string, payload: bytes }` — wraps effect requests
- `EffectResponse`: `oneof { payload: bytes, error: EffectError }` — wraps responses

### effects/effect_error.proto

Unified error type for all effects:
- `EffectError` (oneof): NotFound, InvalidInput, NetworkError, PermissionDenied, Timeout, Custom
- `EffectResult`: Success/error envelope for effect responses

### effects/git.proto

Git operations (`git.*` namespace):
- `GetBranch`: Current branch info
- `GetStatus`: Dirty/staged/untracked files
- `GetCommits`: Recent commit history
- `HasUnpushedCommits`: Check for unpushed commits
- `GetRemoteUrl`: Remote URL
- `GetRepoInfo`: Branch + owner/name
- `GetWorktree`: Worktree info

### effects/github.proto

GitHub API operations (`github.*` namespace):
- `ListIssues`: List repository issues
- `GetIssue`: Get issue with comments
- `ListPullRequests`: List PRs
- `GetPullRequest`: Get PR with reviews
- `GetPullRequestForBranch`: Find PR for branch
- `CreatePullRequest`: Create new PR
- `GetPullRequestReviewComments`: Get inline comments

### effects/jj.proto

Jujutsu VCS operations (`jj.*` namespace):
- `BookmarkCreate`: Create bookmark at revision
- `GitPush`: Push bookmark to remote
- `GitFetch`: Fetch from remote (triggers auto-rebase)
- `Log`: Query revsets, return JSON
- `New`: Start a new change
- `Status`: Working copy status including conflicts

### effects/fs.proto

Filesystem operations (`fs.*` namespace):
- `ReadFile`: Read file contents
- `WriteFile`: Write file contents
- `FileExists`: Check file existence
- `ListDirectory`: List directory entries
- `DeleteFile`: Delete file/directory

### effects/agent.proto

Agent lifecycle (`agent.*` namespace):
- `Spawn`: Spawn single agent
- `SpawnBatch`: Spawn multiple agents
- `Cleanup`: Clean up single agent
- `CleanupBatch`: Clean up multiple agents
- `CleanupMerged`: Clean up agents with merged branches
- `List`: List active agents

### effects/coordination.proto

Task coordination (`coordination.*` namespace):
- `CreateTask`, `UpdateTask`: Manage tasks
- `ListTasks`, `GetTask`: Query tasks
- `SendMessage`, `GetMessages`: Messaging integration

### effects/copilot.proto

Copilot integration (`copilot.*` namespace):
- `WaitForReview`: Poll for Copilot review completion

### effects/events.proto

Event system (`events.*` namespace):
- `WaitForEvent`: Block until event occurs
- `NotifyEvent`: Emit event
- `NotifyParent`: Signal completion to parent

### effects/file_pr.proto

PR creation (`file_pr.*` namespace):
- `FilePR`: Create or update PR for current branch

### effects/kv.proto

Key-Value storage (`kv.*` namespace):
- `Get`: Retrieve value by key
- `Set`: Store value by key

### effects/log.proto

Logging and events (`log.*` namespace):
- `Log`: Generic log with level
- `Debug`, `Info`, `Warn`, `Error`: Convenience methods
- `EmitEvent`: Emit structured event

### effects/merge_pr.proto

PR merging (`merge_pr.*` namespace):
- `MergePR`: Merge PR and fetch changes

### effects/messaging.proto

Inter-agent messaging (`messaging.*` namespace):
- `SendNote`: Send non-blocking note
- `SendQuestion`: Send blocking question

### effects/popup.proto

UI popup interaction (`popup.*` namespace):
- `ShowPopup`: Display interactive UI component

### effects/session.proto

Session management (`session.*` namespace):
- `RegisterClaudeSession`: Link Claude session ID to agent identity

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

### Core Types (exomonad/) — JSON wire format

| Proto File | Rust | Haskell | Wire Tests |
|------------|------|---------|------------|
| ffi.proto | ✅ | ✅ | ✅ |
| common.proto | ✅ | ✅ | ✅ |
| hook.proto | ✅ | ✅ | ✅ |
| agent.proto | ✅ | ✅ | ✅ |
| popup.proto | ✅ | ✅ | ✅ |

### Effects Types (effects/) — Protobuf binary wire format

| Proto File | Rust Handler | Haskell Effect | Wire Tests |
|------------|--------------|----------------|------------|
| envelope.proto | ✅ | ✅ | ✅ |
| effect_error.proto | ✅ | ✅ | ✅ |
| git.proto | ✅ | ✅ | ✅ |
| github.proto | ✅ | ✅ | ✅ |
| log.proto | ✅ | ✅ | ✅ |
| fs.proto | ✅ | ✅ | ✅ |
| agent.proto | ✅ | ✅ | ✅ |
| jj.proto | ✅ | ✅ | ✅ |
| session.proto | ✅ | ✅ | ✅ |
| coordination.proto | ✅ | ✅ | ✅ |
| copilot.proto | ✅ | ✅ | ✅ |
| events.proto | ✅ | ✅ | ✅ |
| file_pr.proto | ✅ | ✅ | ✅ |
| kv.proto | ✅ | ✅ | ✅ |
| merge_pr.proto | ✅ | ✅ | ✅ |
| messaging.proto | ✅ | ✅ | ✅ |
| popup.proto | ✅ | ✅ | ✅ |

## Related Files

- `haskell/proto/CLAUDE.md` - Haskell proto package details
- `rust/exomonad-proto/CLAUDE.md` - Rust proto crate details
- `haskell/wasm-guest/src/ExoMonad/Effect/Class.hs` - Effect typeclass (Message constraints)
- `haskell/wasm-guest/src/ExoMonad/Guest/Effect.hs` - Binary envelope encoding/decoding