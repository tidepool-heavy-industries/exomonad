# ExoMonad WASM Migration Plan

## Vision

Compile Haskell business logic to WASM, embed in Rust runtime. Rust becomes the "thick" service layer handling all IO; Haskell becomes a pure decision engine emitting effects.

```
┌─────────────────────────────────────────────────────────────────┐
│ Rust Runtime (Extism Host)                                      │
│                                                                 │
│  Services: Git, GitHub, Docker, LLM, LSP, TUI, Telemetry        │
│  HTTP server, async runtime, socket handling                    │
│                                                                 │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │ Host Functions (effect interpreters)                      │  │
│  │   git_get_branch, github_create_pr, llm_call, ...         │  │
│  └───────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
                              ▲
                              │ Extism host function calls
                              │
┌─────────────────────────────────────────────────────────────────┐
│ Haskell WASM Plugin (Extism Guest)                              │
│                                                                 │
│  Entry points: handle_mcp_call, handle_pre_tool_use, ...        │
│  Pure logic: role dispatch, permission checks, orchestration    │
│  Effects as data: yields Effect ADT, receives results           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## Goals

1. **Fast iteration**: Change Haskell logic → recompile WASM (seconds) → hot reload
2. **Clear separation**: Haskell = decisions, Rust = execution
3. **Type safety**: Schema-first with quicktype, adjacently-tagged enums
4. **Incremental migration**: Move one interpreter at a time from Haskell to Rust

## Architecture Components

### 1. Schema Layer (`schema/`)
- JSON Schema definitions for all effect types
- Single source of truth for Rust ↔ Haskell boundary
- quicktype generates both sides

### 2. Rust Host (`rust/exomonad-runtime/`)
- Extism plugin loader
- HTTP/socket server (current control-server responsibilities)
- Host function implementations (effect interpreters)
- Async runtime (Tokio) with spawn_blocking bridge

### 3. Haskell Guest (`haskell/wasm-guest/`)
- Compiles to WASM via wasm32-wasi-ghc
- Reactor model (multiple entry points, persistent state)
- freer-simple effects interpreted via Extism host calls
- Generated types from schema

### 4. Build System
- GHC WASM flags: `-no-hs-main`, `-optl-mexec-model=reactor`
- quicktype in CI: regenerate on schema change
- Contract tests: property-based fuzzing across boundary

## Entry Points (Haskell exports)

| Export | Input | Output | Purpose |
|--------|-------|--------|---------|
| `handle_mcp_call` | McpRequest | McpResponse | MCP tool execution |
| `handle_pre_tool_use` | HookInput | PreToolUseResponse | Allow/deny/transform |
| `handle_post_tool_use` | HookInput | PostToolUseResponse | Logging, side effects |
| `handle_session_start` | HookInput | SessionStartResponse | Context injection |
| `handle_session_end` | HookInput | () | Cleanup, shipping |

## Effect Types (Host Functions)

| Effect | Host Function | Rust Implementation |
|--------|---------------|---------------------|
| `GitGetBranch` | `git_get_branch` | docker exec + git |
| `GitGetWorktree` | `git_get_worktree` | docker exec + git |
| `GitHubListIssues` | `github_list_issues` | octocrab |
| `GitHubCreatePR` | `github_create_pr` | octocrab |
| `LLMCall` | `llm_call` | anthropic client |
| `LSPHover` | `lsp_hover` | tower-lsp client |
| `DockerSpawn` | `docker_spawn` | docker-ctl |
| `DockerExec` | `docker_exec` | docker-ctl |
| `TUIPopup` | `tui_popup` | tui-spawner |
| `Log` | `log_info` | tracing |
| `Emit` | `emit_event` | OTLP |

## Migration Strategy

### Phase 1: Spike
- Minimal Haskell Extism plugin (echo)
- Load from Rust, call, verify roundtrip
- Validate GHC WASM + Extism PDK compatibility

### Phase 2: Schema Foundation
- Define `schema/effects.json` for core effects
- Set up quicktype generation pipeline
- Add contract tests

### Phase 3: Single Effect Migration
- Pick one effect (e.g., `Log`)
- Implement host function in Rust
- Wire up in Haskell guest
- Verify end-to-end

### Phase 4: Incremental Migration
- Move effects one at a time
- Each effect: Rust host function + Haskell caller
- Maintain parallel Haskell-native path during transition

### Phase 5: Full Cutover
- All effects via Extism
- Remove Haskell IO interpreters
- Rust runtime is sole IO executor

## Resolved Questions

1. **Streaming**: Out of scope. Non-streaming LLM calls only, focus on tools.
2. **Cloudflare**: Vestigial. Mine it for patterns but no compatibility constraint.
3. **State**: Use Extism Var system (idiomatic key-value store persisting across calls).
4. **Error handling**: Use Result ADT. Effects return `Either Error a`, handlers propagate explicitly.

## Open Questions

1. **HLS**: LSP session is long-lived. Does this stay in Rust entirely?

## Documentation

```
wasm-plan/
├── 00-overview.md          ← You are here (vision + goals)
├── 01-schema-design.md     ← Effect type schemas, quicktype, adjacently tagged
├── 02-rust-host.md         ← Extism host, services, host functions
├── 03-haskell-guest.md     ← WASM compilation, reactor model, PDK
├── 04-build-system.md      ← GHC flags, justfile, CI pipeline
├── 05-testing.md           ← Contract tests, property fuzzing, integration
├── 06-work-streams.md      ← Parallelized branches (2-4 days each)
└── 07-implementation-details.md ← Research findings, gotchas, code patterns
```

## Quick Links

| I want to... | Read |
|--------------|------|
| Understand the tagging convention | [01-schema-design.md](01-schema-design.md) |
| See Rust service implementations | [02-rust-host.md](02-rust-host.md) |
| Understand reactor model | [03-haskell-guest.md](03-haskell-guest.md) |
| Set up GHC WASM compilation | [04-build-system.md](04-build-system.md) |
| Write contract tests | [05-testing.md](05-testing.md) |
| Pick up a work stream | [06-work-streams.md](06-work-streams.md) |
| See concrete code patterns | [07-implementation-details.md](07-implementation-details.md) |
