# Parallelized Work Streams

Work structured as independent branches, each 2-4 days, mergeable in any order.

## Dependency Graph

```
                    ┌─────────────────────┐
                    │  A: Schema + Codegen │
                    │  (2 days)            │
                    └──────────┬──────────┘
                               │
           ┌───────────────────┼───────────────────┐
           │                   │                   │
           ▼                   ▼                   ▼
┌──────────────────┐ ┌──────────────────┐ ┌──────────────────┐
│ B: Rust Plugin   │ │ C: Haskell WASM  │ │ D: Rust Services │
│    Loader        │ │    Compilation   │ │    (parallel)    │
│ (2 days)         │ │ (3 days)         │ │ (2 days each)    │
└────────┬─────────┘ └────────┬─────────┘ └──────────────────┘
         │                    │                   │
         └────────────────────┼───────────────────┘
                              │
                              ▼
                    ┌─────────────────────┐
                    │  E: Integration     │
                    │  (2-3 days)         │
                    └─────────────────────┘
```

## Stream A: Schema + Codegen Foundation

**Branch**: `wasm/schema-foundation`
**Duration**: 2 days
**Dependencies**: None
**Deliverable**: `schema/` directory with working quicktype pipeline

### Tasks
1. Create `schema/` directory structure
2. Define `effects.json` with 5 core effects:
   - Log
   - GitGetBranch
   - GitGetWorktree
   - GitHubListIssues
   - DockerExec
3. Define `common.json` with shared types
4. Set up quicktype in justfile/Makefile
5. Verify generation to both Rust and Haskell
6. Add CI check for schema drift

### Acceptance Criteria
- `just gen-types` produces valid Rust + Haskell
- CI fails if generated files don't match schema
- Adjacently tagged encoding verified in both outputs

---

## Stream B: Rust Plugin Loader

**Branch**: `wasm/rust-plugin-loader`
**Duration**: 2 days
**Dependencies**: A (can start with stub schema)
**Deliverable**: `rust/exomonad-runtime` with Extism plugin loading

### Tasks
1. Create `exomonad-runtime` crate
2. Add extism, tokio, axum dependencies
3. Implement `PluginManager`:
   - Load WASM file
   - Call exported functions
   - Hot reload support
4. Implement stub host functions (return dummy data)
5. Basic HTTP server with `/health` and `/call` endpoints
6. Test with any valid WASM (can use extism example)

### Acceptance Criteria
- Can load a WASM file
- Can call an exported function and get JSON back
- Hot reload endpoint works
- Logging shows host function calls

---

## Stream C: Haskell WASM Compilation

**Branch**: `wasm/haskell-guest`
**Duration**: 3 days
**Dependencies**: A (for generated types)
**Deliverable**: `haskell/wasm-guest` compiling to reactor WASM

### Tasks
1. Create `wasm-guest` package
2. Configure cabal for WASM:
   - `-no-hs-main`
   - `-optl-mexec-model=reactor`
   - Export flags
3. Add extism-pdk dependency
4. Implement entry points:
   - `handle_mcp_call`
   - `handle_pre_tool_use`
5. Implement host call FFI wrappers
6. Implement freer-simple interpreter calling host
7. Verify compilation produces valid WASM

### Acceptance Criteria
- `wasm32-wasi-cabal build` succeeds
- Output is a reactor (not command) module
- Exports are visible: `wasm-objdump -x plugin.wasm | grep Export`
- Can be loaded by Extism (even if host functions missing)

---

## Stream D: Rust Services (Parallelizable)

Each service is independent. Can run multiple in parallel.

### D1: Git Service
**Branch**: `wasm/rust-git-service`
**Duration**: 2 days

- `GitService` struct
- `get_branch`, `get_worktree`, `get_dirty_files`
- Uses docker exec (shell out to docker-ctl or use bollard)
- Host function registration

### D2: GitHub Service
**Branch**: `wasm/rust-github-service`
**Duration**: 2 days

- `GitHubService` struct with octocrab
- `list_issues`, `get_issue`, `create_pr`
- Host function registration
- Auth via env var

### D3: Docker Service
**Branch**: `wasm/rust-docker-service`
**Duration**: 2 days

- `DockerService` struct
- `exec`, `spawn`, `kill`
- Either shell to docker-ctl or use bollard
- Host function registration

### D4: Log/Telemetry Service
**Branch**: `wasm/rust-log-service`
**Duration**: 1 day

- `LogService` with tracing
- `log_info`, `log_error`, `emit_event`
- Fire-and-forget host functions
- OTLP export (optional)

---

## Stream E: Integration

**Branch**: `wasm/integration`
**Duration**: 2-3 days
**Dependencies**: B, C, at least one of D*
**Deliverable**: Working end-to-end MCP call

### Tasks
1. Wire Haskell WASM into Rust loader
2. Connect host functions to services
3. Test MCP call end-to-end:
   - HTTP request → Rust
   - Rust calls WASM
   - WASM calls host function
   - Rust executes service
   - Response returns through chain
4. Add integration tests
5. Update docker-compose for new runtime

### Acceptance Criteria
- `curl localhost:7432/role/tl/mcp/call` with `file_pr` works
- Logs show full request flow
- No Haskell IO code executed (all via host functions)

---

## Optional Parallel Streams

### F: Contract Testing
**Branch**: `wasm/contract-tests`
**Duration**: 2 days
**Dependencies**: A

- Property-based tests for serialization roundtrip
- proptest generating random Effect values
- Verify JSON parses correctly on both sides

### G: Handler Migration
**Branch**: `wasm/migrate-handlers`
**Duration**: 3-4 days
**Dependencies**: C

- Move existing Handler logic from control-server
- Adapt to use HostEffect instead of IO interpreters
- Keep same business logic

---

## Suggested Parallelization

**Week 1**:
- Agent 1: Stream A (Schema)
- Agent 2: Stream B (Rust Loader) - can use stub types initially

**Week 2**:
- Agent 1: Stream C (Haskell WASM)
- Agent 2: Stream D1 + D4 (Git + Log services)
- Agent 3: Stream D2 (GitHub service)

**Week 3**:
- Agent 1: Stream E (Integration)
- Agent 2: Stream G (Handler migration)
- Agent 3: Stream F (Contract tests)

---

## Branch Naming

```
wasm/schema-foundation
wasm/rust-plugin-loader
wasm/haskell-guest
wasm/rust-git-service
wasm/rust-github-service
wasm/rust-docker-service
wasm/rust-log-service
wasm/integration
wasm/contract-tests
wasm/migrate-handlers
```

## Merge Order

1. A (Schema) - first, everything depends on it
2. B, C, D* - any order, independent
3. E (Integration) - after B + C + at least one D
4. F, G - after E
