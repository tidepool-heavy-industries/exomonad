# ExoMonad - Copilot Instructions

## Project Overview

ExoMonad is a heterogeneous LLM agent orchestration system. Haskell WASM is a typed configuration DSL — tool schemas, handlers, hooks, event routing — with the full power of a type system and effect system. Rust executes the I/O effects that the DSL yields.

## Architecture

**Split architecture: Haskell WASM (logic) + Rust (I/O)**

1. **Haskell WASM guest** — All MCP tool definitions, hooks, and event handlers. Uses freer-simple for reified continuations (yield/resume across WASM FFI). Compiled to WASM32-WASI via GHC 9.12, loaded by Rust at runtime via Extism.

2. **Rust host** — Executes effects yielded by the WASM guest. Effect handlers for Git, GitHub API, filesystem, tmux CLI, agent spawning, PR workflows. All I/O happens here.

3. **Protobuf FFI boundary** — All WASM↔Rust communication uses protobuf binary encoding via a single `yield_effect` host function. Proto files in `proto/` are the source of truth for request/response types.

## Project Structure

```
haskell/wasm-guest/         # WASM guest SDK (freer-simple effects, tool/hook framework)
haskell/proto/              # Generated Haskell proto types
.exo/roles/devswarm/        # Role definitions (TL, Dev, Worker)
.exo/lib/                   # Shared Haskell code across roles
rust/exomonad/              # Rust binary (MCP server, hook handler, CLI)
rust/exomonad-core/         # Framework, effect handlers, services, protocol types
rust/exomonad-proto/        # Proto-generated Rust types (prost)
proto/                      # Proto definitions (FFI boundary)
```

## Key Patterns

### Tool Definitions (Haskell)

Tools use a `mode :- Tool` record pattern for type-safe schema generation and handler dispatch:

```haskell
data Tools mode = Tools
  { spawn :: SpawnTools mode
  , pr :: FilePRTools mode
  , notifyParent :: mode :- NotifyParent
  } deriving Generic

config :: RoleConfig (Tools AsHandler)
config = RoleConfig { roleName = "tl", tools = Tools { ... }, hooks = ... }
```

### Effect System (Haskell)

Effects are phantom types with `Effect` instances mapping to protobuf types:

```haskell
data GitGetBranch
instance Effect GitGetBranch where
  type Input GitGetBranch = GetBranchRequest
  type Output GitGetBranch = GetBranchResponse
  effectId = "git.get_branch"
```

### Effect Handlers (Rust)

Handlers implement auto-generated traits from proto service definitions:

```rust
#[async_trait]
impl GitEffects for GitHandler {
    async fn get_branch(&self, req: GetBranchRequest) -> EffectResult<GetBranchResponse> { ... }
}
```

## Building and Testing

```bash
just install-all-dev          # Build WASM + Rust, install binary
cargo test --workspace        # Rust tests
cabal test all                # Haskell tests
just wasm-all                 # Rebuild WASM only
```

## Dependencies

### Haskell
- **freer-simple** — Effect system with reified continuations
- **proto3-suite** / **proto3-runtime** — Protobuf serialization
- **aeson** — JSON serialization
- **extism-pdk** — WASM host function interface

### Rust
- **extism** — WASM runtime
- **prost** — Protobuf codegen
- **tokio** — Async runtime
- **axum** — HTTP server (UDS)

## References

- [freer-simple](https://hackage.haskell.org/package/freer-simple)
- [GHC WASM](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html)
- [Extism](https://extism.org/)
