# Haskell Code Organization

All Haskell packages live here.

## When to Read Which CLAUDE.md

| I want to... | Read this |
|--------------|-----------|
| Understand effect types, structured output | `dsl/core/CLAUDE.md` |
| Add or modify an effect interpreter | `effects/CLAUDE.md` |
| Work on WASM guest (MCP tools) | `wasm-guest/` |
| Understand wire protocols | `protocol/CLAUDE.md` |
| Generate training data | `tools/training-generator/CLAUDE.md` |

## Structure

| Directory | Purpose |
|-----------|---------|
| `wasm-guest/` | WASM plugin with MCP tools (hosted by Rust runtime) |
| `dsl/core/` | Effect types, structured output, LLM infrastructure (SDK) |
| `effects/` | Effect interpreters (LLM, Git, GitHub, Zellij, etc.) |
| `proto/` | Generated Haskell proto types (from `proto/` root) |
| `proto-test/` | Tests for generated proto types |
| `protocol/` | Wire formats for native UI |
| `tools/` | Standalone utilities (training data generation) |
| `vendor/` | Vendored dependencies (ginger, polysemy, freer-simple) |

## Design Patterns

- **Algebraic effects**: Polysemy (DSL/effects) and freer-simple (WASM guest) interpreted to IO
- **Adapter pattern**: Interpreters adapt external APIs (HTTP, subprocess, sockets)
- **Embedded DSL**: Haskell WASM as pure logic, hosted by Rust runtime

## Role System

The xmonad pattern for LLM agents: users define agent roles in Haskell, compiled to WASM.

### Directory Structure

```
.exo/roles/unified/
├── AllRoles.hs     # Role registry: Map Text SomeRoleConfig
├── TLRole.hs       # TL role config (spawn, PR, merge, popup, notify_parent)
├── DevRole.hs      # Dev role config (PR, notify_parent + permission cascade)
├── WorkerRole.hs   # Worker role config (notify_parent only, allow-all hooks)
├── Main.hs         # FFI exports that read role from input JSON
└── unified.cabal   # Package definition
```

Shared code across roles lives in `.exo/lib/` (e.g., `StopHook.hs`, `HttpDevHooks.hs`).

### How It Works

1. Role configs live in `.exo/roles/unified/` — one module per role (`TLRole.hs`, `DevRole.hs`, `WorkerRole.hs`)
2. `AllRoles.hs` registers all roles in a `Map Text SomeRoleConfig`
3. `cabal.project.wasm` lists `.exo/roles/unified` as a package
4. `just wasm-all` (or `exomonad recompile --role unified`) builds via nix + wasm32-wasi-cabal
5. Output: `.exo/wasm/wasm-guest-unified.wasm` — loaded by Rust at runtime
6. In serve mode, hot reload checks mtime per tool call

Key types:
- `AllRoles.SomeRoleConfig` — captures dispatch, listing, and hook capabilities for any role
- `AllRoles.lookupRole` — look up role by name from the registry
- `ExoMonad.Permissions` — typed permission ADTs for the three-tier cascade

### Role Anatomy

Each role is a `RoleConfig` selecting from pre-built tool records:

```haskell
-- .exo/roles/unified/TLRole.hs
data Tools mode = Tools
  { spawn :: SpawnTools mode,
    popups :: PopupTools mode,
    pr :: FilePRTools mode,
    mergePr :: mode :- MergePR,
    notifyParent :: mode :- NotifyParent
  } deriving Generic

config :: RoleConfig (Tools AsHandler)
config = RoleConfig { roleName = "tl", tools = Tools { ... }, hooks = ... }
```

The `mode` parameter enables the same record for both schema generation (`AsSchema`) and handler dispatch (`AsHandler`). See ADR-004 for the full design rationale.

- **Library**: `haskell/wasm-guest` provides the SDK (`ExoMonad.Guest.Tool.Runtime`)

## Common Commands

```bash
cabal build all      # Build everything
cabal test all       # Run tests
just pre-commit      # Run all checks
```

## Adding New Effects

1. Define effect type in `dsl/core/src/ExoMonad/Effect/Types.hs` (or `Effects/*.hs`)
2. Create interpreter package at `effects/{name}-interpreter/`
3. Add to `cabal.project`
4. Wire into `wasm-guest/` if needed for MCP tools (via host functions)
