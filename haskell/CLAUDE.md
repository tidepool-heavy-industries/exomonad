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
| `dsl/core/` | Effect types, structured output, LLM infrastructure |
| `effects/` | Effect interpreters (LLM, Git, GitHub, Zellij, etc.) |
| `protocol/` | Wire formats for native UI |
| `tools/` | Standalone utilities (training data generation) |
| `vendor/` | Vendored dependencies (ginger, polysemy) |

## Design Patterns

- **Algebraic effects**: Polysemy effects interpreted to IO
- **Adapter pattern**: Interpreters adapt external APIs (HTTP, subprocess, sockets)
- **Embedded DSL**: Haskell WASM as pure logic, hosted by Rust runtime

## Role System

The xmonad pattern for LLM agents: users define agent roles in Haskell, compiled to WASM.

### Directory Structure

```
.exomonad/roles/<role>/
├── Role.hs          # User-authored: tool composition + hook config (TRACKED)
├── Main.hs          # Generated: FFI exports wiring Role.config to WASM entry points (GITIGNORED)
├── <role>.cabal     # Generated: cabal package with WASM linker flags (GITIGNORED)
├── gen/             # Generated: alternate scaffolding for cabal.project.wasm (GITIGNORED)
└── dist/            # Build output: compiled .wasm artifact (GITIGNORED)
```

Shared code across roles lives in `.exomonad/lib/` (e.g., `StopHook.hs`).

### How It Works

1. User writes `Role.hs` — a `RoleConfig` record selecting tools and hooks
2. `cabal.project.wasm` lists `.exomonad/roles/tl` and `.exomonad/roles/dev` as packages
3. `just wasm <role>` (or `exomonad recompile --role <role>`) builds via nix + wasm32-wasi-cabal
4. Build output lands in `dist/`, then gets copied to `.exomonad/wasm/wasm-guest-<role>.wasm`
5. WASM loaded from file by Rust binary at runtime (both hooks and serve mode)
6. In serve mode, hot reload checks mtime per tool call

### Unified WASM

The unified WASM module is the primary build target, containing all roles:

```
.exomonad/roles/unified/
├── AllRoles.hs     # Role registry: Map Text SomeRoleConfig
├── TLRole.hs       # TL role config (re-exported under unique module name)
├── DevRole.hs      # Dev role config (uses httpDevHooks with permission cascade)
├── Main.hs         # FFI exports that read role from input JSON
└── unified.cabal   # Package definition
```

Key types:
- `AllRoles.SomeRoleConfig` — captures dispatch, listing, and hook capabilities for any role
- `AllRoles.lookupRole` — look up role by name from the registry
- `ExoMonad.Permissions` — typed permission ADTs for the three-tier cascade

The unified WASM is additive — individual tl/dev packages still build independently.

### Role Anatomy

Each role is a `RoleConfig` selecting from pre-built tool records:

```haskell
-- .exomonad/roles/tl/Role.hs
data Tools mode = Tools
  { spawn :: SpawnTools mode     -- spawn_subtree, spawn_leaf
  , popups :: PopupTools mode   -- popup UI
  } deriving Generic

config :: RoleConfig (Tools AsHandler)
config = RoleConfig { roleName = "tl", tools = Tools { ... }, hooks = defaultHooks }
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
