# Haskell Code Organization

All Haskell packages live here.

## Structure

| Directory | Purpose |
|-----------|---------|
| `wasm-guest/` | WASM plugin with MCP tools (hosted by Rust runtime, freer-simple) |
| `proto/` | Generated Haskell proto types (from `proto/` root) |
| `vendor/freer-simple/` | Vendored freer-simple (GHC 9.12 patches) |
| `vendor/exomonad-pdk/` | Vendored Extism PDK |
| `vendor/proto3-runtime/` | Vendored protobuf runtime |
| `vendor/ginger/` | Vendored typed Jinja templates |

## Design Patterns

- **Algebraic effects**: freer-simple `Eff` with coroutine-based yield/resume
- **Typed config DSL**: Haskell WASM defines tool schemas, dispatch, hooks, event routing — everything that would otherwise be scattered config files, but with a type system and effect system
- **Effect boundary**: WASM yields typed effects, Rust executes the I/O. Agents themselves have full tool access (bash, files, git)

## Role System

The xmonad pattern for LLM agents: users define agent roles in Haskell, compiled to WASM.

### Directory Structure

```
.exo/roles/devswarm/
├── AllRoles.hs     # Role registry: Map Text SomeRoleConfig
├── RootRole.hs     # Root TL role (human-facing: spawn, merge, no file_pr/notify_parent)
├── TLRole.hs       # Spawned TL role (spawn, PR, merge, notify_parent, send_message)
├── DevRole.hs      # Dev role config (PR, notify_parent, task tools + permission cascade)
├── WorkerRole.hs   # Worker role config (notify_parent, task tools, allow-all hooks)
├── TLStopCheck.hs  # Shared stop-hook logic for Root and TL roles
├── Main.hs         # FFI exports that read role from input JSON
└── devswarm.cabal  # Package definition
```

Shared code across roles lives in `.exo/lib/` (e.g., `HttpDevHooks.hs`).

### How It Works

1. Role configs live in `.exo/roles/devswarm/` — one module per role (`RootRole.hs`, `TLRole.hs`, `DevRole.hs`, `WorkerRole.hs`)
2. `AllRoles.hs` registers all roles in a `Map Text SomeRoleConfig`
3. `cabal.project.wasm` lists `.exo/roles/devswarm` as a package
4. `just wasm-all` (or `exomonad recompile --role devswarm`) builds via nix + wasm32-wasi-cabal
5. Output: `.exo/wasm/wasm-guest-devswarm.wasm` — loaded by Rust at runtime
6. In serve mode, hot reload checks mtime per tool call

Key types:
- `AllRoles.SomeRoleConfig` — captures dispatch, listing, and hook capabilities for any role
- `AllRoles.lookupRole` — look up role by name from the registry
- `ExoMonad.Permissions` — typed permission ADTs for the three-tier cascade

### Role Anatomy

Each role is a `RoleConfig` selecting from pre-built tool records:

```haskell
-- .exo/roles/devswarm/TLRole.hs
data Tools mode = Tools
  { spawn :: SpawnTools mode,
    pr :: FilePRTools mode,
    mergePr :: mode :- MergePR,
    notifyParent :: mode :- NotifyParent,
    sendMessage :: mode :- SendMessage
  } deriving Generic

config :: RoleConfig (Tools AsHandler)
config = RoleConfig { roleName = "tl", tools = Tools { ... }, hooks = ... }
```

The `mode` parameter enables the same record for both schema generation (`AsSchema`) and handler dispatch (`AsHandler`). See ADR-004 for the full design rationale.

- **Library/SDK**: `haskell/wasm-guest` provides the SDK for defining MCP tools and hooks.

## Common Commands

```bash
cabal build all           # Build everything
cabal test all            # Run tests
just pre-commit           # Run all checks
just proto-gen-haskell    # Regenerate Haskell types from proto files
```

## Adding New MCP Tools

1. Create tool module in `wasm-guest/src/ExoMonad/Guest/Tools/`
2. Define tool using `MCPTool` typeclass (schema + handler)
3. Wire into role configs in `.exo/roles/devswarm/`
4. If new I/O is needed, add effect GADT in `wasm-guest/src/ExoMonad/Guest/Effects/` and corresponding Rust handler
