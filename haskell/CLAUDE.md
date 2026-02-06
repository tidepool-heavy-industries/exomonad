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

Users define agent roles in Haskell, compiled to WASM:

- **Role Definitions**: User roles in `.exomonad/roles/`
- **Tool Records**: Compose tools via `MCPTool` typeclass
- **WASM Compilation**: Roles compiled to WASM, loaded by Rust sidecar
- **Library**: `haskell/wasm-guest` provides the SDK

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
