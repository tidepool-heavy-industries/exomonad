# Haskell Code Organization

All Haskell packages live here.

## When to Read Which CLAUDE.md

| I want to... | Read this |
|--------------|-----------|
| Understand effect types, structured output | `dsl/core/CLAUDE.md` |
| Add or modify an effect interpreter | `effects/CLAUDE.md` |
| Understand wire protocols | `protocol/CLAUDE.md` |
| Generate training data | `tools/training-generator/CLAUDE.md` |

## Structure

| Directory | Purpose |
|-----------|---------|
| `dsl/core/` | Effect types, structured output, LLM infrastructure (SDK) |
| `effects/` | Effect interpreters (LLM, Git, GitHub, Zellij, etc.) |
| `protocol/` | Wire formats for native UI |
| `tools/` | Standalone utilities (training data generation) |
| `vendor/` | Vendored dependencies (ginger, polysemy, freer-simple) |

## Design Patterns

- **Algebraic effects**: Polysemy (DSL/effects) interpreted to IO
- **Adapter pattern**: Interpreters adapt external APIs (HTTP, subprocess, sockets)

## MCP Tool Definitions

MCP tools are defined in Haskell and compiled to native code via Tidepool (Cranelift JIT). Tool definitions live in `rust/exomonad-core/haskell/`.

### Role System

Each agent role selects a subset of tools:
- **TL role**: spawn, PR, merge, popup, notify_parent
- **Dev role**: PR, notify_parent
- **Worker role**: notify_parent only

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
