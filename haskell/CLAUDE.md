# Haskell Code Organization

All Haskell packages live here, organized by architectural pattern.

## Structure

- `dsl/` - **Domain-Specific Language** for agent graphs → see dsl/CLAUDE.md
- `effects/` - **Effect system** with interpreter pattern → see effects/CLAUDE.md
- `runtime/` - **Execution backends** (actor model, parallel, WASM)
- `native-server/` - **Facade pattern** - WebSocket server orchestrating interpreters
- `protocol/` - **Wire protocol** - Serialization formats (native, WASM)
- `tools/` - Standalone utilities (ghci-oracle, sleeptime)
- `vendor/` - Vendored dependencies (freer-simple, ginger)

## Design Patterns

This codebase follows well-known CS patterns:
- **Interpreter pattern**: Effect types (abstract syntax) + interpreters (semantic actions)
- **Algebraic effects**: Free monad over effect types, interpreted to IO
- **Actor model**: Alternative execution backend (runtime/actor)
- **Adapter pattern**: Interpreters adapt external APIs (HTTP, subprocess, sockets)
- **Facade pattern**: Server provides unified interface over all interpreters

## Common Commands

Build all:     `cabal build all`
Run server:    `just native`
Run tests:     `cabal test all`
Pre-commit:    `just pre-commit`

## Adding New Effects

1. Define effect type in `dsl/core/src/Tidepool/Effect/Types.hs` (or Effects/*.hs)
2. Create interpreter package at `effects/{name}-executor/`
3. Add to `cabal.project`
4. Wire into `native-server/` (see native-server/CLAUDE.md)
