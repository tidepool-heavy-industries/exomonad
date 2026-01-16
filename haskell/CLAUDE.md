# Haskell Code Organization

All Haskell packages live here, organized by architectural pattern.

## When to Read Which CLAUDE.md

| I want to... | Read this |
|--------------|-----------|
| Understand graph DSL, handlers, annotations | `dsl/core/CLAUDE.md` |
| Add or modify an effect interpreter | `effects/CLAUDE.md` → `effects/{name}-interpreter/CLAUDE.md` |
| Understand graph execution model | `runtime/CLAUDE.md` → `runtime/actor/CLAUDE.md` |
| Work on Claude Code++ (hooks/MCP/scout) | `control-server/CLAUDE.md` ⭐ |
| Work on semantic-scout code exploration | `control-server/CLAUDE.md` (merged from agents/semantic-scout) |
| Expose agents as MCP tools | `effects/mcp-server/CLAUDE.md` |
| Work with LSP integration | `effects/lsp-interpreter/CLAUDE.md` |
| Generate training data for FunctionGemma | `tools/training-generator/CLAUDE.md` |
| Work on the WebSocket server | `native-server/CLAUDE.md` |
| Understand wire protocols | `protocol/CLAUDE.md` |
| Work on dev tools (GHCi oracle, sleeptime) | `tools/CLAUDE.md` → `tools/{name}/CLAUDE.md` |

**Navigation pattern**: Start at `haskell/CLAUDE.md`, drill into topic-specific docs.

## Documentation Tree

```
haskell/CLAUDE.md  ← YOU ARE HERE (router)
├── control-server/CLAUDE.md  ⭐ Claude Code++ hub (hooks/MCP/scout/LSP)
├── dsl/CLAUDE.md
│   └── core/CLAUDE.md  ← Graph DSL reference (detailed)
├── agents/
│   └── semantic-scout/CLAUDE.md  ← MERGED into control-server (redirect notice)
├── effects/CLAUDE.md  ← Effect interpreter pattern
│   ├── llm-interpreter/CLAUDE.md
│   ├── lsp-interpreter/CLAUDE.md  ← Language Server Protocol (used by scout)
│   ├── mcp-server/CLAUDE.md  ← MCP tool server (vendored library)
│   ├── ghci-interpreter/CLAUDE.md
│   ├── habitica/CLAUDE.md
│   └── ...
├── runtime/CLAUDE.md
│   ├── actor/CLAUDE.md  ← Actor model (detailed)
│   └── wasm/CLAUDE.md
├── protocol/CLAUDE.md
│   ├── wire-types/CLAUDE.md
│   └── generated-ts/CLAUDE.md
├── native-server/CLAUDE.md  ← WebSocket server facade
├── platform/CLAUDE.md
└── tools/CLAUDE.md
    ├── ghci-oracle/CLAUDE.md
    ├── sleeptime/CLAUDE.md
    └── training-generator/CLAUDE.md  ← FunctionGemma training data (used by scout)
```

## Structure

| Directory | Purpose | When to read |
|-----------|---------|--------------|
| `control-server/` ⭐ | Claude Code++ hub: hook/MCP handler + semantic-scout + LSP session manager | Working on Claude Code++ integration, scout, or hook logic |
| `dsl/` | Graph DSL, effects, templates, schemas | Defining graphs, handlers, templates |
| `agents/` | Production agents (semantic-scout merged into control-server) | See control-server for active scout implementation |
| `effects/` | Effect interpreters (HTTP, subprocess, etc.) | Adding/modifying external integrations |
| `runtime/` | Execution backends (actor, WASM) | Understanding concurrent execution |
| `native-server/` | WebSocket server facade | Server lifecycle, effect composition |
| `protocol/` | Wire formats (native, WASM) | Client-server communication |
| `tools/` | Standalone utilities | GHCi integration, log analysis, training data |
| `vendor/` | Vendored dependencies | Rarely (freer-simple, ginger internals) |

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
2. Create interpreter package at `effects/{name}-interpreter/`
3. Add to `cabal.project`
4. Wire into `native-server/` (see native-server/CLAUDE.md)
