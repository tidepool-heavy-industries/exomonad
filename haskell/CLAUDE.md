# Haskell Code Organization

All Haskell packages live here, organized by architectural pattern.

## When to Read Which CLAUDE.md

| I want to... | Read this |
|--------------|-----------|
| Understand graph DSL, handlers, annotations | `dsl/core/CLAUDE.md` |
| Work on LLM-level teaching infrastructure | `dsl/teaching/CLAUDE.md` |
| Add or modify an effect interpreter | `effects/CLAUDE.md` → `effects/{name}-interpreter/CLAUDE.md` |
| Understand graph execution model | `runtime/CLAUDE.md` → `runtime/actor/CLAUDE.md` |
| Work on WASM guest (MCP tools) | `wasm-guest/` (see `../rust/exomonad-sidecar/CLAUDE.md` for runtime) |
| Work with LSP integration | `effects/lsp-interpreter/CLAUDE.md` |
| Generate training data for FunctionGemma | `tools/training-generator/CLAUDE.md` |
| Understand wire protocols | `protocol/CLAUDE.md` |
| Work on dev tools (GHCi oracle, sleeptime) | `tools/CLAUDE.md` → `tools/{name}/CLAUDE.md` |

**Navigation pattern**: Start at `haskell/CLAUDE.md`, drill into topic-specific docs.

## Documentation Tree

```
haskell/CLAUDE.md  ← YOU ARE HERE (router)
├── wasm-guest/  ← WASM plugin (MCP tools, hosted by Rust runtime)
├── dsl/CLAUDE.md
│   ├── core/CLAUDE.md  ← Graph DSL reference (detailed)
│   └── teaching/CLAUDE.md  ← LLM-level teaching for FunctionGemma training
├── effects/CLAUDE.md  ← Effect interpreter pattern
│   ├── llm-interpreter/CLAUDE.md
│   ├── lsp-interpreter/CLAUDE.md  ← Language Server Protocol
│   ├── ghci-interpreter/CLAUDE.md
│   ├── habitica/CLAUDE.md
│   └── ...
├── runtime/CLAUDE.md
│   ├── actor/CLAUDE.md  ← Actor model (detailed)
│   └── wasm/CLAUDE.md
├── protocol/CLAUDE.md
│   ├── wire-types/CLAUDE.md
│   └── generated-ts/CLAUDE.md
└── tools/CLAUDE.md
    ├── ghci-oracle/CLAUDE.md
    ├── sleeptime/CLAUDE.md
    └── training-generator/CLAUDE.md  ← FunctionGemma training data
```

## Structure

| Directory | Purpose | When to read |
|-----------|---------|--------------|
| `wasm-guest/` | WASM plugin with MCP tools (hosted by Rust runtime) | Adding/modifying MCP tools, effect handlers |
| `dsl/` | Graph DSL (core) + LLM teaching infrastructure (teaching) | Defining graphs, handlers, templates; LLM-level training data capture |
| `effects/` | Effect interpreters (HTTP, subprocess, etc.) | Adding/modifying external integrations |
| `runtime/` | Execution backends (actor, WASM) | Understanding concurrent execution |
| `protocol/` | Wire formats (native, WASM) | Client-server communication |
| `tools/` | Standalone utilities | GHCi integration, log analysis, training data |
| `vendor/` | Vendored dependencies | Rarely (freer-simple, ginger internals) |

## Design Patterns

This codebase follows well-known CS patterns:
- **Interpreter pattern**: Effect types (abstract syntax) + interpreters (semantic actions)
- **Algebraic effects**: Free monad over effect types, interpreted to IO
- **Actor model**: Alternative execution backend (runtime/actor)
- **Adapter pattern**: Interpreters adapt external APIs (HTTP, subprocess, sockets)
- **Embedded DSL**: Haskell WASM as pure logic, hosted by Rust runtime

## Common Commands

Build all:     `cabal build all`
Run tests:     `cabal test all`
Pre-commit:    `just pre-commit`

## Adding New Effects

1. Define effect type in `dsl/core/src/ExoMonad/Effect/Types.hs` (or Effects/*.hs)
2. Create interpreter package at `effects/{name}-interpreter/`
3. Add to `cabal.project`
4. Wire into `wasm-guest/` if needed for MCP tools (via host functions)
