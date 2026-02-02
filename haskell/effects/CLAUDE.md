# Effect Interpreters

Effect types define operations (in `dsl/core`); interpreters provide implementations.

This follows the **interpreter design pattern**: abstract syntax (effect types) separated from semantic actions (interpreters).

## When to Read Which CLAUDE.md

| I want to... | Read this |
|--------------|-----------|
| Understand LLM API calls | `llm-interpreter/CLAUDE.md` |
| Work with GitHub issues/PRs (via Rust/Octocrab) | `github-interpreter/CLAUDE.md` |
| Manage git worktrees | `worktree-interpreter/CLAUDE.md` |
| Run just recipes | `justfile-interpreter/CLAUDE.md` |
| Add Grafana observability | `observability-interpreter/CLAUDE.md` |
| Run cabal builds/tests | `cabal-interpreter/CLAUDE.md` |
| Add gamification (Habitica) | `habitica-interpreter/CLAUDE.md` |
| Create Zellij tabs (parallel agents) | `zellij-interpreter/CLAUDE.md` |
| Understand the interpreter pattern | This file (you're here) |

## Documentation Tree

```
effects/CLAUDE.md  ← YOU ARE HERE (router)
├── llm-interpreter/CLAUDE.md           ← Anthropic API
├── github-interpreter/CLAUDE.md        ← Socket client (Rust Octocrab)
├── observability-interpreter/CLAUDE.md ← Grafana Loki & Tempo
├── worktree-interpreter/CLAUDE.md      ← Git worktree management
├── justfile-interpreter/CLAUDE.md      ← Justfile recipe execution
├── cabal-interpreter/CLAUDE.md         ← Build & test integration
├── habitica-interpreter/CLAUDE.md      .. Gamification API
├── env-interpreter/CLAUDE.md           ← Environment variable access
├── filesystem-interpreter/CLAUDE.md    ← File system operations
├── gemini-interpreter/CLAUDE.md        ← Gemini API integration
├── zellij-interpreter/CLAUDE.md        ← Zellij terminal multiplexer
└── habitica/CLAUDE.md                  ← Habitica effect types
```

## Structure

```
effects/
  {name}-interpreter/          # Interpreter package (HTTP client, subprocess, etc.)
  habitica/                    # Exception: standalone effect types package
```

Most effect types live in `dsl/core/src/ExoMonad/Effect/Types.hs` or `Effects/*.hs`.

## Interpreter Listing

| Effect | Types | Interpreter | Implementation |
|--------|-------|-------------|----------------|
| LLM | dsl/core | llm-interpreter | Socket (preferred) |
| Habitica | effects/habitica | habitica-interpreter | HTTP API |
| Observability | dsl/core | observability-interpreter | OTLP/Loki push |
| GitHub | dsl/core | github-interpreter | Socket client (Rust Octocrab) |
| Worktree | dsl/core | worktree-interpreter | Git subprocess |
| Cabal | dsl/core | cabal-interpreter | Cabal CLI subprocess |
| Justfile | dsl/core | justfile-interpreter | just CLI subprocess |
| Env | dsl/core | env-interpreter | Environment variables |
| Filesystem | dsl/core | filesystem-interpreter | File system operations |
| Gemini | dsl/core | gemini-interpreter | Gemini API |
| Zellij | dsl/core | zellij-interpreter | Zellij multiplexer |

## Adding a New Effect Interpreter

1. Define effect type in `dsl/core/src/ExoMonad/Effect/Types.hs` (or Effects/*.hs)
2. Create interpreter package:
   ```bash
   mkdir -p effects/{name}-interpreter
   cd effects/{name}-interpreter
   cabal init -n --lib exomonad-{name}-interpreter
   ```
3. Implement interpreter (see existing for patterns)
4. Add to `cabal.project`: `haskell/effects/{name}-interpreter`
5. Wire into `wasm-guest/` if needed for MCP tools (via host functions in Rust runtime)

## Implementation Patterns

- **HTTP clients**: habitica, github, observability
- **Subprocesses**: worktree, cabal, docker-ctl
- **Socket clients**: lsp, llm

## Claude Code Integration

For Claude Code integration, see `rust/CLAUDE.md`. The Rust workspace provides:
- **exomonad-sidecar** - MCP server + hook handler that hosts Haskell WASM plugin
- **exomonad-runtime** - WASM plugin loading + host functions for effect execution

All MCP tools and hook logic live in the Haskell WASM plugin (`wasm-guest/`). Rust executes effects via host functions (git, GitHub API, filesystem, Zellij).