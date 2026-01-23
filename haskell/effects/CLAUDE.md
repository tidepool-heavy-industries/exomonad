# Effect Interpreters

Effect types define operations (in `dsl/core`); interpreters provide implementations.

This follows the **interpreter design pattern**: abstract syntax (effect types) separated from semantic actions (interpreters).

## When to Read Which CLAUDE.md

| I want to... | Read this |
|--------------|-----------|
| Understand LLM API calls | `llm-interpreter/CLAUDE.md` |
| Query Haskell types from an agent | `ghci-interpreter/CLAUDE.md` |
| Work with GitHub issues/PRs | `github-interpreter/CLAUDE.md` |
| Build agent UIs (WebSocket bridge) | `ui-interpreter/CLAUDE.md` |
| Add code intelligence (LSP) | `lsp-interpreter/CLAUDE.md` |
| Manage git worktrees | `worktree-interpreter/CLAUDE.md` |
| Run just recipes | `justfile-interpreter/CLAUDE.md` |
| Integrate beads task tracking | `bd-interpreter/CLAUDE.md` |
| Add Grafana observability | `observability-interpreter/CLAUDE.md` |
| Run cabal builds/tests | `cabal-interpreter/CLAUDE.md` |
| Add session logging | `devlog-interpreter/CLAUDE.md` |
| Add gamification (Habitica) | `habitica-interpreter/CLAUDE.md` |
| Understand the interpreter pattern | This file (you're here) |

## Documentation Tree

```
effects/CLAUDE.md  ← YOU ARE HERE (router)
├── llm-interpreter/CLAUDE.md           ← Anthropic/OpenAI API
├── lsp-interpreter/CLAUDE.md           ← Language server protocol
├── ghci-interpreter/CLAUDE.md          ← GHCi oracle client
├── github-interpreter/CLAUDE.md        ← gh CLI for issues/PRs
├── ui-interpreter/CLAUDE.md            ← WebSocket UI bridge
├── bd-interpreter/CLAUDE.md            ← Beads task tracking
├── observability-interpreter/CLAUDE.md ← Grafana Loki & Tempo
├── worktree-interpreter/CLAUDE.md      ← Git worktree management
├── justfile-interpreter/CLAUDE.md      ← Justfile recipe execution
├── cabal-interpreter/CLAUDE.md         ← Build & test integration
├── devlog-interpreter/CLAUDE.md        ← Session-scoped logging
├── habitica-interpreter/CLAUDE.md      ← Gamification API
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

Most effect types live in `dsl/core/src/Tidepool/Effect/Types.hs` or `Effects/*.hs`.

## Interpreter Listing

| Effect | Types | Interpreter | Implementation |
|--------|-------|-------------|----------------|
| LLM | dsl/core | llm-interpreter | HTTP (Anthropic/OpenAI) |
| LSP | dsl/core | lsp-interpreter | lsp-client library |
| BD | dsl/core | bd-interpreter | Subprocess (urchin CLI) |
| Habitica | effects/habitica | habitica-interpreter | HTTP API |
| UI | dsl/core | ui-interpreter | WebSocket bridge |
| Observability | dsl/core | observability-interpreter | OTLP/Loki push |
| GHCi | dsl/core | ghci-interpreter | ghci-oracle client |
| GitHub | dsl/core | github-interpreter | gh CLI subprocess |
| Worktree | dsl/core | worktree-interpreter | Git subprocess |
| Cabal | dsl/core | cabal-interpreter | Cabal CLI subprocess |
| Justfile | dsl/core | justfile-interpreter | just CLI subprocess |
| DevLog | dsl/core | devlog-interpreter | File IO |
| Env | dsl/core | env-interpreter | Environment variables |
| Filesystem | dsl/core | filesystem-interpreter | File system operations |
| Gemini | dsl/core | gemini-interpreter | Gemini API |
| Zellij | dsl/core | zellij-interpreter | Zellij multiplexer |

## Adding a New Effect Interpreter

1. Define effect type in `dsl/core/src/Tidepool/Effect/Types.hs` (or Effects/*.hs)
2. Create interpreter package:
   ```bash
   mkdir -p effects/{name}-interpreter
   cd effects/{name}-interpreter
   cabal init -n --lib tidepool-{name}-interpreter
   ```
3. Implement interpreter (see existing for patterns)
4. Add to `cabal.project`: `haskell/effects/{name}-interpreter`
5. Wire into `native-server/` effect composition

## Implementation Patterns

- **HTTP clients**: llm, habitica, github, observability
- **Subprocesses**: bd, worktree, cabal
- **Socket clients**: ghci, lsp
- **WebSocket bridge**: ui
- **File IO**: devlog

## Claude Code Integration

For Claude Code integration, see `rust/CLAUDE.md`. The Rust workspace provides:
- **mantle-agent hook** - Forwards hooks to Haskell via HTTP over Unix socket

MCP tools are served directly by control-server via HTTP MCP transport. This replaces the removed `session-interpreter` which was used for headless orchestration.
