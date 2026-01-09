# Effect Interpreters

Effect types define operations (in `dsl/core`); interpreters provide implementations.

This follows the **interpreter design pattern**: abstract syntax (effect types) separated from semantic actions (interpreters).

## Structure

```
effects/
  {name}-executor/          # Interpreter package (HTTP client, subprocess, etc.)
  habitica/                 # Exception: standalone effect types package
```

Most effect types live in `dsl/core/src/Tidepool/Effect/Types.hs` or `Effects/*.hs`.

## Interpreter Listing

| Effect | Types | Interpreter | Implementation |
|--------|-------|-------------|----------------|
| LLM | dsl/core | llm-executor | HTTP (Anthropic/OpenAI) |
| BD | dsl/core | bd-executor | Subprocess (urchin CLI) |
| ClaudeCode | dsl/core | claude-code-executor | Subprocess (mantle) |
| Habitica | effects/habitica | habitica-executor | HTTP API |
| UI | dsl/core | ui-executor | WebSocket bridge |
| Observability | dsl/core | observability-executor | OTLP/Loki push |
| LSP | dsl/core | lsp-executor | lsp-client library |
| GHCi | dsl/core | ghci-executor | ghci-oracle client |
| GitHub | dsl/core | github-executor | gh CLI subprocess |
| Worktree | dsl/core | worktree-executor | Git subprocess |
| Cabal | dsl/core | cabal-executor | Cabal CLI subprocess |
| DevLog | dsl/core | devlog-executor | File IO |

## Adding a New Effect Interpreter

1. Define effect type in `dsl/core/src/Tidepool/Effect/Types.hs` (or Effects/*.hs)
2. Create interpreter package:
   ```bash
   mkdir -p effects/{name}-executor
   cd effects/{name}-executor
   cabal init -n --lib tidepool-{name}-executor
   ```
3. Implement interpreter (see existing for patterns)
4. Add to `cabal.project`: `haskell/effects/{name}-executor`
5. Wire into `native-server/` effect composition

## Implementation Patterns

- **HTTP clients**: llm, habitica, github, observability
- **Subprocesses**: bd, claude-code, worktree, cabal
- **Socket clients**: ghci, lsp
- **WebSocket bridge**: ui
- **File IO**: devlog
