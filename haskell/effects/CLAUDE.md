# Effect Interpreters

Effect types define operations (in `dsl/core`); interpreters provide implementations.

This follows the **interpreter pattern**: abstract syntax (effect types) separated from semantic actions (interpreters).

## Interpreter Listing

| Effect | Interpreter | Implementation |
|--------|-------------|----------------|
| LLM | llm-interpreter | Socket client |
| Git | git-interpreter | Git subprocess |
| GitHub | github-interpreter | Socket client (Rust Octocrab) |
| Worktree | worktree-interpreter | Git worktree subprocess |
| Justfile | justfile-interpreter | just CLI subprocess |
| Observability | observability-interpreter | OTLP/Loki push |
| Env | env-interpreter | Environment variables |
| Filesystem | filesystem-interpreter | File operations |
| Zellij | zellij-interpreter | Zellij multiplexer |

## Structure

```
effects/
├── llm-interpreter/         # LLM API calls (Anthropic)
├── git-interpreter/         # Git CLI operations
├── github-interpreter/      # GitHub API via socket
├── worktree-interpreter/    # Git worktree management
├── justfile-interpreter/    # Just recipe execution
├── observability-interpreter/ # Grafana/OTLP
├── env-interpreter/         # Environment variables
├── filesystem-interpreter/  # File operations
├── zellij-interpreter/      # Terminal multiplexer
└── socket-client/           # Shared socket protocol
```

## Adding a New Interpreter

1. Define effect type in `dsl/core/src/ExoMonad/Effect/Types.hs` (or `Effects/*.hs`)
2. Create interpreter package:
   ```bash
   mkdir -p effects/{name}-interpreter
   cd effects/{name}-interpreter
   cabal init -n --lib exomonad-{name}-interpreter
   ```
3. Implement interpreter
4. Add to `cabal.project`
5. Wire into `wasm-guest/` if needed for MCP tools (via host functions)

## Implementation Patterns

- **Socket clients**: llm, github (connect to Rust services)
- **Subprocesses**: git, worktree, justfile
- **Direct I/O**: env, filesystem
