# tidepool-native-gui/ - Native Effect Interpreters

Native Haskell runtime with WebSocket GUI. Production deployment option alongside Cloudflare Workers. Uses Anthropic API directly with full observability via Grafana.

## What This Is

A collection of effect interpreters and a WebSocket server that:
1. Interprets tidepool-core effects natively (no WASM)
2. Provides a SolidJS frontend for local development
3. Enables full observability via Grafana Cloud

The key insight: **Effect types live in tidepool-core, interpreters are platform-specific**. This package provides native interpreters; `deploy/` provides Cloudflare Worker interpreters.

## Structure

```
tidepool-native-gui/
├── server/                   # WebSocket server - ties it together
├── wire-types/               # UIState, UserAction, protocol types
├── solid-frontend/           # SolidJS frontend
├── ui-interpreter/              # UI effect interpreter
├── llm-interpreter/             # Anthropic/OpenAI native HTTP client
├── habitica-interpreter/        # Native Habitica HTTP client
├── observability-interpreter/   # OpenTelemetry/Loki push
├── bd-interpreter/              # Beads integration + urchin CLI
├── lsp-interpreter/             # LSP via lsp-client
├── claude-code-interpreter/     # Claude Code subprocess (WIP)
├── worktree-interpreter/        # Git worktree management
├── ghci-interpreter/            # GHCi Oracle thin client
├── github-interpreter/          # GitHub API integration
├── issue-interpreter/           # Issue tracking
└── devlog-interpreter/          # Devlog effect interpreter
```

## Interpreters

| Package | Effect | Execution |
|---------|--------|-----------|
| `llm-interpreter` | `LLM`, `LLMComplete` | Anthropic/OpenAI HTTP API |
| `ui-interpreter` | `ShowText`, `RequestChoice`, etc. | WebSocket → frontend |
| `habitica-interpreter` | `Habitica` | Habitica HTTP API |
| `observability-interpreter` | `PublishEvent`, `WithSpan` | Grafana Cloud Loki |
| `bd-interpreter` | `BD` | Beads database operations |
| `lsp-interpreter` | `LSP` | lsp-client subprocess |
| `claude-code-interpreter` | `ClaudeCodeExec` | claude CLI subprocess |
| `worktree-interpreter` | `Worktree` | Git worktree via subprocess |
| `ghci-interpreter` | `GHCi` | Socket client to ghci-oracle server |
| `github-interpreter` | `GitHub` | GitHub REST API |
| `issue-interpreter` | `FileIssue` | Issue tracking |

## Key Principle

Effect *types* live in `tidepool-core/src/Tidepool/Effects/`, shared by WASM and native. Effect *interpreters* are platform-specific and live here.

## Dependencies

```
wire-types ──────► ui-interpreter
             ├───► server
             └───► solid-frontend

habitica-interpreter ───► server
observability-interpreter ───► server
llm-interpreter ───► server
bd-interpreter ───► server
lsp-interpreter ───► server
```

## Building

```bash
# Haskell packages (from repo root)
cabal build all

# Solid frontend
cd tidepool-native-gui/solid-frontend
npm install
npm run build     # Production build
npm run dev       # Dev server on port 3000
```

Set `VITE_WS_URL` to override the WebSocket URL (default: `ws://localhost:8080`).

## Running

```bash
# From repo root
just native  # Starts server at localhost:8080
```

## Related Documentation

- [server/CLAUDE.md](server/CLAUDE.md) - Native server details
- [tidepool-core/CLAUDE.md](../tidepool-core/CLAUDE.md) - Effect type definitions
- [deploy/CLAUDE.md](../deploy/CLAUDE.md) - Cloudflare Worker executors (alternative runtime)
- [Root CLAUDE.md](../CLAUDE.md) - Project overview
