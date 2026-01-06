# tidepool-native-gui/ - Native Effect Executors

Native Haskell runtime with WebSocket GUI. Production deployment option alongside Cloudflare Workers. Uses Anthropic API directly with full observability via Grafana.

## What This Is

A collection of effect executors and a WebSocket server that:
1. Interprets tidepool-core effects natively (no WASM)
2. Provides a SolidJS frontend for local development
3. Enables full observability via Grafana Cloud

The key insight: **Effect types live in tidepool-core, executors are platform-specific**. This package provides native executors; `deploy/` provides Cloudflare Worker executors.

## Structure

```
tidepool-native-gui/
├── server/                   # WebSocket server - ties it together
├── wire-types/               # UIState, UserAction, protocol types
├── solid-frontend/           # SolidJS frontend
├── ui-executor/              # UI effect interpreter
├── llm-executor/             # Anthropic/OpenAI native HTTP client
├── habitica-executor/        # Native Habitica HTTP client
├── observability-executor/   # OpenTelemetry/Loki push
├── bd-executor/              # Beads integration + urchin CLI
├── lsp-executor/             # LSP via lsp-client
├── claude-code-executor/     # Claude Code subprocess (WIP)
├── github-executor/          # GitHub API integration
├── issue-executor/           # Issue tracking
└── devlog-executor/          # Devlog effect executor
```

## Executors

| Package | Effect | Execution |
|---------|--------|-----------|
| `llm-executor` | `LLM`, `LLMComplete` | Anthropic/OpenAI HTTP API |
| `ui-executor` | `ShowText`, `RequestChoice`, etc. | WebSocket → frontend |
| `habitica-executor` | `Habitica` | Habitica HTTP API |
| `observability-executor` | `PublishEvent`, `WithSpan` | Grafana Cloud Loki |
| `bd-executor` | `BD` | Beads database operations |
| `lsp-executor` | `LSP` | lsp-client subprocess |
| `claude-code-executor` | `ClaudeCodeExec` | claude CLI subprocess |
| `github-executor` | `GitHub` | GitHub REST API |
| `issue-executor` | `FileIssue` | Issue tracking |

## Key Principle

Effect *types* live in `tidepool-core/src/Tidepool/Effects/`, shared by WASM and native. Effect *executors* are platform-specific and live here.

## Dependencies

```
wire-types ──────► ui-executor
             ├───► server
             └───► solid-frontend

habitica-executor ───► server
observability-executor ───► server
llm-executor ───► server
bd-executor ───► server
lsp-executor ───► server
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
