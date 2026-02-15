# Docs Staleness Audit - 2026-02-15

This report identifies stale, inaccurate, and missing documentation across `CLAUDE.md` files in the repository.

## Summary

The documentation tree structure in the root `CLAUDE.md` is significantly out of sync with the actual file system. Several directories (e.g., `haskell/runtime/`, `tools/`) no longer exist, and new components (e.g., `haskell/dsl/core/src/ExoMonad/Effects/`) are not fully reflected. The Rust documentation (`rust/exomonad/CLAUDE.md`) incorrectly describes the MCP server transport mechanism (claiming Unix sockets instead of TCP) and omits several implemented tools and handlers.

## 1. Root `CLAUDE.md`

### Missing Directories/Files
The following paths referenced in the "Documentation Tree" or "Package Inventory" sections no longer exist:
*   `haskell/runtime/` (and subdirectories `actor`, `parallel`, `wasm`)
*   `haskell/dsl/teaching/`
*   `haskell/effects/habitica-interpreter/`
*   `haskell/effects/lsp-interpreter/`
*   `haskell/effects/ghci-interpreter/`
*   `haskell/effects/cabal-interpreter/`
*   `haskell/effects/habitica/`
*   `haskell/tools/ghci-oracle/`
*   `haskell/tools/sleeptime/`
*   `tools/` (including `micro-gastown`, `blast-radius`)

### Inaccurate Claims
*   **Server Transport**: States "Uses synchronous Unix sockets... for minimal overhead... during blocking hook execution." While `exomonad reply` uses a control socket, `exomonad serve` (the main MCP server) listens on TCP/HTTP (default port 7432), and hooks communicate via HTTP to `localhost:{port}/hook`.
*   **Documentation Tree**: The tree visualization is stale and missing current paths like `haskell/effects/filesystem-interpreter/` (which likely corresponds to `fs.proto`).

## 2. `rust/CLAUDE.md`

### Incomplete Handler Table
The "Built-in Handlers" table is missing the following implemented handlers (confirmed in `rust/exomonad-core/src/handlers/`):
*   `coordination.*` -> `CoordinationHandler`
*   `kv.*` -> `KvHandler`
*   `session.*` -> `SessionHandler`

### Typos & Missing Tools
*   **Tool Name**: Lists `spawn_worker` (singular) instead of `spawn_workers` (plural). `haskell/wasm-guest/src/ExoMonad/Guest/Tools/Spawn.hs` confirms `toolName = "spawn_workers"`.
*   **Missing Tool**: `spawn_leaf_subtree` is implemented but missing from the tool list.

## 3. `rust/exomonad/CLAUDE.md`

### Inaccurate CLI Documentation
*   **Serve Command**: Claims `exomonad serve [--socket PATH]` uses a Unix socket by default. `src/main.rs` shows `Serve { port: Option<u16> }` which binds a `TcpListener`. The `--socket` argument does not exist for the `serve` command.
*   **Init Command**: Correctly describes creating a session, but the context about "Server tab runs `exomonad serve`" implies Unix socket usage which contradicts the implementation.

### Incomplete Tool & Handler Lists
*   **Missing Tools**: `spawn_leaf_subtree`, `file_pr`, `merge_pr`, `popup`, `note`, `question`, `wait_for_event`, `notify_parent`.
*   **Missing Handlers**: `events.*`, `jj.*`, `merge_pr.*`, `coordination.*`, `kv.*`, `session.*`.
*   **Typo**: Lists `spawn_worker` instead of `spawn_workers`.

## 4. `haskell/CLAUDE.md`

### Missing Structure
*   **Proto Directories**: The "Structure" table omits `proto/` (generated code) and `proto-test/`.
*   **WASM SDK**: References `haskell/wasm-guest` as the SDK, but `haskell/dsl/core` (specifically `ExoMonad.Effect.*`) is also a core part of the SDK for defining effects.

## 5. `proto/CLAUDE.md`

### Missing Proto Files
The file list in "Structure" and "Proto File Reference" is missing:
*   `proto/effects/coordination.proto`
*   `proto/effects/copilot.proto`
*   `proto/effects/egregore.proto`
*   `proto/effects/events.proto`
*   `proto/effects/file_pr.proto`
*   `proto/effects/kv.proto`
*   `proto/effects/merge_pr.proto`
*   `proto/effects/messaging.proto`
*   `proto/effects/popup.proto`
*   `proto/effects/session.proto`

### Outdated Status
*   **Wire Tests**: marks several files as "Pending" (e.g., `common.proto`, `hook.proto`, `agent.proto`, `popup.proto`).
*   **Haskell Implementation**: marks `fs.proto` as "Pending" for Haskell Effect, but `haskell/dsl/core/src/ExoMonad/Effects/FileSystem.hs` exists.

## Recommendations

1.  **Consolidate Documentation**: Remove references to deleted directories in the root `CLAUDE.md`.
2.  **Update CLI Docs**: Correct `rust/exomonad/CLAUDE.md` to reflect that `exomonad serve` is TCP-only and remove the `--socket` argument documentation.
3.  **Sync Handler/Tool Tables**: Update all tables to include all currently implemented handlers (e.g., `coordination`, `kv`) and tools (e.g., `spawn_leaf_subtree`, `notify_parent`).
4.  **Fix Typos**: Correct `spawn_worker` to `spawn_workers`.
5.  **Update Proto Manifest**: Add missing proto files to `proto/CLAUDE.md`.
