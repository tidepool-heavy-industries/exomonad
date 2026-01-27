# Gemini Code Intelligence

This project uses `CLAUDE.md` files for documentation and context.

## Setup

Gemini Code Assist uses the same MCP architecture as Claude Code:

1. **Start the control server:**
   ```bash
   ./start-augmented.sh
   ```

2. **Configure MCP in settings.json:**
   The `settings.json` file uses HTTP MCP transport to connect directly to control-server via Unix socket.

**Note:** Gemini uses the same tools as Claude Code (find_callers, teach-graph, spawn_agents, etc.) via the control-server HTTP MCP endpoints.

## Documentation

Please refer to the following files for information:
- `CLAUDE.md` - Root project overview and architecture.
- `haskell/CLAUDE.md` - Haskell package organization and details.
- `rust/CLAUDE.md` - Rust workspace overview.
- Other `CLAUDE.md` files in subdirectories as linked from the root.

Gemini-specific configuration is located in `settings.json` at the project root.

## ExoMonad Orchestration

The project uses a multi-process architecture orchestrated via Unix Domain Sockets (UDS).

### Core Components
- **`start-augmented.sh`**: The main entry point. Orchestrates the environment, sets canonical socket paths, and launches Zellij.
- **`.zellij/exomonad.kdl`**: Defines the TUI layout. Each pane uses `scripts/wait-for-socket.sh` to synchronize with backend readiness.

### Socket Paths (Single Source of Truth)
- `EXOMONAD_CONTROL_SOCKET`: Default `.exomonad/sockets/control.sock`
- `EXOMONAD_TUI_SOCKET`: Default `.exomonad/sockets/tui.sock`

### Key Patterns
- **Supervision**: Zellij panes run bash loops that restart clients (Claude, TUI Sidebar) on crash.
- **Readiness**: Clients do not connect blindly; they wait for the UDS file to appear via `wait-for-socket.sh`.
- **TUI Detection**: `tui-sidebar` (Rust) checks for the existence of the control socket to determine if the TUI was explicitly disabled (e.g., via `--no-tui`) and exits gracefully if so.

### Subagent Spawning

The `spawn_agents` tool automates the creation of isolated worktrees for parallel task execution.

### Subagent Hardening
- **Environment Isolation**: `SpawnAgents.hs` explicitly sets `EXOMONAD_CONTROL_SOCKET` and `EXOMONAD_TUI_SOCKET` to relative paths and passes them directly to the Docker container via the `docker-ctl` API. This prevents accidental connection to the root control server (isolation breach) even if the parent environment uses absolute paths.

## Operational Considerations

### Resource Limits
Spawning multiple subagents creates a linear increase in resource consumption.
- **Processes**: Each agent spawns a `exomonad-control-server` (~50MB) and potentially an LSP server (varies, ~500MB+ for HLS/RA).
- **File Descriptors**: Each agent requires open sockets and file handles. Check `ulimit -n` if spawning >10 agents.
- **Disk Space**: Each worktree mimics the repo structure. While git worktrees are efficient, the `.exomonad` artifacts and build outputs are unique per agent.

## Hardening & Edge Cases

The `spawn_agents` tool includes several safety checks to prevent common failures:
1.  **Binary Check**: Verifies `exomonad-control-server` exists in the runtime path before creating a worktree.
3.  **Path Traversal**: Rejects bead IDs containing `/` or `..` to prevent arbitrary file system writes.
4.  **Env Var Validation**: Ensures critical environment variables (like `EXOMONAD_ROOT`) are not empty before spawning the agent.


## Architectural Pillars of ExoMonad Idiomatic Haskell

The Haskell codebase adheres to a set of core principles designed for maximum safety, testability, and type-level expressiveness:

1. **Extensible Effect Manifold**: Business logic resides strictly in the `Eff` monad (`freer-simple`), ensuring agents remain IO-blind. Interpreters mediate all world interactions (FS, Network, Time) at the application edge.
2. **Servant-Patterned Graphs**: Graph DSLs use the `mode :- nodeDef` record pattern. This allows a single record structure to serve as a type-level specification (`AsGraph` mode) and a runtime handler map (`AsHandler` mode).
3. **Inductive Type-Safe Dispatch**: Graph traversal is performed via recursive typeclass dispatch on `OneOf` sum types (via `GotoChoice`), providing fully typed state transitions without `Dynamic` or `unsafeCoerce`.
4. **Proof-Carrying Structured I/O**: The `StructuredOutput` system unifies JSON Schema generation, encoding, and diagnostic parsing. Success types carry proof of validity, ensuring "Parse, Don't Validate" remains the standard.
5. **Strict Compile-Time Guardrails**: Extensive use of `TypeError` and `Unsatisfiable` constraints to catch graph errors—such as unreachable nodes, dead ends, or invalid transition targets—during compilation.

## Gemini Added Memories
- Implemented TUI-interactive MCP tools (confirm_action, select_option, request_guidance) in haskell/control-server. Verified via mock TUI logic. Committed changes.
- Documented macOS arm64 requirement: binaries in `runtime/bin` (`exomonad-control-server`, `exomonad`, `tui-sidebar`) must be ad-hoc signed (`codesign -s -`) to avoid `Killed: 9` errors.
- Refactored subagent spawning to pass environment variables directly through the Docker API instead of writing `.env` files, resolving environment contamination and shadowing issues.