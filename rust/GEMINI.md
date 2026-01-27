# Gemini Code Intelligence: Rust Workspace

This workspace contains the Rust-based infrastructure for the ExoMonad system, focusing on augmenting human-driven CLI sessions (Claude Code, Gemini CLI) with interactive UIs, MCP tools, and lifecycle hooks.

## Project Overview

The Rust workspace consists of several crates that bridge the gap between human-facing CLI tools and the Haskell-based control server.

### Core Components

- **`exomonad`**: The primary integration point for CLI tools.
  - **Hooks**: Forwards Claude Code/Gemini hooks (PreToolUse, PostToolUse, etc.) to the control server.
  - **MCP Server**: Implements the Model Context Protocol (MCP) to expose ExoMonad-specific tools to the agent.
  - **Design**: Uses synchronous Unix sockets for minimal overhead and reliability during blocking hook execution.
- **`tui-sidebar`**: A terminal-based interactive UI.
  - **Technology**: Built with `ratatui` and `tokio`.
  - **Function**: Connects to the control server to render graph visualizations and interactive components (checkboxes, text inputs, etc.) based on `UISpec` messages.
- **`exomonad-shared`**: Common library for all Rust crates.
  - Defines the NDJSON protocol for control socket communication.
  - Provides Unix socket clients and process supervision utilities.
  - Contains shared types for hooks, MCP calls, and telemetry.
- **`exomonad-hub`**: (WIP) A telemetry and metrics hub for tracking session progress and tool usage.

## Architecture & Communication

The system follows a sidecar/agent architecture:

1.  **CLI Tool** (e.g., Claude Code) executes a hook.
2.  **`exomonad`** is invoked, parses stdin JSON, and connects to the **Control Server** (Haskell) via Unix Domain Socket.
3.  **Control Server** processes the event and returns a response.
4.  **`tui-sidebar`** runs in a parallel pane, receiving UI updates from the Control Server to keep the human in the loop.

### Socket Paths (Canonical)
- Control Socket: `.exomonad/sockets/control.sock` (or `EXOMONAD_CONTROL_SOCKET` env)
- TUI Socket: `.exomonad/sockets/tui.sock` (or `EXOMONAD_TUI_SOCKET` env)

## Building and Running

### Building
```bash
# Build the entire workspace
cargo build --release

# Build a specific crate
cargo build -p exomonad
cargo build -p tui-sidebar
```

### Testing
```bash
# Run all tests
cargo test

# Run tests for a specific crate
cargo test -p exomonad-shared
```

### Running Components
```bash
# Handle a hook (invoked by CLI tool)
exomonad hook pre-tool-use

# Start the MCP server
exomonad mcp --tools pm_propose,pm_status

# Start the TUI sidebar (requires running control server)
tui-sidebar --socket .exomonad/sockets/tui.sock
```

## Development Conventions

- **IPC Protocol**: All communication over Unix sockets uses Newline-Delimited JSON (NDJSON).
- **Error Handling**: Use `ExoMonadError` (in `exomonad-shared`) or `anyhow` for top-level binaries.
- **Logging**: Use the `tracing` crate. Configuration is managed via `RUST_LOG` environment variable.
- **Fail-Safe Behavior**: Agents should aim for "fail-open" or graceful degradation when the control server is unavailable, though current development often uses "fail-closed" to catch configuration issues.
- **Sync vs Async**:
  - Use **Synchronous I/O** for CLI agents (`exomonad`) to match the blocking nature of hooks.
  - Use **Asynchronous I/O** (Tokio) for long-lived UI or server processes (`tui-sidebar`, `exomonad-hub`).
