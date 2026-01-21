# Gemini Code Intelligence

This project uses `CLAUDE.md` files for documentation and context. 

## Setup

Before using Gemini with this project, you must build the MCP server and link it to the location expected by `settings.json`.

Run the following command:

```bash
just setup-gemini
```

This will:
1. Build the `tidepool-mcp-server` executable.
2. Create a symlink at `./result/bin/mcp-server`.

## Documentation

Please refer to the following files for information:
- `CLAUDE.md` - Root project overview and architecture.
- `haskell/CLAUDE.md` - Haskell package organization and details.
- `rust/CLAUDE.md` - Rust workspace overview.
- Other `CLAUDE.md` files in subdirectories as linked from the root.

Gemini-specific configuration is located in `settings.json` at the project root.

## Tidepool Orchestration

The project uses a multi-process architecture orchestrated via Unix Domain Sockets (UDS).

### Core Components
- **`start-augmented.sh`**: The main entry point. Orchestrates the environment, sets canonical socket paths, and launches Zellij.
- **`.zellij/tidepool.kdl`**: Defines the TUI layout. Each pane uses `scripts/wait-for-socket.sh` to synchronize with backend readiness.
- **`process-compose.yaml`**: Supervises the `tidepool-control-server` (Haskell) which creates the control and TUI sockets.
- **`scripts/tidepool-runner.sh`**: Ensures graceful shutdown of `process-compose` services when the Zellij session ends.

### Socket Paths (Single Source of Truth)
- `TIDEPOOL_CONTROL_SOCKET`: Default `.tidepool/sockets/control.sock`
- `TIDEPOOL_TUI_SOCKET`: Default `.tidepool/sockets/tui.sock`

### Key Patterns
- **Supervision**: Zellij panes run bash loops that restart clients (Claude, TUI Sidebar) on crash.
- **Readiness**: Clients do not connect blindly; they wait for the UDS file to appear via `wait-for-socket.sh`.
- **TUI Detection**: `tui-sidebar` (Rust) checks for the existence of the control socket to determine if the TUI was explicitly disabled (e.g., via `--no-tui`) and exits gracefully if so.

## Subagent Spawning & Templates

The `spawn_agents` tool automates the creation of isolated worktrees for parallel task execution.

### Template Resolution
The tool (`SpawnAgents.hs`) resolves the `process-compose.yaml` template for new worktrees using the following priority:
1. **Hangar Root**: `$HANGAR_ROOT/templates/subagent-pc.yaml` (Preferred)
2. **Repo Fallback**: `<repo_root>/.tidepool/templates/subagent-pc.yaml`

### Process Compose Configuration
The template (`subagent-pc.yaml`) is critical for bootstrapping the subagent environment.
- **Log Location**: Must be set to `.tidepool/logs` (directory), not a specific file, to allow `process-compose` to manage log rotation and avoid "No such file" errors.
- **Binary Discovery**: Uses an inline shell script to walk up and find `Hangar.toml`, then resolves binaries from `$HANGAR_ROOT/runtime/bin`. This ensures robustness against PATH variations in subagent shells.
- **Sockets**: Explicitly manages `.tidepool/sockets/control.sock` and `.tidepool/sockets/tui.sock` via environment variables and cleanup scripts.
