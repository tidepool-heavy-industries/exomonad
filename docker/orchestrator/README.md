# Tidepool Orchestrator Container

The Orchestrator container provides a centralized environment for managing Tidepool agents and services. It uses Zellij as a terminal multiplexer and runs the `tidepool-control-server` as a background service.

## Features

- **Zellij IDE**: Multi-pane terminal interface for managing services and agents.
- **Control Server**: Pre-configured `tidepool-control-server` running in a dedicated pane.
- **Docker-out-of-Docker (DooD)**: Ability to spawn and manage sibling agent containers.
- **Zombie Reaping**: Uses `tini` as PID 1 for proper signal handling and process management.

## Prerequisites

- Linux x86_64 binaries for Tidepool components:
    - `tidepool-control-server`
    - `tui-sidebar`
    - `tui-popup` (optional/in-progress)
    - `mantle-agent` (recommended)

Place these binaries in the `bin/` directory before building the image.

## Build

```bash
cd docker/orchestrator
mkdir -p bin
# Copy Linux binaries into bin/
docker build -t tidepool/orchestrator .
```

## Run

Run the orchestrator with the following command:

```bash
docker run -it --rm \
  --name orchestrator \
  --detach-keys="ctrl-e,e" \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v $(pwd)/worktrees:/worktrees \
  -v ~/.claude.json:/root/.claude.json:ro \
  -v tidepool-sockets:/sockets \
  -v tidepool-zellij-cache:/root/.cache/zellij \
  tidepool/orchestrator
```

### Mounts and Flags

- `--detach-keys="ctrl-e,e"`: Allows detaching from the container without killing the Zellij session.
- `-v /var/run/docker.sock:/var/run/docker.sock`: Enables DooD (Docker-out-of-Docker) for sibling container management.
- `-v $(pwd)/worktrees:/worktrees`: Shared workspace for agents.
- `-v ~/.claude.json:/root/.claude.json:ro`: Shares Claude Code authentication/config.
- `-v tidepool-sockets:/sockets`: Named volume for Unix Domain Sockets.
- `-v tidepool-zellij-cache:/root/.cache/zellij`: Named volume for Zellij session persistence.

## Layout

The orchestrator launches with a default Zellij layout:
- **Control Server**: (Left) Real-time logs and status of the Haskell control server.
- **Shell**: (Top Right) Interactive bash shell for manual commands and agent spawning.
- **Logs**: (Bottom Right) Tail of `/var/log/tidepool/control-server.log`.
