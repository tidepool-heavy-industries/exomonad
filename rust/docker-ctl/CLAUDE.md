# Docker Control CLI (docker-ctl)

CLI tool for managing the lifecycle of Tidepool agent containers. Replaces the older `docker-spawner` HTTP service to eliminate network overhead and simplify orchestration.

## Commands

```bash
# Spawn a new agent container
docker-ctl spawn \
  --bead-id <id> \
  --worktree-path <path> \
  --backend <claude|gemini> \
  [--uid <uid>] [--gid <gid>] \
  [--expires-at <iso-date>]

# Execute command in a container
docker-ctl exec <container-id> \
  [--workdir <dir>] \
  [--user <user>] \
  [-e KEY=VALUE] ... \
  -- <command> [args...]

# Get container status
docker-ctl status <container-id>

# Stop and remove a container
docker-ctl stop <container-id> [--timeout <seconds>]
```

## JSON Output

All commands return structured JSON on `stdout`:

- **Spawn**: `{"container_id": "...", "hostname": "..."}`
- **Exec**: `{"exit_code": 0, "stdout": "...", "stderr": "..."}`
- **Status**: `{"status": "running"}` or `{"status": "not_found"}`
- **Stop**: `{"stopped": true}`

Errors are reported as JSON on `stderr` with exit code 1:
`{"error": "description"}`

## Environment Variables

Used by `spawn` command:
- `TIDEPOOL_AGENT_IMAGE` - Docker image for agents (default: `tidepool-agent:latest`)
- `TIDEPOOL_NETWORK` - Docker network for agents (default: `tidepool`)
- `HOST_UID` - Default UID for agent user (default: `1000`)
- `HOST_GID` - Default GID for agent user (default: `1000`)

## Design

- **Stateless**: Doesn't maintain its own state; queries Docker API directly.
- **Synchronous**: Blocks until the operation completes (or exec stream finishes).
- **Buffered**: `exec` buffers all output before returning JSON (simplifies Haskell side).
```