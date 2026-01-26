# Docker Control CLI (docker-ctl)

CLI tool for managing the lifecycle of ExoMonad agent containers. Replaces the older `docker-spawner` HTTP service to eliminate network overhead and simplify orchestration.

## Commands

```bash
# Spawn a new agent container
docker-ctl spawn \
  --issue-id <id> \
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
- `EXOMONAD_AGENT_IMAGE` - Docker image for agents (default: `exomonad-agent:latest`)
- `EXOMONAD_NETWORK` - Docker network for agents (default: `exomonad`)
- `EXOMONAD_WORKTREES_VOLUME` - Docker volume for worktrees (default: `exomonad-worktrees`)
- `HOST_UID` - Default UID for agent user (default: `1000`)
- `HOST_GID` - Default GID for agent user (default: `1000`)

## Worktree Mounting

The `spawn` command uses a **named Docker volume** (not bind mounts) to share worktrees between containers. This is required because Docker runs on a remote host where local paths don't exist.

**How it works:**
1. `--worktree-path` is passed (e.g., `/worktrees/gh-346-test-issue`)
2. The worktree directory name is extracted (e.g., `gh-346-test-issue`)
3. The entire `exomonad-worktrees` volume is mounted to `/worktrees` in the container
4. The container's working directory is set to `/worktrees/{worktree_dir}`

This allows both control-server and agent containers to share the same worktrees volume.

## Design

- **Stateless**: Doesn't maintain its own state; queries Docker API directly.
- **Synchronous**: Blocks until the operation completes (or exec stream finishes).
- **Buffered**: `exec` buffers all output before returning JSON (simplifies Haskell side).
```