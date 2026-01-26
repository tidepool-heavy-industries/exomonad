# Claude Code Agent Container

Docker container for running Claude Code agents in the Tidepool container separation architecture.

## Architecture

Agent containers run Claude Code CLI and connect to the control-server via TCP MCP:

```
┌──────────────────────────────────────┐
│ claude-agent container               │
│  • Claude Code CLI                   │
│  • mantle-agent (hook forwarding)    │
│  • Haskell/Rust toolchains           │
│  • No SSH (uses docker exec)         │
└──────────────┬───────────────────────┘
               │ TCP :7432
               ▼
┌──────────────────────────────────────┐
│ control-server container             │
│  • MCP server                        │
│  • LSP (HLS)                         │
└──────────────────────────────────────┘
```

## Environment Variables

| Variable | Purpose |
|----------|---------|
| `TIDEPOOL_ROLE` | Agent role: `tl` (Tech Lead) or `pm` (Project Manager) |
| `CONTROL_SERVER_URL` | TCP URL for MCP (e.g., `http://control-server:7432`) |
| `ANTHROPIC_API_KEY` | Claude API key |
| `GIT_ALTERNATES_OBJECT_DIR` | Optional: shared git objects directory |

## MCP Configuration

The entrypoint automatically configures MCP based on environment:

**TCP transport (Docker - recommended):**
```json
{
  "mcpServers": {
    "tidepool": {
      "type": "http",
      "url": "http://control-server:7432/role/tl/mcp"
    }
  }
}
```

**Unix socket transport (local dev):**
```json
{
  "mcpServers": {
    "control": {
      "type": "http",
      "url": "http+unix://%2Fhome%2Fagent%2F.tidepool%2Fsockets%2Fcontrol.sock/mcp"
    }
  }
}
```

## Remote Execution

Remote command execution uses the `docker-ctl exec` CLI tool instead of SSH:

```bash
# Execute command in this container
docker-ctl exec tidepool-tl -- echo hello
```

This approach eliminates the need for:
- SSH daemon in the container
- SSH key generation/distribution
- Port 22 exposure

## Build

The container is built as part of docker-compose:

```bash
docker compose build tl pm
```

Or standalone:

```bash
docker build -t tidepool/claude-agent -f docker/claude-agent/Dockerfile .
```

## Usage via Docker Compose

```yaml
services:
  tl:
    build:
      dockerfile: docker/claude-agent/Dockerfile
    container_name: tidepool-tl
    tty: true
    stdin_open: true
    environment:
      - TIDEPOOL_ROLE=tl
      - CONTROL_SERVER_URL=http://control-server:7432
      - ANTHROPIC_API_KEY=${ANTHROPIC_API_KEY}
```

Attach via Zellij panes or directly:

```bash
docker attach tidepool-tl
# Detach: Ctrl+p, Ctrl+q
```

## Shared Caches

### Git Alternates

Share git objects between containers to save disk space:

```bash
# Host: create shared object store
git clone --bare https://github.com/org/repo.git ~/.cache/tidepool/git/repo.git
```

Mount and set environment:
```yaml
volumes:
  - ~/.cache/tidepool/git:/cache/git:ro
environment:
  - GIT_ALTERNATES_OBJECT_DIR=/cache/git/repo.git/objects
```

### Authentication

Mount Claude credentials (read-only recommended):

```yaml
volumes:
  - tidepool-claude-auth:/mnt/secrets:ro
```

The entrypoint links credentials from `/mnt/secrets/` into the config directory.

## Entrypoint Behavior

The `entrypoint.sh` script:

1. **Git alternates**: Links worktree to shared object store if `GIT_ALTERNATES_OBJECT_DIR` set
2. **Auth isolation**: Links credentials from `/mnt/secrets/` if mounted
3. **Claude hooks**: Creates `settings.json` with mantle-agent hooks (if not already linked)
4. **MCP config**: Generates `.mcp.json` based on `CONTROL_SERVER_URL` or `MANTLE_CONTROL_SOCKET`
5. **Exec via tini**: Launches CMD with proper signal handling

## Installed Toolchains

| Tool | Version | Purpose |
|------|---------|---------|
| GHC | 9.6.4 | Haskell compiler |
| Cabal | 3.10.2.1 | Haskell build tool |
| HLS | Latest | Haskell Language Server |
| Rust | Latest stable | Rust toolchain |
| Node.js | 20.x | JavaScript runtime |
| Claude Code | Latest | AI coding assistant |

## Files

| File | Purpose |
|------|---------|
| `Dockerfile` | Multi-stage build (rust builder → runtime) |
| `entrypoint.sh` | Container initialization |
