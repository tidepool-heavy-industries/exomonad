# docker-spawner

HTTP service for container lifecycle management. Replaces SSH-based remote execution with Docker's native exec API.

## Purpose

Provides a REST API for:
- **Spawning** agent containers with proper mounts and labels
- **Executing** commands inside running containers (replaces SSH)
- **Monitoring** container status
- **Stopping** containers with cleanup

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│ control-server (Haskell)                                        │
│  • DockerSpawner effect → HTTP calls to docker-spawner          │
└───────────────────────────────┬─────────────────────────────────┘
                                │ HTTP :7435
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│ docker-spawner container                                        │
│  • Axum HTTP server on port 7435                                │
│  • bollard crate for Docker API                                 │
│  • Mounted /var/run/docker.sock                                 │
└───────────────────────────────┬─────────────────────────────────┘
                                │ Docker socket
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│ Docker daemon                                                    │
│  • Creates/manages agent containers                              │
│  • Provides exec API for remote command execution               │
└─────────────────────────────────────────────────────────────────┘
```

## API Endpoints

### `GET /health`

Health check endpoint.

**Response:** `"OK"`

### `POST /spawn`

Create and start a new agent container.

**Request:**
```json
{
  "bead_id": "tidepool-abc",
  "worktree_path": "/worktrees/tidepool-abc",
  "backend": "claude",
  "uid": 1000,
  "gid": 1000,
  "expires_at": "2026-01-26T00:00:00Z"
}
```

**Response:**
```json
{
  "container_id": "a1b2c3d4...",
  "hostname": "tidepool-agent-tidepool-abc"
}
```

**Container labels:**
- `com.tidepool.bead_id` - Bead ID for tracking
- `com.tidepool.role` - Always "agent"
- `com.tidepool.expires_at` - Expiration timestamp (or "never")

### `GET /status/{id}`

Get container status.

**Response:**
```json
{
  "status": "running"  // or "exited", "not_found", etc.
}
```

### `POST /stop/{id}`

Stop and remove a container (10s graceful timeout).

**Response:** `200 OK` on success

### `POST /exec/{id}`

Execute a command inside a running container.

**Request:**
```json
{
  "cmd": ["cabal", "build"],
  "workdir": "/workspace",
  "env": ["PATH=/home/agent/.ghcup/bin:$PATH"],
  "user": "agent"
}
```

**Response:**
```json
{
  "exit_code": 0,
  "stdout": "Build output...",
  "stderr": ""
}
```

**Fields:**
- `cmd` (required): Command and arguments
- `workdir` (optional): Working directory inside container
- `env` (optional): Additional environment variables
- `user` (optional): User to run as (default: container's user)

## Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `TIDEPOOL_AGENT_IMAGE` | `tidepool-agent:latest` | Docker image for spawned agents |
| `HOST_UID` | `1000` | Default UID for container processes |
| `HOST_GID` | `1000` | Default GID for container processes |
| `TIDEPOOL_NETWORK` | `tidepool` | Docker network to join |
| `RUST_LOG` | - | Tracing log level |

## Building

```bash
# Build standalone
cargo build -p docker-spawner --release

# Build Docker image
docker build -f docker/docker-spawner/Dockerfile -t tidepool-docker-spawner .
```

## Running

### Docker Compose (recommended)

```yaml
services:
  docker-spawner:
    build:
      dockerfile: docker/docker-spawner/Dockerfile
    container_name: tidepool-docker-spawner
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock
    environment:
      - TIDEPOOL_AGENT_IMAGE=tidepool-agent:latest
      - TIDEPOOL_NETWORK=tidepool-internal
    networks:
      - tidepool-internal
```

### Standalone

```bash
# Must have access to Docker socket
RUST_LOG=info docker-spawner
```

## Testing

```bash
# Run tests
cargo test -p docker-spawner

# Manual API test
curl http://localhost:7435/health
# → "OK"

# Test exec
curl -X POST http://localhost:7435/exec/tidepool-tl \
  -H "Content-Type: application/json" \
  -d '{"cmd": ["echo", "hello"]}'
# → {"exit_code":0,"stdout":"hello\n","stderr":""}
```

## Migration from SSH

This service replaces the previous `ssh-proxy` approach:

| Old (ssh-proxy) | New (docker-spawner) |
|-----------------|----------------------|
| SSH daemon in each container | No SSH needed |
| SSH key generation/distribution | No authentication |
| Port 22 per container | Single HTTP endpoint |
| `POST /exec` with SSH | `POST /exec/{id}` with Docker exec |
| Separate spawn/exec services | Unified API |

**Benefits:**
- Simpler: No SSH infrastructure to manage
- Faster: Direct Docker API, no SSH handshake
- Unified: Container lifecycle and exec in one service
- Native: Uses Docker's built-in capabilities

## Key Modules

| File | Purpose |
|------|---------|
| `main.rs` | Axum HTTP server, route definitions |
| `spawner.rs` | Spawner struct, bollard Docker client |

## Dependencies

- **axum** - HTTP server framework
- **bollard** - Docker API client
- **futures-util** - Async stream handling for exec output
- **tracing** - Structured logging

## Related Documentation

- **[haskell/effects/docker-spawner-interpreter/CLAUDE.md](../../haskell/effects/docker-spawner-interpreter/CLAUDE.md)** - Haskell interpreter
- **[rust/ssh-proxy/CLAUDE.md](../ssh-proxy/CLAUDE.md)** - Deprecated SSH approach
- **[docker/docker-spawner/Dockerfile](../../docker/docker-spawner/Dockerfile)** - Container build
