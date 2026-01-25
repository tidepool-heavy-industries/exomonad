# DockerSpawner Interpreter

HTTP-based interpreter for the `DockerSpawner` effect.

## Overview

This package implements the `DockerSpawner` effect by making HTTP requests to the `docker-spawner` service (a Rust service typically running on port 7435).

## Build

```bash
cabal build tidepool-docker-spawner-interpreter
```

## API Endpoints

The interpreter communicates with docker-spawner's REST API:

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/spawn` | POST | Create a new container |
| `/status/{id}` | GET | Get container status |
| `/stop/{id}` | POST | Stop a container |
| `/exec/{id}` | POST | Execute command in container |

### Remote Execution

The `/exec/{id}` endpoint replaces the previous SSH-based approach for running commands in agent containers:

```bash
# Execute command in container
curl -X POST http://localhost:7435/exec/tidepool-tl \
  -H "Content-Type: application/json" \
  -d '{
    "cmd": ["cabal", "build"],
    "workdir": "/workspace",
    "env": ["PATH=/home/agent/.ghcup/bin:$PATH"],
    "user": "agent"
  }'
```

**Response:**
```json
{
  "exit_code": 0,
  "stdout": "...",
  "stderr": "..."
}
```

## Configuration

Set `DOCKER_SPAWNER_URL` environment variable to the docker-spawner service URL:

```bash
export DOCKER_SPAWNER_URL=http://docker-spawner:7435  # Docker
export DOCKER_SPAWNER_URL=http://localhost:7435      # Local
```

## Dependencies

- `tidepool-core`: Defines the `DockerSpawner` effect
- `http-client`: For making HTTP requests
- `aeson`: For JSON serialization

## Key Modules

- `Tidepool.DockerSpawner.Interpreter`: Main interpreter logic (`runDockerSpawner`)

## Migration from SSH

This interpreter replaces the previous `ssh-proxy` approach:

| Old (ssh-proxy) | New (docker-spawner) |
|-----------------|----------------------|
| SSH connections | Docker exec API |
| Key management | No authentication |
| sshd in containers | No SSH needed |
| Port 22 | HTTP on 7435 |

See `rust/ssh-proxy/CLAUDE.md` for historical context.
