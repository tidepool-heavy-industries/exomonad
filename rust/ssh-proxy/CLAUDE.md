# ssh-proxy

> **DEPRECATED:** This crate is no longer used. Remote command execution in agent containers is now handled by the `docker-spawner` service via its `/exec/{id}` endpoint.
>
> See: `rust/docker-spawner/` and `docker/docker-spawner/Dockerfile`

## Migration

The SSH-based approach has been replaced with Docker exec:

| Old (ssh-proxy) | New (docker-spawner) |
|-----------------|----------------------|
| SSH connection to container | Docker exec via bollard API |
| Key management required | No authentication needed |
| Port 22 + sshd in container | HTTP API on port 7435 |
| `POST /exec` with SSH | `POST /exec/{id}` with Docker exec |

## New Approach

```bash
# Execute command in container
curl -X POST http://localhost:7435/exec/tidepool-tl \
  -H "Content-Type: application/json" \
  -d '{"cmd": ["echo", "hello"], "workdir": "/workspace"}'
```

The docker-spawner service uses bollard (Rust Docker API client) to execute commands directly via `docker exec`, eliminating the need for SSH infrastructure in agent containers.

## Historical Context

This crate provided SSH-based remote execution for agent containers. It was replaced because:

1. **Simpler**: No SSH key generation, distribution, or management
2. **Fewer dependencies**: Agent containers don't need sshd
3. **Native Docker**: Uses Docker's built-in exec capability
4. **Unified API**: Container lifecycle and exec in one service
