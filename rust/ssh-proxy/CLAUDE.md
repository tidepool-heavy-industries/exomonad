# ssh-proxy

> **DEPRECATED:** This crate is no longer used. Remote command execution in agent containers is now handled by the `docker-ctl` CLI tool via its `exec` command.
>
> See: `rust/docker-ctl/` and `docker/control-server/Dockerfile`

## Migration

The SSH-based approach has been replaced with Docker exec:

| Old (ssh-proxy) | New (docker-ctl) |
|-----------------|----------------------|
| SSH connection to container | Docker exec via bollard API |
| Key management required | No authentication needed |
| Port 22 + sshd in container | CLI tool (no persistent port) |
| `POST /exec` with SSH | `docker-ctl exec {id}` with Docker exec |

## New Approach

```bash
# Execute command in container
docker-ctl exec tidepool-tl -- echo hello
```

The docker-ctl tool uses bollard (Rust Docker API client) to execute commands directly via `docker exec`, eliminating the need for SSH infrastructure in agent containers.

## Historical Context

This crate provided SSH-based remote execution for agent containers. It was replaced because:

1. **Simpler**: No SSH key generation, distribution, or management
2. **Fewer dependencies**: Agent containers don't need sshd
3. **Native Docker**: Uses Docker's built-in exec capability
4. **Unified API**: Container lifecycle and exec in one service
