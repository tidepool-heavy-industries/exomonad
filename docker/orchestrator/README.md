# Tidepool Orchestrator Container (Legacy)

> **DEPRECATED:** This monolithic orchestrator has been replaced by the Container Separation Architecture.
>
> **Use instead:** `docker compose up` which starts dedicated containers:
> - `tidepool-zellij` - Minimal visual multiplexer
> - `tidepool-control-server` - Haskell MCP server
> - `tidepool-tl` / `tidepool-pm` - Named agent containers
> - `tidepool-control-server` - Haskell MCP server (with `docker-ctl`)
>
> See root `CLAUDE.md` for the new architecture.

## Migration

The orchestrator combined multiple concerns in one container. The new architecture separates them:

| Old (orchestrator) | New (separated) |
|--------------------|-----------------|
| Zellij + control-server + shell | `zellij` container (minimal) |
| Built-in control-server | `control-server` container |
| Manual agent spawning | `tl` + `pm` containers (auto-start) |
| SSH for remote exec | `docker-ctl` CLI tool |

## Running Legacy Orchestrator

If you need the old behavior:

```bash
# Start with legacy profile
docker compose --profile legacy up orchestrator
docker attach tidepool-orchestrator
```

## Historical Documentation

The orchestrator provided:

- **Zellij IDE**: Multi-pane terminal interface
- **Control Server**: `tidepool-control-server` in dedicated pane
- **Docker-out-of-Docker**: Sibling container management
- **Zombie Reaping**: tini as PID 1

### Layout

Default Zellij layout:
- **Control Server** (Left): Real-time logs
- **Shell** (Top Right): Interactive bash
- **Logs** (Bottom Right): Log tail

### Mounts

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

## Files

| File | Purpose |
|------|---------|
| `Dockerfile` | Multi-stage build (Haskell + Rust + runtime) |
| `config/layout.kdl` | Zellij layout definition |
| `config/config.kdl` | Zellij configuration |
