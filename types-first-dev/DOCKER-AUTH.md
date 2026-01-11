# Docker Authentication Setup for types-first-dev

This document describes how to set up shared Claude Code authentication for the Docker-based execution environment.

## Platform-Specific Behavior

### Linux/WSL (Recommended)

On Linux and WSL, mantle automatically bind-mounts the host's `~/.claude` directory into containers. **No setup required** if you've already authenticated with `claude login` on the host.

```bash
# Just authenticate on host (if not already done)
claude login

# That's it! mantle will mount ~/.claude:/home/user/.claude automatically
```

### macOS

On macOS, Claude stores OAuth tokens in the system keychain rather than in `~/.claude`, so bind-mounting the directory doesn't work. You need a Docker volume with credentials.

## Overview (macOS Only)

Instead of passing OAuth tokens via environment variables (fragile), we use a shared Docker volume that contains Claude's authentication credentials. You authenticate once in a persistent container, and all worker containers inherit that authentication.

```
┌───────────────────────────────────────────────────────────────────┐
│                         Host (macOS)                              │
│  ┌─────────────┐    ┌─────────────┐                              │
│  │   Haskell   │───▶│   mantle    │──┐                           │
│  │   runner    │    │  (binary)   │  │ spawns containers         │
│  └─────────────┘    └─────────────┘  │ with -v auth_volume       │
└──────────────────────────────────────┼───────────────────────────┘
                                       │
┌──────────────────────────────────────┼───────────────────────────┐
│                      Docker          ▼                           │
│  ┌──────────────┐    ┌──────────────────┐                       │
│  │  auth-shell  │    │  Worker (Claude)  │                       │
│  │ (persistent) │    │  (ephemeral)      │                       │
│  └──────┬───────┘    └────────┬─────────┘                       │
│         │                     │                                  │
│         ▼                     ▼                                  │
│  ┌────────────────────────────────────────┐                     │
│  │     tidepool-claude-auth (volume)      │                     │
│  │     /home/user/.claude                 │                     │
│  └────────────────────────────────────────┘                     │
└─────────────────────────────────────────────────────────────────┘
```

## Initial Setup

### 1. Build the Docker image

From the repo root:

```bash
cd rust
docker build -t tidepool/claude-code:latest -f mantle/Dockerfile .
```

### 2. Start the auth-shell container

```bash
cd types-first-dev
docker-compose up -d
```

This creates:
- `tidepool-auth-shell` container (persistent)
- `tidepool-claude-auth` volume (shared auth credentials)

### 3. Authenticate with Claude

```bash
docker-compose exec auth-shell claude login
```

Follow the OAuth flow in your browser. Once complete, credentials are stored in the shared volume.

### 4. Configure mantle

Create `~/.config/mantle/config.toml`:

```toml
[docker]
auth_volume = "tidepool-claude-auth"
image = "tidepool/claude-code:latest"
```

### 5. Run the orchestrator

```bash
./run-actor-graph.sh specs/url-shortener.yaml
```

Mantle will now mount the shared auth volume into worker containers.

## Verification

Check auth status:
```bash
docker-compose exec auth-shell claude --version
```

Check volume exists:
```bash
docker volume ls | grep claude-auth
```

Check mantle config:
```bash
cat ~/.config/mantle/config.toml
```

## Re-authentication

If auth expires or you need to re-authenticate:

```bash
docker-compose exec auth-shell claude login
```

## Troubleshooting

### "Invalid API key" errors

1. Check auth-shell has valid credentials:
   ```bash
   docker-compose exec auth-shell ls -la /home/user/.claude
   ```

2. Verify volume is mounted in workers:
   ```bash
   docker inspect <container-name> | grep -A5 Mounts
   ```

3. Check mantle config is loaded:
   ```bash
   RUST_LOG=debug mantle session start --help 2>&1 | grep config
   ```

### Volume not found

If the volume doesn't exist, recreate it:

```bash
docker-compose down
docker volume rm tidepool-claude-auth
docker-compose up -d
# Re-authenticate
docker-compose exec auth-shell claude login
```

### Permission issues

The image runs as `user` (non-root). If you see permission errors:

```bash
docker-compose exec auth-shell ls -la /home/user/.claude
# Should show user:user ownership
```

## Config Reference

`~/.config/mantle/config.toml`:

```toml
[docker]
# Named Docker volume containing Claude auth credentials.
# When set, mounted to /home/user/.claude in worker containers.
auth_volume = "tidepool-claude-auth"

# Docker image to use for Claude Code containers.
# Default: "mantle-agent:latest"
image = "tidepool/claude-code:latest"
```

If no config file exists, mantle falls back to mounting host's `~/.claude` directly.
