# Claude Code Agent Container

This directory contains the Docker infrastructure for running Claude Code agents in isolated containers.

## Build

Before building the container, ensure the `mantle-agent` binary is built for Linux:

```bash
# Cross-compile mantle-agent for Linux (x86_64)
docker run --rm -v "$(pwd):/app" -w /app/rust rust:latest cargo build --release -p mantle-agent
cp rust/target/release/mantle-agent docker/claude-agent/bin/
```

Then build the container:

```bash
docker build -t tidepool/claude-agent:latest docker/claude-agent
```

## Running with Shared Volumes

To optimize performance and disk usage, the container supports mounting shared caches for Git, Cargo, and Cabal.

### 1. Git Cache (Alternates)

Create a bare clone of the repository on the host to serve as a shared object store. Set `preciousObjects` to true to prevent `git gc` from corrupting the shared objects.

```bash
mkdir -p ~/.cache/tidepool/git
git clone --bare https://github.com/tidepool-heavy-industries/tidepool.git ~/.cache/tidepool/git/tidepool.git
cd ~/.cache/tidepool/git/tidepool.git
git config extensions.preciousObjects true
```

When running the container, mount this directory and set `GIT_ALTERNATES_OBJECT_DIR`:

```bash
docker run -it \
  -v ~/.cache/tidepool/git/tidepool.git/objects:/cache/git-objects:ro \
  -e GIT_ALTERNATES_OBJECT_DIR=/cache/git-objects \
  ...
```

### 2. Authentication (OAuth Session)

Claude Code stores its OAuth session in `~/.claude.json`. Bind-mount your host's config to avoid re-authenticating inside the container.

```bash
docker run -it \
  -v ~/.claude.json:/home/agent/.claude.json:ro \
  ...
```

### 3. Full Run Command Example

Use `--detach-keys="ctrl-e,e"` to avoid `Ctrl+P` conflicts with Claude's navigation shortcuts.

```bash
docker run -it \
  --name claude-agent \
  --detach-keys="ctrl-e,e" \
  -v $(pwd):/workspace \
  -v ~/.claude.json:/home/agent/.claude.json:ro \
  -v ~/.cache/tidepool/git/tidepool.git/objects:/cache/git-objects:ro \
  -v ~/.cache/tidepool/cargo:/home/agent/.cargo/registry \
  -v ~/.cache/tidepool/cabal:/home/agent/.cabal/store \
  -v ~/.tidepool/sockets:/home/agent/.tidepool/sockets \
  -e GIT_ALTERNATES_OBJECT_DIR=/cache/git-objects \
  -e TIDEPOOL_CONTROL_SOCKET=/home/agent/.tidepool/sockets/control.sock \
  tidepool/claude-agent:latest
```

## Environment Variables

| Variable | Description |
|----------|-------------|
| `GIT_ALTERNATES_OBJECT_DIR` | Path inside container to the shared git object store. |
| `MANTLE_CONTROL_SOCKET` | Path to the control server Unix socket (default: `/home/agent/.tidepool/sockets/control.sock`). |

## Startup Configuration

The `entrypoint.sh` script automatically:
1. Configures `.claude/settings.json` with hooks pointing to `mantle-agent`.
2. Configures `.mcp.json` using the `http+unix` protocol for control socket communication.
3. Links the mounted git alternates if `GIT_ALTERNATES_OBJECT_DIR` is set.
4. Pre-seeds `~/.claude.json` to bypass onboarding prompts.

```
