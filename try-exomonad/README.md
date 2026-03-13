# ExoMonad Try

Run ExoMonad on any GitHub repository. One command, clean container, no setup.

## Quick Start

```bash
# From the exomonad repo root:
./try-exomonad/run.sh https://github.com/BurntSushi/ripgrep
```

**Prerequisites:**
- Docker
- Claude Code logged in on host (`claude` must have been run at least once)
- Pre-built artifacts (`just install-all-dev` from exomonad repo)

**Optional:**
- `GITHUB_TOKEN` — enables `gh` CLI, PR workflows, agent spawning
- Gemini CLI logged in on host — enables Gemini agent spawning

Auth is automatic: `~/.claude` and `~/.gemini` are mounted from the host, so the container reuses your existing subscriptions and credentials. No API keys needed.

## What Happens

1. **`run.sh`** stages pre-built WASM artifacts from your host
2. Builds a Docker image (cached after first run) with:
   - `exomonad` binary (compiled from source in a Rust builder stage)
   - tmux, gh CLI, Node.js, Claude Code
   - Pre-built Haskell WASM plugin
3. Launches a container that clones the target repo
4. **`bootstrap.sh`** copies WASM into the repo, configures git, runs `exomonad init`
5. You land in a tmux session with Server + TL windows

## What You Can Do

Once in the tmux session:
- **TL window**: Run `claude` to start Claude Code with full MCP tool access
- **Spawn agents**: Use `spawn_leaf_subtree` to fork Gemini workers into worktrees
- **File PRs**: Agents call `file_pr` to create pull requests
- **Coordinate**: `notify_parent` delivers messages between agents via Teams inbox

## Verification Checklist

| # | Test | Command |
|---|------|---------|
| 1 | Binary runs | `exomonad --version` |
| 2 | Server starts | `curl --unix-socket .exo/server.sock http://localhost/health` |
| 3 | tmux session | `exomonad init` shows Server + TL windows |
| 4 | Tools listed | `curl --unix-socket .exo/server.sock http://localhost/agents/tl/root/tools` |
| 5 | Spawn works | `spawn_leaf_subtree` in Claude TUI creates new window |
| 6 | Comms work | Agent calls `notify_parent` and message arrives in TL |
| 7 | PR workflow | Agent calls `file_pr`, TL calls `merge_pr` |

## Troubleshooting

**"Missing artifact" error from run.sh:**
Build artifacts first: `just install-all-dev` from the exomonad repo root.

**Rust build fails in Docker:**
The builder stage needs network access for crates.io. Check Docker's network settings.

**tmux doesn't render properly:**
Ensure your terminal supports 256 colors and you're running `docker run -it` (interactive + TTY).

**Claude Code can't connect:**
Run `claude` on your host machine first to log in. The container mounts `~/.claude` from the host for auth.

**`exomonad init` warns about missing roles:**
Expected. The WASM is pre-built — roles are only needed for WASM compilation, not runtime.

## Architecture

```
Host (your machine)
  ├── ~/.claude/                  ──mount──►  /home/exo/.claude/     (auth + cache)
  ├── ~/.claude.json              ──mount──►  /home/exo/.claude.json (config)
  ├── ~/.gemini/                  ──mount──►  /home/exo/.gemini/     (auth + cache)
  └── docker run -it exomonad-try
        └── Container (Ubuntu 24.04, user: exo)
              ├── /usr/local/bin/exomonad     (Rust binary, built in Docker)
              ├── /opt/exomonad/wasm/         (pre-built Haskell WASM)
              └── /workspace/project/         (cloned target repo)
                    ├── .exo/wasm/            (WASM copied by bootstrap)
                    ├── .exo/server.sock      (MCP server socket)
                    └── ... (target repo files)
```
