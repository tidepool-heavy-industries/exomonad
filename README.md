# ExoMonad

Exomonad builds on the agentic loop to provide a Tree-of-agents model where a root 'tech lead' agent forks its context windows across multiple worktrees to recursively unfold an 'agentic tree' that accumulates scaffolding commits and context as it grows. Swarms of fast cheap agents implement specs at the leaf nodes. Each node is an agent that files PRs against its parent over waves of recursively nested trunk based development. Agents form a supervision hierarchy, reviewing and merging PRs filed by their child agents. All agents can communicate with their spawned child agents and their parent agent, using the tree to route messages.

It hooks into Claude Code and Gemini CLI, using their existing binaries and your existing subscription plans. Opus decomposes and dispatches. Gemini implements. Copilot reviews. Each model does what it's best at. All orchestration logic — tool dispatch, hooks, event handling, PR review routing — is defined in Haskell effects executed by a shared Rust server. Agents run in tmux windows and panes, isolated via git worktrees. No Docker, no web dashboard, no new UI to learn.

![tmux devswarm — TL dispatching to three Gemini workers in parallel, each in its own worktree. Bottom panes show workers mid-execution.](img/exomonad_tmux_devswarm.png)

## Try It

Run ExoMonad on any GitHub repo. One command, clean container, no local dependencies beyond Docker.

```bash
git clone https://github.com/tidepool-heavy-industries/exomonad
cd exomonad
just install-all-dev                              # Build artifacts (first time only)
./try-exomonad/run.sh https://github.com/user/repo
```

This builds a Docker image with the correct tmux version, pre-built WASM, and all dependencies. You land in a tmux session with MCP tools ready. Auth is automatic — your `~/.claude` and `~/.gemini` credentials are mounted from the host.

See [try-exomonad/README.md](try-exomonad/README.md) for details.

## Install (Native)

**Prerequisites:** [Nix](https://nixos.org/) (with flakes), [tmux](https://github.com/tmux/tmux/wiki), and [just](https://github.com/casey/just).

Install Nix if you don't have it:

```bash
sh <(curl -L https://nixos.org/nix/install) --daemon
mkdir -p ~/.config/nix && grep -q 'nix-command flakes' ~/.config/nix/nix.conf 2>/dev/null || echo 'experimental-features = nix-command flakes' >> ~/.config/nix/nix.conf
```

Then build and install:

```bash
git clone https://github.com/tidepool-heavy-industries/exomonad
cd exomonad
just install-all      # Release build (optimized, slower compile)
# or
just install-all-dev  # Debug build (fast compile, good for development)
```

First build downloads Nix dependencies and initializes the WASM toolchain — subsequent builds are cached. Artifacts are installed to `~/.cargo/bin/exomonad` and `~/.exo/wasm/`.

## Getting Started

```bash
cd your-project/
exomonad init       # Creates tmux session with Server + TL windows
                    # Writes .mcp.json (auto-registers MCP tools)
                    # Starts background server on .exo/server.sock
```

You're now in a tmux session. Switch to the **TL window** and run `claude`. ExoMonad's MCP tools are available immediately — Claude can spawn agents, file PRs, and coordinate work.

### Use on any project

ExoMonad works on any git repository. After installing, just init:

```bash
cd ~/my-project
exomonad init
# → Copies WASM from ~/.exo/wasm/, starts server, MCP registered
```

## How It Works

**Three layers, each doing one thing:**

| Layer | What | Why |
|-------|------|-----|
| **Haskell WASM** | Typed config DSL: tool schemas, dispatch, hooks, event routing | Deterministic, testable, hot-reloadable |
| **Rust runtime** | Executes effects (git, GitHub API, filesystem, tmux CLI) | Performance, safety |
| **tmux** | Process isolation (windows for subtrees, panes for workers) | Multiplexing without Docker |

Haskell WASM is a typed configuration DSL — tool schemas, dispatch logic, hooks, event routing — with the full power of a type system and effect system. The WASM yields typed effects; Rust executes the I/O. This means tool logic is deterministic, testable, and hot-reloadable — edit a Haskell tool, run `just wasm-all`, and the next MCP call picks up the change.

**Agent types:**

| Spawn tool | Creates | Isolation | Use case |
|------------|---------|-----------|----------|
| `spawn_gemini` (inline) | Gemini panes in your window | Shared directory, no branch | Fast parallel tasks (10-30x cheaper than Opus) |
| `spawn_gemini` (worktree) | Gemini in own worktree + window | Own branch, files PR | Independent features that need isolation |
| `fork_wave` | Claude in own worktree + window | Own branch, can spawn children | Complex decomposition (TL role, recursive) |

**Communication:** Child agents call `notify_parent` when done. Messages arrive in your Claude conversation as native teammate notifications via the Teams inbox. No polling, no stdin hacks.

## Available Tools

| Tool | Role | Description |
|------|------|-------------|
| `fork_wave` | root, tl | Fork N parallel Claude agents, each in its own worktree |
| `spawn_gemini` | root, tl | Spawn Gemini agent (worktree, inline, or standalone isolation) |
| `file_pr` | tl, dev | Create or update a PR for the current branch |
| `merge_pr` | root, tl | Merge a child agent's PR and fetch changes |
| `notify_parent` | tl, dev, worker | Send message to parent agent via Teams inbox |
| `send_message` | all | Send message to any agent (Teams, ACP, UDS, or tmux) |
| `shutdown` | dev, worker | Gracefully exit: notify parent, close own pane |
| `task_list` | dev, worker | List tasks from shared task list |
| `task_get` | dev, worker | Get task by ID |
| `task_update` | dev, worker | Update task status/owner/activeForm |

## Development

```bash
just install-all-dev    # Full build (WASM + Rust + install)
just wasm-all           # Rebuild WASM only (after Haskell changes)
just proto-gen          # Regenerate proto types (Rust + Haskell)
cargo test --workspace  # Rust tests
just fmt                # Format all code
```

All `just` recipes handle their own Nix dependencies — no need to be in a `nix develop` shell.

See [CLAUDE.md](CLAUDE.md) for the full architecture, data flows, and contributor guide.

## License

ExoMonad is released under the [BSD 3-Clause License](LICENSE).
