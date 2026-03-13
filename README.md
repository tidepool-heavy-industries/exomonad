# ExoMonad

You're in Claude Code. You need to implement a feature that touches 8 files across Rust and Haskell. Instead of doing it yourself, you describe three tasks and spawn Gemini workers. Three panes open in your Zellij session. Each worker implements its piece. Messages arrive in your conversation when they finish. You merge the results. Total cost: 10-30x less than doing it yourself in Opus.

ExoMonad is an agent orchestration runtime that gives Claude Code (and Gemini CLI) the ability to spawn, coordinate, and manage teams of LLM agents. It integrates directly into your existing CLI workflow — no new UI, no web dashboard, no context switching.

## What It Looks Like

```
You (in Claude Code):  "Implement close_self effect — proto, Rust handler, Haskell types, plugin"

Claude spawns 3 Gemini workers:

  ┌─ TL (Claude Opus) ──────────────────────────────────────────────┐
  │  You: "implement the plan"                                      │
  │  Claude: spawning 3 workers...                                  │
  │                                                                 │
  │  [from: rust-close-self] Done. cargo build passes.              │
  │  [from: haskell-close-self] Done. cabal build passes.           │
  │  [from: plugin-close-pane] Done. wasm32 build passes.           │
  ├─────────────────────────────────────────────────────────────────┤
  │ 💎 rust-close-self  │ 💎 haskell-close-self │ 💎 plugin-close  │
  │ (working...)        │ (working...)          │ (working...)      │
  └─────────────────────────────────────────────────────────────────┘
```

Workers run in parallel, notify you when done, and close their own panes.

## Install

Requires [Nix](https://nixos.org/) and [Zellij](https://zellij.dev/).

```bash
git clone https://github.com/tidepool-heavy-industries/exomonad
cd exomonad
just install-all      # Release build (optimized, slower compile)
# or
just install-all-dev  # Debug build (fast compile, good for development)
```

This builds the Haskell WASM plugin (via Nix), the Rust binary, and the Zellij plugin, then installs everything to `~/.cargo/bin/` and `~/.config/zellij/plugins/`.

## Getting Started

```bash
cd your-project/
exomonad init       # Creates Zellij session with Server + TL tabs
                    # Writes .mcp.json (auto-registers MCP tools)
                    # Starts background server on .exo/server.sock
```

You're now in a Zellij session. Switch to the **TL tab** and run `claude`. ExoMonad's MCP tools are available immediately — Claude can spawn agents, file PRs, and coordinate work.

### Use on any project

ExoMonad works on any git repository. After installing, just init:

```bash
cd ~/my-project
exomonad init
# → Copies WASM from ~/.exo/wasm/, starts server, MCP registered
```

Or try it in a container with zero setup: see [try-exomonad/](try-exomonad/).

## How It Works

**Three layers, each doing one thing:**

| Layer | What | Why |
|-------|------|-----|
| **Haskell WASM** | Tool definitions, schemas, decision logic | Pure logic, no I/O, hot-reloadable |
| **Rust runtime** | Executes effects (git, GitHub API, filesystem, Zellij IPC) | Performance, safety |
| **Zellij** | Process isolation (tabs for subtrees, panes for workers) | Multiplexing without Docker |

Agents are IO-blind state machines compiled to WASM. They yield typed effects; Rust executes them. This means tool logic is deterministic, testable, and hot-reloadable — edit a Haskell tool, run `just wasm-all`, and the next MCP call picks up the change.

**Agent types:**

| Spawn tool | Creates | Isolation | Use case |
|------------|---------|-----------|----------|
| `spawn_workers` | Gemini panes in your tab | Shared directory, no branch | Fast parallel tasks (10-30x cheaper than Opus) |
| `spawn_leaf_subtree` | Gemini in own worktree + tab | Own branch, files PR | Independent features that need isolation |
| `spawn_subtree` | Claude in own worktree + tab | Own branch, can spawn children | Complex decomposition (TL role, recursive) |

**Communication:** Child agents call `notify_parent` when done. Messages arrive in your Claude conversation as native teammate notifications via the Teams inbox. No polling, no stdin hacks.

## Available Tools

| Tool | Role | Description |
|------|------|-------------|
| `spawn_subtree` | tl | Fork a Claude agent into a new worktree and Zellij tab |
| `spawn_leaf_subtree` | tl | Fork a Gemini agent into a new worktree and Zellij tab |
| `spawn_workers` | tl | Spawn Gemini agents as panes (ephemeral, no branch) |
| `file_pr` | tl, dev | Create or update a PR for the current branch |
| `merge_pr` | tl | Merge a child agent's PR and fetch changes |
| `popup` | tl | Show interactive forms in a tiled split pane |
| `notify_parent` | all | Send message to parent agent via Teams inbox |
| `send_message` | all | Send message to any agent (Teams, ACP, UDS, or Zellij) |
| `shutdown` | dev, worker | Gracefully exit: notify parent, close own pane |

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
