# ExoMonad

Orchestrate heterogeneous LLM agent teams from your CLI.

ExoMonad lets you spawn and manage teams of LLM agents (Claude + Gemini) directly from a single CLI session. Each agent operates in its own isolated git worktree, performing tasks, filing PRs, and notifying you when they finish—all integrated into your native Claude Code or Gemini CLI conversation.

## Key Features
- **Heterogeneous Agents**: Deploy Claude (Opus/3.5 Sonnet) for high-level architecture and Gemini (1.5 Flash/Pro) for fast, focused implementation.
- **Git Worktree Isolation**: Every agent gets its own directory and branch. No merge conflicts while work is in progress.
- **Native Teams Inbox Delivery**: Child agents notify the parent via Claude Code's Teams inbox. Completion messages arrive as native `<teammate-message>` events — structured, attributed, and delivered through the official Claude Code inbox mechanism. No polling, no stdin hacks, no context switching.
- **Haskell WASM DSL**: Agent logic and tool definitions are written in Haskell and compiled to WASM. Hot reload by editing a tool and running your next command.
- **Template System**: Reuse verified patterns and anti-patterns across worker specs to save up to 89% on orchestration tokens.

## Quick Demo

Spawn multiple Gemini workers to implement features in parallel from your Claude session:

```bash
# In your Claude session:
spawn_workers agents=[
  { name: "rust-impl", role: "dev", task: "Implement the Rust effect handlers" },
  { name: "haskell-sdk", role: "dev", task: "Add corresponding Haskell effect types" }
]
```

**What happens:**
1. Two new Zellij panes open immediately.
2. Each worker starts implementing its assigned task in isolation.
3. You continue working in your main tab.
4. When a worker finishes, it calls `notify_parent` — the message lands in your Claude conversation as a native teammate notification via the Teams inbox.

## Install

### For Users
Requires [Nix](https://nixos.org/) (for Haskell WASM compilation) and [Zellij](https://zellij.dev/).
```bash
git clone https://github.com/tidepool-heavy-industries/exomonad
cd exomonad
just install-all  # Builds WASM via Nix, compiles Rust, installs to ~/.cargo/bin/
```

### For Developers
Requires [Nix](https://nixos.org/) for the Haskell WASM build.
```bash
git clone https://github.com/tidepool-heavy-industries/exomonad
cd exomonad
just install-all-dev  # Builds WASM via Nix and installs the Rust binary to ~/.cargo/bin/
```

## Getting Started

1. **Initialize**: Run `exomonad init` in your project root. This starts the background server and sets up your environment.
2. **Register MCP** (one-time):
   ```bash
   claude mcp add --transport http exomonad http://localhost:7432/agents/tl/root/mcp
   ```
3. **Launch Agent**: Open the Tech Lead tab in your Zellij session and run `claude`.
4. **Orchestrate**: Delegate tasks using tools like `spawn_workers` or `spawn_subtree`.

## Architecture

ExoMonad uses a split architecture for safety and performance:
- **Haskell WASM**: Defines all tool logic, schemas, and agent decision trees. Agents are IO-blind state machines.
- **Rust Runtime**: Hosts the WASM plugin and executes all side effects (Git, GitHub API, Filesystem, Zellij).
- **Zellij**: Provides process isolation and multiplexing for agent tabs and panes.

See [CLAUDE.md](CLAUDE.md) for a deep dive into the system architecture.

## Available Tools

| Tool | Role | Description |
|------|------|-------------|
| `spawn_subtree` | tl | Fork a Claude agent into a new worktree and Zellij tab. |
| `spawn_leaf_subtree` | tl | Fork a Gemini agent into a new worktree and Zellij tab. |
| `spawn_workers` | tl | Spawn Gemini agents as Zellij panes in the current directory. |
| `file_pr` | tl, dev | Create or update a pull request for the current branch. |
| `merge_pr` | tl | Merge a child agent's PR and fetch the changes. |
| `notify_parent` | all | Signal completion to the parent agent via the Teams inbox. |

## License
ExoMonad is released under the [BSD 3-Clause License](LICENSE).