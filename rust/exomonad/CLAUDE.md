# exomonad

Unified sidecar binary: Rust host with Haskell WASM plugin.

## Architecture

**All logic is in Haskell WASM. Rust handles I/O only.**

Two WASM loading modes:
- **Embedded** (`include_bytes!`): WASM compiled into binary, used by `hook`
- **File-based** (`from_file`): Unified WASM loaded from `.exomonad/wasm/`, used by `serve`. Contains all roles, selected per-call. Enables hot reload.

```
# Hook Mode (embedded WASM)
Claude Code → exomonad hook → WASM handle_pre_tool_use → HookOutput

# HTTP mode (multi-agent, unified WASM)
N agents → exomonad serve → Unix socket (.exomonad/server.sock) → Unified WASM (handles all roles) → effects → I/O
```

## CLI Usage

```bash
exomonad hook pre-tool-use        # Handle Claude Code hook
exomonad serve [--socket PATH]    # Unix socket MCP server (multi-agent, hot reload)
exomonad recompile [--role ROLE]  # Build WASM from Haskell source
exomonad init [--session NAME]    # Initialize Zellij session (Server tab + TL tab)
```

### Init Command

`exomonad init` creates a two-tab Zellij session:
- **Server tab**: Runs `exomonad serve --port <port>` (stays open on exit)
- **TL tab**: Runs `nix develop` for the dev environment (focused by default)

MCP config is managed by the CLI tools (`claude mcp add` / `gemini mcp add`), not by `exomonad init`.

Use `--recreate` to delete an existing session and create fresh (e.g., after binary updates).

**Example `.exomonad/config.toml`:**
```toml
default_role = "tl"
project_dir = "."
```

**Config hierarchy:**
- `config.toml` uses `default_role` (project-wide default)
- `config.local.toml` uses `role` (worktree-specific override)

To update WASM: rebuild the Rust binary with `just install-all-dev`. The `build.rs` copies WASM files into the binary via `include_bytes!`, so WASM and binary are always in sync.

## MCP Server

The MCP server provides tools via HTTP (rmcp).

### Configuration

Use CLI-native config commands:
```bash
# Claude Code
claude mcp add --transport http exomonad http://localhost:7432/tl/mcp

# Gemini CLI (HTTP mode only)
gemini mcp add --transport http exomonad http://localhost:7432/tl/mcp
```

### Available Tools

| Tool | Description | Arguments |
|------|-------------|-----------|
| `git_branch` | Get current git branch | `path?`: directory |
| `git_status` | Get dirty files (git status --porcelain) | `path?`: directory |
| `git_log` | Get recent commits | `path?`: directory, `limit?`: count |
| `read_file` | Read file contents | `path`: file path, `max_lines?`: limit |
| `github_list_issues` | List GitHub issues | `owner`, `repo`, `state?`, `labels?` |
| `github_get_issue` | Get single issue details | `owner`, `repo`, `number` |
| `github_list_prs` | List GitHub pull requests | `owner`, `repo`, `state?`, `limit?` |
| `spawn_agents` | Spawn agents in Zellij tabs | `issues[]`, `owner`, `repo`, `worktree_dir?` |
| `spawn_gemini_teammate` | Spawn named Gemini teammate | `name`, `prompt`, `agent_type?`, `subrepo?` |
| `cleanup_agents` | Clean up agent worktrees | `issues[]`, `force?` |
| `list_agents` | List active agent worktrees | (none) |
| `get_agent_messages` | Read notes/questions from agents | `agent_id?` |
| `answer_question` | Answer pending agent question | `agent_id`, `question_id`, `answer` |

### Spawn Tools Requirements

The spawn tools (`spawn_agents`, `spawn_gemini_teammate`, `cleanup_agents`, `list_agents`) require:
- **Zellij session**: Must be running inside a Zellij terminal multiplexer
- **GITHUB_TOKEN**: Environment variable for GitHub API access
- **Git repository**: The project directory must be a git repository
- **Gemini MCP config**: Gemini agents get `.gemini/settings.json` (not `.mcp.json`) pointing to the ExoMonad server


## Environment Variables

| Variable | Required | Purpose |
|----------|----------|---------|
| `RUST_LOG` | No | Tracing log level |

## Effect Boundary (WASM)

All effects flow through a single `yield_effect` host function using protobuf binary encoding. Haskell calls `runEffect @EffectType request`, which encodes an `EffectEnvelope` and dispatches to the appropriate handler in the `EffectRegistry` by namespace prefix.

### Registered Handlers

| Namespace | Handler | Implementation |
|-----------|---------|----------------|
| `git.*` | GitHandler | git subprocess |
| `github.*` | GitHubHandler | HTTP API |
| `agent.*` | AgentHandler | GitHub API + git worktree + Zellij |
| `fs.*` | FsHandler | tokio::fs |
| `log.*` | LogHandler | tracing |
| `popup.*` | PopupHandler | Zellij plugin IPC |
| `file_pr.*` | FilePRHandler | gh CLI |
| `copilot.*` | CopilotHandler | GitHub API polling |
| `messaging.*` | MessagingHandler | Agent messaging files (JSON) |

All handlers are registered by `exomonad_core::register_builtin_handlers()`.

## Building

WASM must be built before Rust, since the Rust binary embeds WASM at compile time:

```bash
# Full install (recommended): builds WASM, then Rust, installs everything
just install-all-dev

# Or manually:
just wasm-all                    # Build WASM plugins first
cargo build -p exomonad          # Then build Rust (embeds WASM via build.rs)
```

## Testing

Integration tests use a shared server pattern (OnceLock singleton) to reduce startup overhead.

```bash
# Unit tests
cargo test -p exomonad

# E2E hook test (binary has embedded WASM)
echo '{"session_id":"test","hook_event_name":"PreToolUse","tool_name":"Write","transcript_path":"/tmp/t.jsonl","cwd":"/","permission_mode":"default"}' | \
  ./target/debug/exomonad hook pre-tool-use
```

## Data Flow

### Hook Flow
```
Claude Code hook JSON (stdin)
         ↓
    exomonad hook pre-tool-use
         ↓
    Use embedded WASM (role from config)
         ↓
    Parse HookInput
         ↓
    Call WASM handle_pre_tool_use
         ↓
    Haskell yields effects (GitGetBranch, LogInfo, etc.)
         ↓
    Rust executes effects via host functions
         ↓
    Haskell returns HookOutput
         ↓
    Serialize and print to stdout
         ↓
    Claude Code receives response
```

## Related Documentation

- **[exomonad-core](../exomonad-core/)** - Framework, handlers, services, protocol types, UI protocol
- **[wasm-guest](../../haskell/wasm-guest/)** - Haskell WASM plugin source
