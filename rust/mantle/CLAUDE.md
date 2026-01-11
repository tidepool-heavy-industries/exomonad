# mantle

Host-side CLI for spawning and managing Claude Code sessions. Creates isolated git worktrees, runs Claude Code in Docker containers, and returns structured JSON results.

## Usage

```bash
# Start a new session
mantle session start --slug implement/auth --prompt "Add user authentication"

# Continue an existing session
mantle session continue <session-id> --prompt "Now add tests"

# Fork a session (branch from parent's state)
mantle session fork <parent-id> --child-slug refactor --child-prompt "Refactor auth"

# Query sessions
mantle session list --state running
mantle session info <session-id>

# Cleanup terminal sessions
mantle session cleanup --completed
```

## Architecture

```
                                       ┌─────────────────────────────┐
                                       │ mantle-hub (daemon)         │
                                       │ • SQLite persistence        │
                                       │ • WebSocket streaming       │
                                       └──────────────▲──────────────┘
                                                      │ HTTP/WS
                                                      │
┌───────────────┐     ┌───────────────────────────────┴──────────────────────┐
│ Haskell       │     │                       mantle                          │
│ orchestrator  │────▶│  session/                                             │
│               │     │  ├─ start.rs      Create session + worktree + run     │
└───────────────┘     │  ├─ continue_.rs  Resume session with new prompt      │
       stdout JSON    │  ├─ fork.rs       Create child from parent context    │
                      │  ├─ list.rs       Query sessions by state             │
                      │  ├─ info.rs       Get session metadata                │
                      │  ├─ cleanup.rs    Remove terminal sessions            │
                      │  ├─ state.rs      File-locked session store           │
                      │  ├─ types.rs      SessionMetadata, SessionOutput      │
                      │  └─ worktree.rs   Git worktree management             │
                      │                                                        │
                      │  docker/                                               │
                      │  ├─ container.rs  Docker run with TTY                 │
                      │  └─ control_listener.rs  Hook event forwarding        │
                      └────────────────────────────────────────────────────────┘
                                                      │
                                                      │ docker run
                                                      ▼
                      ┌────────────────────────────────────────────────────────┐
                      │                Docker Container                         │
                      │  • Claude Code (npm package)                           │
                      │  • mantle-agent (hook handler + MCP server)            │
                      └────────────────────────────────────────────────────────┘
```

## Module Reference

### session/

| Module | Purpose |
|--------|---------|
| `start.rs` | `start_session()` - create worktree, register with hub, run Claude |
| `continue_.rs` | `continue_session()` - resume existing session |
| `fork.rs` | `fork_session()` - create child from parent context |
| `list.rs` | `list_sessions()` - query by state/parent |
| `info.rs` | `session_info()` - get metadata for one session |
| `cleanup.rs` | `cleanup_sessions()` - remove terminal sessions and worktrees |
| `state.rs` | `StateManager` - file-locked persistent store |
| `types.rs` | `SessionMetadata`, `SessionOutput`, `SessionState` |
| `worktree.rs` | `WorktreeManager` - git worktree creation/deletion |

### docker/

| Module | Purpose |
|--------|---------|
| `container.rs` | Run Claude Code via `docker run -t` with TTY |
| `control_listener.rs` | Forward hook events to control socket |

### Other

| Module | Purpose |
|--------|---------|
| `config.rs` | Load `~/.config/mantle/config.toml` |
| `stream_parser.rs` | Parse Claude Code `--stream-json` output |

## Key Types

### Session Lifecycle
```rust
pub enum SessionState {
    Pending,    // Created, not yet run
    Running,    // Currently executing
    Completed,  // Exit code 0
    Failed,     // Non-zero exit
    Cancelled,  // Manually stopped
}

pub struct SessionMetadata {
    pub id: String,           // UUID
    pub slug: String,         // User-provided slug
    pub branch: String,       // Generated: {slug}-{hex}
    pub worktree: PathBuf,    // .mantle/worktrees/{slug}-{hex}
    pub model: String,        // haiku, sonnet, opus
    pub state: SessionState,
    pub parent_id: Option<String>,
    pub cc_session_id: Option<String>,  // Claude Code session ID
    pub total_cost_usd: f64,
    pub total_turns: i64,
    // ...
}
```

### Session Output (→ Haskell)
```rust
pub struct SessionOutput {
    pub session_id: String,
    pub branch: String,
    pub worktree: PathBuf,
    pub exit_code: i32,
    pub is_error: bool,
    pub result_text: Option<String>,
    pub structured_output: Option<Value>,  // --json-schema result
    pub total_cost_usd: f64,
    pub num_turns: i64,
    pub interrupts: Vec<InterruptSignal>,
    pub tool_calls: Option<Vec<ToolCall>>,
    pub model_usage: HashMap<String, ModelUsage>,
    pub cc_session_id: Option<String>,
    pub duration_secs: f64,
    pub error: Option<String>,
}
```

### Configuration
```rust
pub struct StartConfig {
    pub slug: String,
    pub prompt: String,
    pub model: String,           // Default: "sonnet"
    pub timeout_secs: u64,       // Default: 300
    pub base_branch: Option<String>,
    pub json_schema: Option<String>,
    pub decision_tools: Option<String>,
}

pub struct ContinueConfig {
    pub session_id: String,
    pub prompt: String,
    pub timeout_secs: u64,
    pub docker: bool,
    pub decision_tools: Option<String>,
}

pub struct ForkConfig {
    pub parent_id: String,
    pub child_slug: String,
    pub child_prompt: String,
    pub timeout_secs: u64,
    pub docker: bool,
    pub decision_tools: Option<String>,
}
```

## Data Storage

### `.mantle/sessions.json`
File-locked JSON store for session metadata:
```json
{
  "sessions": {
    "uuid-1": { "id": "uuid-1", "slug": "fix/auth", "state": "completed", ... },
    "uuid-2": { "id": "uuid-2", "slug": "add/tests", "state": "running", ... }
  }
}
```

Uses `fs2` file locking for concurrent access safety.

### `.mantle/worktrees/{slug}-{hex}/`
Git worktrees for session isolation. Each session gets its own worktree branched from the base branch.

### `~/.config/mantle/config.toml`
```toml
[docker]
auth_volume = "tidepool-claude-auth"  # Docker volume with ~/.claude
image = "mantle-agent:latest"
cabal_store_volume = "tidepool-cabal-store"  # Optional shared cabal
```

## Stream Parsing

The `stream_parser` module handles Claude Code's `--stream-json` output:

```rust
pub fn parse_stream(lines: impl Iterator<Item = String>) -> Result<RunResult>;
```

Handles:
- Control character stripping (TTY output can contain ANSI codes)
- Event accumulation (System → Assistant/User → Result)
- Error recovery (partial output on timeout/crash)
- Interrupt signal extraction from `mantle signal` tool calls

## Environment Variables

| Variable | Purpose |
|----------|---------|
| `MANTLE_HUB_URL` | Hub HTTP endpoint (default: http://localhost:7433) |
| `RUST_LOG` | Tracing log level |

## Testing

```bash
cargo test -p mantle

# Key test files:
# - src/main.rs (inline tests for stream parsing)
# - tests/control_socket_integration.rs
# - tests/fifo_integration.rs
# - tests/supervisor_integration.rs
```

## Integration with Haskell

Haskell's `tidepool-claude-code-executor` spawns mantle as a subprocess:

1. **Spawn**: `mantle session start --slug ... --prompt ... --json-schema ...`
2. **Stdout**: JSON `SessionOutput` captured by Haskell
3. **Exit code**: 0 = success, non-zero = failure
4. **Hooks**: Control socket for real-time tool allow/deny decisions

The output JSON is designed to match Haskell's `deriving-aeson` expectations (snake_case fields).

## Sanitization

All string output is sanitized to remove control characters that would break JSON parsing on the Haskell side. This handles:
- ANSI escape sequences from TTY output
- Bell characters
- Other non-printable control codes

The `sanitize_string` and `sanitize_json_value` functions in `types.rs` handle this recursively for structured output.
