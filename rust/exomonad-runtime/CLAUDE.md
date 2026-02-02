# exomonad-runtime

WASM plugin hosting and host functions. All MCP tools and hook logic routes through this runtime.

## Architecture

```
Sidecar (exomonad-sidecar)
    ↓
PluginManager::load_plugin(wasm_path, services)
    ↓
Extism Plugin (Haskell WASM)
    ↓ calls host functions
Services (Git, GitHub, AgentControl, FileSystem, Log)
    ↓ executes I/O
Docker/Local Executor → git/gh/zellij/file operations
    ↓
Results marshalled back through WASM → Haskell
```

## Key Modules

| Module | File | Purpose |
|--------|------|---------|
| `plugin_manager` | plugin_manager.rs | Extism plugin loading, host function registration |
| `services` | services/mod.rs | Service struct (Git, GitHub, AgentControl, FileSystem, Log) |
| `host_functions` | host_functions.rs | WASM import helpers (get_input, set_output, HostResult protocol) |

### Services Breakdown

| Service | File | Purpose |
|---------|------|---------|
| GitService | services/git.rs | Git operations (branch, status, commits, worktree) |
| GitHubService | services/github.rs | GitHub API (issues, PRs, create PR, list PRs) |
| AgentControlService | services/agent_control.rs | High-level agent lifecycle (spawn, cleanup, list) |
| FileSystemService | services/filesystem.rs | File I/O (read, write) |
| LogService | services/log.rs | Structured logging via tracing |
| DockerService | services/docker.rs | Docker executor (container exec) |
| LocalExecutor | services/local.rs | Local executor (direct subprocess) |
| Secrets | services/secrets.rs | Load secrets from ~/.exomonad/secrets |

## Services

### GitService

Methods:
- `get_branch(container, dir)`: Current git branch
- `get_worktree(container, dir)`: Worktree info (path, branch)
- `get_dirty_files(container, dir)`: Files with uncommitted changes
- `get_recent_commits(container, dir, n)`: Recent commit log
- `has_unpushed_commits(container, dir)`: Check for unpushed commits
- `get_remote_url(container, dir)`: Git remote URL
- `get_repo_info(container, dir)`: Branch + owner/name from remote

**Implementation:** Calls `git` CLI via DockerExecutor (either DockerService or LocalExecutor).

### GitHubService

Methods:
- `list_issues(owner, repo, state, labels)`: List repository issues
- `get_issue(owner, repo, number, include_comments)`: Get single issue with comments
- `create_pr(owner, repo, options)`: File a pull request
- `list_prs(owner, repo, state, limit)`: List pull requests

**Implementation:** Uses GitHub REST API via reqwest (HTTP client).

**Authentication:** Requires GITHUB_TOKEN from Secrets or environment variable.

### AgentControlService (High-Level Semantic Effects)

Methods:
- `spawn_agent(issue, owner, repo, worktree_dir)`: GitHub API + git worktree + Zellij KDL layout (single operation)
- `spawn_agents(issues)`: Batch spawn
- `cleanup_agent(issue)`: Zellij close + git worktree remove
- `cleanup_agents(issues)`: Batch cleanup
- `list_agents()`: Active agent worktrees

**Design:** High-level effects (not low-level primitives). WASM calls SpawnAgent, not CreateWorktree + OpenTab.

**Spawn flow:**
1. Fetch issue from GitHub API
2. Create git worktree: `worktrees/gh-{issue}-{title}-{agent}/`
3. Create branch: `gh-{issue}/{title}-{agent}`
4. Write `.exomonad/config.toml` (role="dev") and `.mcp.json`
5. Build initial prompt with full issue context
6. Generate KDL layout with agent-specific command:
   - Claude: `claude --prompt '...'`
   - Gemini: `gemini --prompt-interactive '...'`
7. Create Zellij tab: `zellij action new-tab --layout /tmp/exomonad-layouts/<name>.kdl`
8. Tab auto-closes when agent exits (`close_on_exit true`)

**Cleanup flow:**
1. Close Zellij tab: `zellij action close-tab`
2. Remove git worktree: `git worktree remove`

### FileSystemService

Methods:
- `read_file(path, max_lines)`: Read file contents (async)
- `write_file(path, contents)`: Write file contents (async)

**Implementation:** tokio::fs for async file I/O.

### LogService

Methods:
- `log_info(message)`: Info-level log
- `log_error(message)`: Error-level log
- `emit_event(event)`: Event bus (for UI updates)

**Implementation:** Uses tracing crate for structured logging.

## Host Functions Registration

PluginManager::load_plugin registers 25+ host functions from all services:

**Git (7 functions):**
- `git_get_branch`
- `git_get_worktree`
- `git_get_dirty_files`
- `git_get_recent_commits`
- `git_has_unpushed_commits`
- `git_get_remote_url`
- `git_get_repo_info`

**GitHub (6 functions):**
- `github_list_issues`
- `github_get_issue`
- `github_create_pr`
- `github_list_prs`
- `github_get_pr`
- `github_list_pr_files`

**Agent Control (5 functions):**
- `agent_spawn`
- `agent_spawn_batch`
- `agent_cleanup`
- `agent_cleanup_batch`
- `agent_list`

**Filesystem (2 functions):**
- `fs_read_file`
- `fs_write_file`

**Log (3 functions):**
- `log_info`
- `log_error`
- `emit_event`

**File PR (1 function):**
- `file_pr` - Create/update PRs via gh CLI

**Copilot Review (1 function):**
- `copilot_poll_review` - Poll for Copilot review comments

## Effect Boundary

| Haskell Effect | Host Function | Rust Implementation |
|----------------|---------------|---------------------|
| GitGetBranch path | git_get_branch | GitService::get_branch → git subprocess |
| SpawnAgent {issue, owner, repo} | agent_spawn | AgentControlService::spawn_agent → GitHub API + git worktree + Zellij KDL |
| ReadFile path | fs_read_file | FileSystemService::read_file → tokio::fs |
| GitHubListIssues {owner, repo} | github_list_issues | GitHubService::list_issues → GitHub API |

**Pattern:** Haskell yields semantic effects, Rust implements all I/O.

## HostResult Protocol

All host functions use HostResult envelope for success/error discrimination:

```rust
#[derive(Serialize)]
#[serde(tag = "kind")]
enum HostResult<T> {
    #[serde(rename = "success")]
    Success { value: T },
    #[serde(rename = "error")]
    Error(HostError),
}

#[derive(Serialize)]
struct HostError {
    message: String,
    code: String,  // Error classification (not_found, not_authenticated, etc.)
}
```

**JSON examples:**

Success:
```json
{
  "kind": "success",
  "value": {"branch": "main", "owner": "anthropics", "name": "exomonad"}
}
```

Error:
```json
{
  "kind": "error",
  "message": "gh auth login required",
  "code": "not_authenticated"
}
```

**Critical:** Batch operations (spawn_agents, cleanup_agents) return raw results (no `?`), singular operations return Result (use `?`).

**Error codes:**
- `not_found` - Resource not found (issue, PR, file)
- `not_authenticated` - GitHub token missing or invalid
- `not_git_repo` - Not in a git repository
- `git_error` - Git command failed
- `io_error` - File I/O error
- `internal_error` - Unexpected error

## Docker vs Local Execution

The runtime supports two execution modes:

| Mode | Constructor | Use Case |
|------|------------|----------|
| Docker | `Services::new()` | Containerized environments |
| Local | `Services::new_local()` | Local development (current default) |

**DockerExecutor trait:**
```rust
#[async_trait]
pub trait DockerExecutor: Send + Sync {
    async fn exec(&self, container: &str, dir: &str, cmd: &[&str]) -> Result<String>;
}
```

**Implementations:**
- `DockerService`: `docker exec <container> sh -c "cd <dir> && <cmd>"`
- `LocalExecutor`: `subprocess::run(<cmd>)` directly (no Docker)

**Note:** Docker functions are NOT exposed to WASM. They are internal implementation details used by GitService and others. Haskell calls high-level effects (GitGetBranch), Rust handles Docker vs Local internally.

## Building

```bash
cargo build -p exomonad-runtime
cargo test -p exomonad-runtime
```

## Testing

```bash
cargo test -p exomonad-runtime
# Tests verify:
# - Host function registration
# - HostResult serialization
# - Service integration (Git, GitHub, AgentControl)
```

## Design Decisions

| Decision | Rationale |
|----------|-----------|
| Extism runtime | Mature WASM runtime with stable host function API, used in production |
| High-level effects | SpawnAgent (not CreateWorktree+OpenTab) reduces WASM surface, encapsulates complexity |
| Services struct | Centralized service lifecycle, easy dependency injection, clean separation |
| LocalExecutor vs DockerExecutor | Support both local dev and containerized environments |
| HostResult envelope | Explicit success/error discrimination, no exceptions across WASM boundary |
| Error codes | Structured error classification for client-side handling |
| Optional GitHub | Graceful degradation when GITHUB_TOKEN is not available |
| Secrets from file | ~/.exomonad/secrets for local development, env vars for CI/containers |

## Related Documentation

- [exomonad-sidecar](../exomonad-sidecar/CLAUDE.md) - MCP server that uses this runtime
- [wasm-guest](../../haskell/wasm-guest/) - Haskell plugin source
- [exomonad-services](../exomonad-services/CLAUDE.md) - External service clients (Anthropic, Ollama, OTLP)
- [Root CLAUDE.md](../../CLAUDE.md) - Project overview
