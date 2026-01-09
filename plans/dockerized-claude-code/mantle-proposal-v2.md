# Dockerized Claude Code - Mantle Proposal v2

## Design Principle

**Mantle is a session syscall layer with dev tooling.** It handles:
- Container lifecycle (spawn, wait, cleanup)
- Git worktree management (create from branch, cleanup)
- Session state persistence (`.mantle/sessions.json`)

Each command does one thing, returns structured JSON, exits. Orchestration (fork detection, parallel sessions, parent/child lifecycle) lives in the **calling layer** (Haskell).

```
┌─────────────────────────────────────────────────────────────┐
│              HASKELL (orchestrator)                          │
│                                                              │
│  - Spawns mantle processes (parallel or sequential)         │
│  - Parses JSON results                                       │
│  - Detects fork signals in interrupts                       │
│  - Manages parent/child session lifecycle                   │
└─────────────────────────────────────────────────────────────┘
                    │ spawn process, read stdout
        ┌───────────┼───────────┐
        ▼           ▼           ▼
   ┌─────────┐ ┌─────────┐ ┌─────────┐
   │ mantle  │ │ mantle  │ │ mantle  │
   │ session │ │ session │ │ session │
   │ start   │ │ continue│ │ fork    │
   └────┬────┘ └────┬────┘ └────┬────┘
        │           │           │
        │ creates   │ reuses    │ creates
        │ worktree  │ worktree  │ child worktree
        ▼           ▼           ▼
   ┌─────────┐ ┌─────────┐ ┌─────────┐
   │Container│ │Container│ │Container│
   │ claude  │ │ claude  │ │ claude  │
   │ -p "..."│ │--resume │ │--fork   │
   └─────────┘ └─────────┘ └─────────┘
```

---

## CLI Interface

### `mantle session start`

Start a new session on a branch. Creates worktree if needed.

```bash
mantle session start \
  --branch feat-x \
  --prompt "Implement feature X" \
  --model sonnet
```

**What it does:**
1. Creates worktree at `.mantle/worktrees/feat-x/` (if not exists)
2. Spawns container with worktree mounted
3. Runs `claude -p "..." --model sonnet`
4. Collects result, writes to `.mantle/sessions.json`
5. Returns JSON to stdout

**Output:**
```json
{
  "session_id": "abc-123-def",
  "branch": "feat-x",
  "worktree": "/repo/.mantle/worktrees/feat-x",
  "exit_code": 0,
  "is_error": false,
  "result_text": "I've implemented feature X...",
  "total_cost_usd": 0.05,
  "num_turns": 3,
  "interrupts": [],
  "duration_secs": 45.2
}
```

### `mantle session continue`

Continue an existing session.

```bash
mantle session continue abc-123-def \
  --prompt "Now add tests"
```

**What it does:**
1. Looks up session in `.mantle/sessions.json` to get branch/worktree
2. Spawns container with that worktree
3. Runs `claude --resume abc-123-def -p "Now add tests"`
4. Updates session state, returns JSON

### `mantle session fork`

Fork a session (child inherits parent's history).

```bash
mantle session fork abc-123-def \
  --child-branch feat-x-subtask \
  --child-prompt "SUBTASK: Handle edge case"
```

**What it does:**
1. Looks up parent session
2. Creates child worktree at `.mantle/worktrees/feat-x-subtask/`
3. Spawns container with child worktree
4. Runs `claude --resume abc-123-def --fork-session -p "SUBTASK: ..."`
5. Registers child session with parent reference
6. Returns JSON with **child's** session_id

### `mantle session info`

Get session metadata.

```bash
mantle session info abc-123-def
```

**Output:**
```json
{
  "session_id": "abc-123-def",
  "branch": "feat-x",
  "worktree": "/repo/.mantle/worktrees/feat-x",
  "parent_session": null,
  "child_sessions": ["def-456-ghi"],
  "status": "idle",
  "last_result": { ... },
  "created_at": "2025-01-09T10:30:00Z",
  "updated_at": "2025-01-09T11:45:00Z"
}
```

### `mantle session list`

List all sessions.

```bash
mantle session list
```

**Output:**
```json
{
  "sessions": [
    {
      "session_id": "abc-123-def",
      "branch": "feat-x",
      "status": "idle",
      "parent_session": null,
      "child_count": 1
    },
    {
      "session_id": "def-456-ghi",
      "branch": "feat-x-subtask",
      "status": "idle",
      "parent_session": "abc-123-def",
      "child_count": 0
    }
  ]
}
```

### `mantle session cleanup`

Clean up completed sessions and their worktrees.

```bash
# Clean specific session
mantle session cleanup abc-123-def

# Clean all completed sessions
mantle session cleanup --completed

# Dry run
mantle session cleanup --completed --dry-run
```

---

## Output Schema

```rust
/// JSON output from session start/continue/fork
#[derive(Serialize)]
pub struct SessionOutput {
    /// Claude Code session ID
    pub session_id: String,

    /// Git branch
    pub branch: String,

    /// Worktree path
    pub worktree: PathBuf,

    /// Container exit code
    pub exit_code: i32,

    /// Whether Claude reported an error
    pub is_error: bool,

    /// Final result text (or structured output)
    pub result_text: Option<String>,

    /// Total cost in USD
    pub total_cost_usd: f64,

    /// Number of turns in this run
    pub num_turns: u32,

    /// Signals collected (fork requests, escalations, etc.)
    pub interrupts: Vec<InterruptSignal>,

    /// Wall-clock duration
    pub duration_secs: f64,

    /// Error message if failed before Claude ran
    pub error: Option<String>,
}

/// Signal from Claude via `mantle signal`
#[derive(Serialize, Deserialize)]
pub struct InterruptSignal {
    pub signal_type: String,    // "fork", "escalate", "transition"
    pub state: Option<String>,  // e.g., child branch for fork
    pub reason: Option<String>, // e.g., child prompt for fork
}
```

### Fork Signal Convention

When Claude wants to fork, it calls via Bash tool:
```bash
mantle signal fork \
  --state "feat-x-subtask" \
  --reason "Handle edge case where input is empty"
```

This appears in `interrupts`:
```json
{
  "interrupts": [
    {
      "signal_type": "fork",
      "state": "feat-x-subtask",
      "reason": "Handle edge case where input is empty"
    }
  ]
}
```

The **Haskell caller** detects this and spawns `mantle session fork`.

---

## State Management

### `.mantle/sessions.json`

```json
{
  "sessions": {
    "abc-123-def": {
      "session_id": "abc-123-def",
      "branch": "feat-x",
      "worktree": "/repo/.mantle/worktrees/feat-x",
      "parent_session": null,
      "child_sessions": ["def-456-ghi"],
      "status": "idle",
      "created_at": "2025-01-09T10:30:00Z",
      "updated_at": "2025-01-09T11:45:00Z",
      "last_exit_code": 0,
      "total_cost_usd": 0.15
    }
  },
  "branch_to_session": {
    "feat-x": "abc-123-def",
    "feat-x-subtask": "def-456-ghi"
  }
}
```

### Status Values

```rust
pub enum SessionStatus {
    /// No container running, can be continued
    Idle,
    /// Container currently running (set at start, cleared on exit)
    Active,
    /// Session explicitly completed
    Completed,
    /// Session failed
    Failed,
}
```

### Concurrency

File locking for `.mantle/sessions.json`:
- Read lock for `info`, `list`
- Write lock for `start`, `continue`, `fork`, `cleanup`

Multiple parallel sessions work because each writes to different session entries.

---

## Directory Structure

```
/repo/
├── .mantle/
│   ├── sessions.json           # Session registry
│   └── worktrees/
│       ├── feat-x/             # Worktree for feat-x branch
│       ├── feat-x-subtask/     # Worktree for child branch
│       └── feat-y/             # Independent session
├── .git/
└── src/
```

Worktrees are created via `git worktree add`:
```bash
git worktree add .mantle/worktrees/feat-x -b feat-x main
```

---

## Implementation

### Module Structure

```
rust/mantle/src/session/
├── mod.rs           # CLI handling, command dispatch
├── types.rs         # SessionOutput, SessionInfo, SessionStatus
├── state.rs         # .mantle/sessions.json read/write with locking
├── docker.rs        # Container spawn/wait (bollard)
├── worktree.rs      # Git worktree create/remove
└── commands.rs      # start, continue, fork, info, list, cleanup
```

### `session/state.rs`

```rust
use std::fs::{File, OpenOptions};
use fs2::FileExt;  // For file locking

pub struct SessionState {
    path: PathBuf,
}

impl SessionState {
    pub fn new(repo_root: &Path) -> Self {
        Self {
            path: repo_root.join(".mantle/sessions.json"),
        }
    }

    pub fn read(&self) -> Result<SessionRegistry> {
        let file = File::open(&self.path)?;
        file.lock_shared()?;  // Read lock
        let registry: SessionRegistry = serde_json::from_reader(&file)?;
        file.unlock()?;
        Ok(registry)
    }

    pub fn write<F>(&self, f: F) -> Result<()>
    where
        F: FnOnce(&mut SessionRegistry) -> Result<()>,
    {
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&self.path)?;

        file.lock_exclusive()?;  // Write lock

        let mut registry: SessionRegistry = if file.metadata()?.len() > 0 {
            serde_json::from_reader(&file)?
        } else {
            SessionRegistry::default()
        };

        f(&mut registry)?;

        file.set_len(0)?;
        file.seek(SeekFrom::Start(0))?;
        serde_json::to_writer_pretty(&file, &registry)?;

        file.unlock()?;
        Ok(())
    }
}
```

### `session/docker.rs`

```rust
use bollard::Docker;

pub fn run_session_container(
    image: &str,
    worktree: &Path,
    claude_args: Vec<String>,
) -> Result<(i32, RunResult)> {
    tokio::runtime::Runtime::new()?.block_on(async {
        run_container_async(image, worktree, claude_args).await
    })
}

async fn run_container_async(
    image: &str,
    worktree: &Path,
    claude_args: Vec<String>,
) -> Result<(i32, RunResult)> {
    let docker = Docker::connect_with_local_defaults()?;

    // Create temp dir for result FIFO
    let result_dir = tempfile::tempdir()?;
    let result_fifo = result_dir.path().join("result.fifo");
    nix::unistd::mkfifo(&result_fifo, nix::sys::stat::Mode::S_IRWXU)?;

    // Build command: mantle wrap --no-tui --result-fifo /run/mantle/result.fifo -- claude <args>
    let mut cmd = vec![
        "wrap".to_string(),
        "--no-tui".to_string(),
        "--result-fifo".to_string(),
        "/run/mantle/result.fifo".to_string(),
        "--".to_string(),
    ];
    cmd.extend(claude_args);

    let config = Config {
        image: Some(image.to_string()),
        entrypoint: Some(vec!["mantle".to_string()]),
        cmd: Some(cmd),
        host_config: Some(HostConfig {
            binds: Some(vec![
                format!("{}:/workspace", worktree.display()),
                format!("{}:/root/.claude", dirs::home_dir().unwrap().join(".claude").display()),
                format!("{}:/run/mantle", result_dir.path().display()),
            ]),
            ..Default::default()
        }),
        working_dir: Some("/workspace".to_string()),
        ..Default::default()
    };

    let container = docker.create_container(None::<CreateContainerOptions<&str>>, config).await?;
    docker.start_container(&container.id, None::<StartContainerOptions<String>>).await?;

    // Read result from FIFO in blocking thread
    let fifo_path = result_fifo.clone();
    let result_handle = tokio::task::spawn_blocking(move || {
        let content = std::fs::read_to_string(&fifo_path)?;
        let run_result: RunResult = serde_json::from_str(&content)?;
        Ok::<_, MantleError>(run_result)
    });

    // Wait for container
    let status = docker.wait_container(&container.id, None::<WaitContainerOptions<String>>)
        .next().await
        .transpose()?
        .map(|r| r.status_code as i32)
        .unwrap_or(-1);

    let run_result = result_handle.await??;

    // Cleanup
    docker.remove_container(&container.id, None).await?;

    Ok((status, run_result))
}
```

### `session/worktree.rs`

```rust
use std::process::Command;

pub fn create_worktree(repo_root: &Path, branch: &str, from_branch: &str) -> Result<PathBuf> {
    let worktree_path = repo_root.join(".mantle/worktrees").join(branch);

    if worktree_path.exists() {
        // Already exists, just return it
        return Ok(worktree_path);
    }

    // Ensure parent dir exists
    std::fs::create_dir_all(worktree_path.parent().unwrap())?;

    // git worktree add <path> -b <branch> <from>
    let output = Command::new("git")
        .args([
            "worktree", "add",
            &worktree_path.to_string_lossy(),
            "-b", branch,
            from_branch,
        ])
        .current_dir(repo_root)
        .output()?;

    if !output.status.success() {
        // Branch might already exist, try without -b
        let output = Command::new("git")
            .args([
                "worktree", "add",
                &worktree_path.to_string_lossy(),
                branch,
            ])
            .current_dir(repo_root)
            .output()?;

        if !output.status.success() {
            return Err(MantleError::WorktreeCreate {
                branch: branch.to_string(),
                stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            });
        }
    }

    Ok(worktree_path)
}

pub fn remove_worktree(repo_root: &Path, branch: &str) -> Result<()> {
    let worktree_path = repo_root.join(".mantle/worktrees").join(branch);

    Command::new("git")
        .args(["worktree", "remove", &worktree_path.to_string_lossy()])
        .current_dir(repo_root)
        .output()?;

    Ok(())
}
```

### `session/commands.rs`

```rust
pub fn start_session(
    branch: &str,
    prompt: &str,
    model: &str,
    image: &str,
) -> Result<SessionOutput> {
    let repo_root = find_repo_root()?;
    let state = SessionState::new(&repo_root);
    let start_time = Instant::now();

    // Create worktree
    let parent_branch = get_current_branch(&repo_root)?;
    let worktree = create_worktree(&repo_root, branch, &parent_branch)?;

    // Run container
    let claude_args = vec![
        "-p".to_string(), prompt.to_string(),
        "--model".to_string(), model.to_string(),
    ];

    let (exit_code, run_result) = run_session_container(image, &worktree, claude_args)?;

    let session_id = run_result.session_id.clone().unwrap_or_default();

    // Update state
    state.write(|registry| {
        registry.sessions.insert(session_id.clone(), SessionEntry {
            session_id: session_id.clone(),
            branch: branch.to_string(),
            worktree: worktree.clone(),
            parent_session: None,
            child_sessions: vec![],
            status: SessionStatus::Idle,
            created_at: Utc::now(),
            updated_at: Utc::now(),
            last_exit_code: exit_code,
            total_cost_usd: run_result.total_cost_usd,
        });
        registry.branch_to_session.insert(branch.to_string(), session_id.clone());
        Ok(())
    })?;

    Ok(SessionOutput {
        session_id,
        branch: branch.to_string(),
        worktree,
        exit_code,
        is_error: run_result.is_error,
        result_text: run_result.result,
        total_cost_usd: run_result.total_cost_usd,
        num_turns: run_result.num_turns,
        interrupts: run_result.interrupts,
        duration_secs: start_time.elapsed().as_secs_f64(),
        error: None,
    })
}

pub fn continue_session(
    session_id: &str,
    prompt: &str,
    image: &str,
) -> Result<SessionOutput> {
    let repo_root = find_repo_root()?;
    let state = SessionState::new(&repo_root);
    let start_time = Instant::now();

    // Look up session
    let registry = state.read()?;
    let entry = registry.sessions.get(session_id)
        .ok_or_else(|| MantleError::SessionNotFound(session_id.to_string()))?;

    let worktree = entry.worktree.clone();
    let branch = entry.branch.clone();

    // Run container with --resume
    let claude_args = vec![
        "--resume".to_string(), session_id.to_string(),
        "-p".to_string(), prompt.to_string(),
    ];

    let (exit_code, run_result) = run_session_container(image, &worktree, claude_args)?;

    // Update state
    state.write(|registry| {
        if let Some(entry) = registry.sessions.get_mut(session_id) {
            entry.updated_at = Utc::now();
            entry.last_exit_code = exit_code;
            entry.total_cost_usd += run_result.total_cost_usd;
        }
        Ok(())
    })?;

    Ok(SessionOutput {
        session_id: session_id.to_string(),
        branch,
        worktree,
        exit_code,
        is_error: run_result.is_error,
        result_text: run_result.result,
        total_cost_usd: run_result.total_cost_usd,
        num_turns: run_result.num_turns,
        interrupts: run_result.interrupts,
        duration_secs: start_time.elapsed().as_secs_f64(),
        error: None,
    })
}

pub fn fork_session(
    parent_session_id: &str,
    child_branch: &str,
    child_prompt: &str,
    image: &str,
) -> Result<SessionOutput> {
    let repo_root = find_repo_root()?;
    let state = SessionState::new(&repo_root);
    let start_time = Instant::now();

    // Look up parent
    let registry = state.read()?;
    let parent = registry.sessions.get(parent_session_id)
        .ok_or_else(|| MantleError::SessionNotFound(parent_session_id.to_string()))?;

    // Create child worktree from parent's branch
    let child_worktree = create_worktree(&repo_root, child_branch, &parent.branch)?;

    // Run container with --resume --fork-session
    let claude_args = vec![
        "--resume".to_string(), parent_session_id.to_string(),
        "--fork-session".to_string(),
        "-p".to_string(), child_prompt.to_string(),
    ];

    let (exit_code, run_result) = run_session_container(image, &child_worktree, claude_args)?;

    let child_session_id = run_result.session_id.clone().unwrap_or_default();

    // Update state
    state.write(|registry| {
        // Add child session
        registry.sessions.insert(child_session_id.clone(), SessionEntry {
            session_id: child_session_id.clone(),
            branch: child_branch.to_string(),
            worktree: child_worktree.clone(),
            parent_session: Some(parent_session_id.to_string()),
            child_sessions: vec![],
            status: SessionStatus::Idle,
            created_at: Utc::now(),
            updated_at: Utc::now(),
            last_exit_code: exit_code,
            total_cost_usd: run_result.total_cost_usd,
        });

        // Update parent's children list
        if let Some(parent) = registry.sessions.get_mut(parent_session_id) {
            parent.child_sessions.push(child_session_id.clone());
        }

        registry.branch_to_session.insert(child_branch.to_string(), child_session_id.clone());
        Ok(())
    })?;

    Ok(SessionOutput {
        session_id: child_session_id,
        branch: child_branch.to_string(),
        worktree: child_worktree,
        exit_code,
        is_error: run_result.is_error,
        result_text: run_result.result,
        total_cost_usd: run_result.total_cost_usd,
        num_turns: run_result.num_turns,
        interrupts: run_result.interrupts,
        duration_secs: start_time.elapsed().as_secs_f64(),
        error: None,
    })
}
```

---

## Dockerfile

```dockerfile
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    curl \
    git \
    && rm -rf /var/lib/apt/lists/*

# Node.js 20.x
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && apt-get install -y nodejs

# Claude Code CLI
RUN npm install -g @anthropic-ai/claude-code

# Pre-built mantle binary
COPY mantle /usr/local/bin/mantle

RUN mkdir -p /run/mantle

WORKDIR /workspace
```

---

## Dependencies

```toml
# rust/mantle/Cargo.toml additions
[dependencies]
bollard = "0.16"
tokio = { version = "1", features = ["rt-multi-thread", "fs"] }
dirs = "5"
fs2 = "0.4"           # File locking
chrono = { version = "0.4", features = ["serde"] }
```

---

## What Mantle Does

| Responsibility | Mantle |
|----------------|--------|
| Container lifecycle | ✅ spawn, wait, cleanup |
| Worktree management | ✅ create, track, remove |
| Session state | ✅ `.mantle/sessions.json` |
| Result parsing | ✅ collect from FIFO, structure output |

## What Haskell Does

| Responsibility | Haskell |
|----------------|---------|
| Fork detection | Parse `interrupts` for fork signals |
| Parent notification | Call `continue` with summary message |
| Parallel execution | Spawn multiple mantle processes |
| Session orchestration | Decide when to fork, continue, complete |

---

## Testing

```bash
# Test 1: Start session
mantle session start --branch test-branch --prompt "Create test.txt"
# Verify: .mantle/worktrees/test-branch exists, sessions.json updated

# Test 2: Continue session
SESSION=$(mantle session start ... | jq -r .session_id)
mantle session continue $SESSION --prompt "What did you create?"
# Verify: Claude remembers context

# Test 3: Fork session
CHILD=$(mantle session fork $SESSION --child-branch test-child --child-prompt "Subtask")
# Verify: Child has different session_id, parent unchanged

# Test 4: Info/List
mantle session info $SESSION
mantle session list
```

---

## Open Questions

1. **`--fork-session` flag**: Does Claude Code CLI support this? Need to verify.

2. **Default model**: Should there be a config file for defaults?

3. **Image configuration**: Hardcode `tidepool-claude:latest` or make configurable?
