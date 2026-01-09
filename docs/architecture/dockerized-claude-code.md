# Dockerized Claude Code Architecture

## Problem Statement

Git worktrees change directory names (`tidepool-worktree-1/`, `tidepool-worktree-2/`), which breaks Claude Code history threading. The AI sees each worktree as a completely different project, losing conversational context.

## Solution

Dockerize Claude Code sessions. Each container sees `/workspace/tidepool` regardless of which "fork" it is, enabling:

1. **Consistent paths** → proper history threading via `--continue`
2. **True isolation** → no permission prompts needed (container IS the sandbox)
3. **Copy-on-write efficiency** → `docker commit` captures state including `~/.claude/`

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                         HOST MACHINE                             │
├─────────────────────────────────────────────────────────────────┤
│  Shared Resources:                                               │
│  ├── /repo/tidepool.git/        (bare repo)                     │
│  ├── /worktrees/                (managed by host)               │
│  │   ├── main/                                                  │
│  │   ├── feat-x/                                                │
│  │   └── feat-x-sub/                                            │
│  └── ~/.cache/cabal/            (build cache)                   │
│                                                                  │
│  Auth (extracted from macOS Keychain on host):                  │
│  └── CLAUDE_CODE_OAUTH_TOKEN    (passed as env var)             │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────────┐│
│  │              MANTLE (host mode - orchestrator)               ││
│  │                                                              ││
│  │  New subcommand: `mantle container`                          ││
│  │  • spawn: Create container with worktree mount               ││
│  │  • pause: docker pause (freeze processes)                    ││
│  │  • resume: docker unpause + inject message                   ││
│  │  • fork: docker commit + spawn from image                    ││
│  │  • status: Query container state                             ││
│  │  • terminate: docker rm -f                                   ││
│  │                                                              ││
│  │  State tracking:                                             ││
│  │  • Container tree (parent → children)                        ││
│  │  • Branch ↔ container mapping                                ││
│  │  • Session IDs for --continue                                ││
│  └─────────────────────────────────────────────────────────────┘│
│           │                                                      │
│           ▼                                                      │
│  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐                │
│  │ Container A │ │ Container B │ │ Container C │                │
│  │ (active)    │ │ (paused)    │ │ (active)    │                │
│  │             │ │             │ │             │                │
│  │ /workspace ─┼─┼─► worktree  │ │ forked from │                │
│  │             │ │   main      │ │ B           │                │
│  │ mantle wrap │ │             │ │             │                │
│  │ claude -p   │ │             │ │ mantle wrap │                │
│  │             │ │             │ │ claude      │                │
│  │             │ │             │ │ --continue  │                │
│  └─────────────┘ └─────────────┘ └─────────────┘                │
└─────────────────────────────────────────────────────────────────┘
```

## Component Design

### 1. Base Docker Image (`tidepool-dev`)

Extends CI image with Claude Code and mantle:

```dockerfile
FROM tidepool-ci:latest

# Install Claude Code CLI
RUN npm install -g @anthropic/claude-code

# Install Rust toolchain (for mantle)
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# Build and install mantle
COPY rust/mantle /build/mantle
RUN cd /build/mantle && cargo build --release \
    && cp target/release/mantle /usr/local/bin/

# Working directory (will be mounted)
WORKDIR /workspace

# Entry point runs mantle wrap
ENTRYPOINT ["mantle", "wrap", "--no-tui"]
```

### 2. Volume Mount Strategy

```yaml
volumes:
  # Shared (efficiency, not coordination)
  - /repo/tidepool.git:/repo.git:ro           # Bare repo
  - ~/.cache/cabal:/root/.cache/cabal         # Build cache

  # Per-container (isolated)
  - /worktrees/${BRANCH}:/workspace           # Worktree

  # NOT shared - travels with docker commit
  # ~/.claude/ lives inside container
```

### 3. Mantle Container Orchestrator (Rust)

New subcommand in mantle: `mantle container <action>`

```rust
// rust/mantle/src/container.rs

pub struct ContainerOrchestrator {
    /// Active containers by ID
    containers: HashMap<String, ContainerState>,
    /// Parent → children relationships
    tree: HashMap<String, Vec<String>>,
    /// Branch → container mapping
    branches: HashMap<String, String>,
}

pub enum ContainerState {
    Active {
        container_id: String,
        session_id: String,  // CC session for --continue
        branch: String,
    },
    Paused {
        container_id: String,
        session_id: String,
        branch: String,
    },
}

// Operations
impl ContainerOrchestrator {
    /// Spawn new container with fresh worktree
    pub fn spawn(&mut self, branch: &str, prompt: &str) -> Result<String>;

    /// Pause container (docker pause)
    pub fn pause(&mut self, container_id: &str) -> Result<()>;

    /// Resume container with injected message
    pub fn resume(&mut self, container_id: &str, message: &str) -> Result<()>;

    /// Fork: commit current state, spawn child
    pub fn fork(&mut self, parent_id: &str, child_branch: &str, task: &str) -> Result<String>;

    /// Get container status
    pub fn status(&self, container_id: &str) -> Option<&ContainerState>;

    /// Terminate container
    pub fn terminate(&mut self, container_id: &str) -> Result<()>;
}
```

CLI interface:

```bash
# Spawn new container
mantle container spawn --branch main --prompt "Implement feature X"

# Pause container
mantle container pause <container-id>

# Fork from paused container
mantle container fork <parent-id> --branch feat-x-sub --task "Handle edge case"

# Resume with message
mantle container resume <container-id> --message "Child completed, review changes"

# Check status
mantle container status <container-id>

# Terminate
mantle container terminate <container-id>
```

### 4. The Fork Operation (Detailed)

```
1. Parent container doing work
   └── mantle wrap supervising claude -p

2. Type-driven-dev harness decides to fork
   └── Calls: mantle container fork parent-123 --branch feat-sub --task "..."

3. Fork operation:
   a. docker pause parent-123
   b. docker commit parent-123 → tidepool-dev:parent-123-checkpoint
   c. git worktree add /worktrees/feat-sub origin/parent-branch
   d. docker run \
        -e CLAUDE_CODE_OAUTH_TOKEN="..." \
        -v /worktrees/feat-sub:/workspace \
        -v ~/.cache/cabal:/root/.cache/cabal \
        tidepool-dev:parent-123-checkpoint \
        --result-fifo /tmp/fifo-child \
        -- --continue --model sonnet -p "TASK: ..."
   e. Return child container ID

4. Child works, completes
   └── Merges to parent branch, signals completion

5. Wake parent:
   a. docker unpause parent-123
   b. Inject message: "Child feat-sub completed. Changes merged. Review:"
   c. Parent's claude --continue picks up
```

### 5. Haskell Integration

Two options for Haskell side:

#### Option A: Extend ClaudeCodeConfig

```haskell
-- tidepool-native-gui/claude-code-executor/src/Tidepool/ClaudeCode/Config.hs

data ExecutionMode
  = DirectMode          -- Current: shell out to mantle run
  | ContainerMode       -- New: use container orchestrator
      { containerImage :: Text
      , parentContainer :: Maybe Text  -- For forks
      }
  deriving (Show, Eq)

data ClaudeCodeConfig = ClaudeCodeConfig
  { ccExecutionMode :: ExecutionMode
  , ccZellijSession :: Text      -- Only for DirectMode
  , ccDefaultTimeout :: Int
  , ccTempDir :: FilePath
  , ccMantlePath :: FilePath
  }
```

#### Option B: New Effect Type

```haskell
-- tidepool-core/src/Tidepool/Effects/Container.hs

data Container r where
  -- Spawn new container
  SpawnContainer :: Branch -> Prompt -> Container ContainerId

  -- Pause container
  PauseContainer :: ContainerId -> Container ()

  -- Resume with message
  ResumeContainer :: ContainerId -> Text -> Container ()

  -- Fork from parent
  ForkContainer :: ContainerId -> Branch -> Task -> Container ContainerId

  -- Get status
  ContainerStatus :: ContainerId -> Container (Maybe ContainerState)

  -- Terminate
  TerminateContainer :: ContainerId -> Container ()

-- Executor in tidepool-native-gui/container-executor/
runContainer :: Container a -> IO a
runContainer = \case
  SpawnContainer branch prompt ->
    -- Shell out to: mantle container spawn ...
  ForkContainer parent branch task ->
    -- Shell out to: mantle container fork ...
  ...
```

**Recommendation**: Option B (new effect type) is cleaner. It separates container orchestration from Claude Code execution, making it composable in graphs.

### 6. Type-Driven-Dev Harness Integration

The harness (in consuming repo, e.g., anemone) would use the new effects:

```haskell
-- In consuming repo's type-driven-dev implementation

typeDrivenDev :: Task -> Eff '[Container, ClaudeCodeExec, ...] Result
typeDrivenDev task = do
  -- Spawn main container
  mainId <- send $ SpawnContainer "main" (taskPrompt task)

  -- Wait for completion or fork request
  result <- waitForResult mainId

  case result of
    NeedsFork subtask -> do
      -- Pause parent
      send $ PauseContainer mainId

      -- Fork for subtask
      childId <- send $ ForkContainer mainId (subtask.branch) (subtask.prompt)

      -- Wait for child
      childResult <- waitForResult childId

      -- Child merges to parent branch
      send $ TerminateContainer childId

      -- Wake parent with context
      send $ ResumeContainer mainId $
        "Subtask completed. Changes merged. Review:\n" <> summarize childResult

      -- Continue parent
      waitForResult mainId

    Completed output ->
      pure output
```

## Implementation Plan

### Phase 1: Docker Infrastructure
- [ ] Create `Dockerfile.dev` based on CI image + Claude Code + mantle
- [ ] Test basic lifecycle: run, pause, commit, resume
- [ ] Verify `CLAUDE_CODE_OAUTH_TOKEN` works in container
- [ ] Set up worktree mount strategy

### Phase 2: Mantle Container Orchestrator
- [ ] Add `container` subcommand to mantle
- [ ] Implement spawn operation
- [ ] Implement pause/resume operations
- [ ] Implement fork operation (commit + spawn)
- [ ] State tracking (container tree, branch mapping)
- [ ] Persistent state (survive mantle restart)

### Phase 3: Haskell Integration
- [ ] Create `Container` effect type in tidepool-core
- [ ] Create `container-executor` package
- [ ] Wire into native server EffectRunner
- [ ] Integration tests

### Phase 4: Type-Driven-Dev Integration
- [ ] Update harness to use Container effects
- [ ] Implement fork/merge/wake cycle
- [ ] Test with real subtask decomposition

### Phase 5: History Threading Validation
- [ ] Verify `--continue` works after fork
- [ ] Test conversation continuity across fork/wake
- [ ] Benchmark vs worktree approach

## Open Questions

1. **Mantle state persistence**: Where does orchestrator state live?
   - Option: JSON file in `~/.mantle/containers.json`
   - Option: SQLite database
   - Option: In-memory only (containers are ephemeral anyway)

2. **Control socket in containers**: How does Haskell hook server connect?
   - Option: Mount socket as volume
   - Option: Network socket with port mapping
   - Option: Each container runs its own hook handler

3. **Resource limits**: Should containers have CPU/memory limits?
   - Probably yes for safety, but start permissive

4. **Cleanup policy**: When are committed images garbage collected?
   - After child terminates?
   - After configurable TTL?
   - Manual cleanup?

## Security Considerations

Starting permissive (trusted code, container provides isolation):

```dockerfile
# No special security restrictions initially
# Container isolation is the security boundary
```

Future hardening options:
- Drop capabilities
- Read-only root filesystem (except /workspace, /root/.claude)
- No network access for sandboxed runs
- seccomp profiles

## References

- [Docker container commit](https://docs.docker.com/reference/cli/docker/container/commit/)
- [Claude Code OAuth](https://github.com/anthropics/claude-code/issues/16238)
- [Docker pause/unpause](https://docs.docker.com/reference/cli/docker/container/pause/)
