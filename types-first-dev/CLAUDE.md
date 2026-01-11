# types-first-dev

TDD workflow orchestrator using Claude Code agents in Docker containers.

## When to Read Which Docs

| I want to... | Read this |
|--------------|-----------|
| Run the TDD workflow | Quick Start (below) |
| Understand handler signatures | "Types and Architecture" section |
| Debug Docker auth issues | `DOCKER-AUTH.md` |
| Understand graph DSL | `../haskell/dsl/core/CLAUDE.md` |
| Understand session continuation | `../haskell/effects/session-executor/CLAUDE.md` |
| See implementation notes | `IMPLEMENTATION-NOTES.md` (if exists) |

## Quick Start

### Linux/WSL (Recommended)

```bash
# 1. Authenticate on host (if not already done)
claude login

# 2. Build the mantle stack (REQUIRED before first run)
just rebuild-mantle

# 3. Run
./run-actor-graph.sh specs/url-shortener.yaml
```

On Linux/WSL, mantle automatically mounts `~/.claude` from the host. No volume setup needed.

### macOS

```bash
# 1. Setup Docker auth (one-time)
docker-compose up -d
docker-compose exec auth-shell claude login

# 2. Configure mantle to use auth volume
cp mantle-config.example.toml ~/.config/mantle/config.toml

# 3. Build the mantle stack (REQUIRED before first run)
just rebuild-mantle

# 4. Run
./run-actor-graph.sh specs/url-shortener.yaml
```

On macOS, OAuth tokens are stored in the system keychain rather than `~/.claude`, so a Docker volume is required. See [DOCKER-AUTH.md](DOCKER-AUTH.md) for detailed setup instructions.

---

## Architecture Overview

### Components

```
┌─────────────────────────────────────────────────────────────────┐
│                         HOST MACHINE                             │
│                                                                  │
│  ┌──────────────────┐    ┌──────────────────────────────────┐   │
│  │ types-first-dev  │    │          mantle (Rust)           │   │
│  │    (Haskell)     │───▶│  - Reads ~/.config/mantle/       │   │
│  │                  │    │  - Spawns Docker containers      │   │
│  │  Actor graph     │    │  - Streams Claude Code output    │   │
│  │  orchestration   │    │  - Logs to .mantle/logs/         │   │
│  └──────────────────┘    └──────────────────────────────────┘   │
│                                       │                          │
│                                       ▼                          │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │                    DOCKER CONTAINERS                      │   │
│  │                                                           │   │
│  │  ┌─────────────────┐  ┌─────────────────┐                │   │
│  │  │  auth-shell     │  │  worker (N)     │                │   │
│  │  │  (persistent)   │  │  (ephemeral)    │                │   │
│  │  │                 │  │                 │                │   │
│  │  │  claude login   │  │  claude -p ...  │                │   │
│  │  │  stores creds   │  │  TDD scaffold   │                │   │
│  │  └────────┬────────┘  └────────┬────────┘                │   │
│  │           │                    │                          │   │
│  │           └────────┬───────────┘                          │   │
│  │                    ▼                                      │   │
│  │           tidepool-claude-auth (Docker volume)            │   │
│  └──────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

### Key Design Decisions

1. **Haskell orchestrator on host** — types-first-dev runs natively, not containerized
2. **mantle on host** — Rust binary that manages Docker containers
3. **Shared auth volume** — Single `claude login`, all workers use same creds
4. **Workers have GHC/cabal** — Docker image includes full Haskell toolchain via ghcup

---

## Build Pipeline

**Always run `just rebuild-mantle` after modifying:**
- `rust/mantle/Dockerfile` (Docker image)
- `rust/mantle/src/**` (host mantle binary)
- `~/.config/mantle/config.toml` (config changes need binary rebuild to take effect)

**Why:** The mantle stack has two independent artifacts that must stay in sync:
1. **Docker image** (`tidepool/claude-code:latest`) — runs Claude Code workers
2. **Host mantle binary** — reads config, spawns containers with correct image

If you update the Dockerfile but don't rebuild the host binary, mantle will use stale config and spawn the wrong image. The `just rebuild-mantle` recipe rebuilds both atomically.

```bash
just rebuild-mantle    # Full rebuild (Docker + host binary) — USE THIS
just rebuild-image     # Docker only (faster, no Rust compile)
just rebuild-mantle-bin # Host binary only
```

### Docker Image Contents

The `tidepool/claude-code:latest` image (built from `rust/mantle/Dockerfile`) includes:
- **Debian bookworm-slim** base
- **Node.js 20** + Claude Code CLI (`@anthropic-ai/claude-code`)
- **GHC 9.14.1** + **cabal 3.16.1** via ghcup
- **git** for commits
- **mantle-agent** binary (for hooks, not currently used)
- Non-root `user` with pre-created `/home/user/.claude`

### Config File

Location: `~/.config/mantle/config.toml`

```toml
[docker]
auth_volume = "tidepool-claude-auth"  # Named volume with Claude creds
image = "tidepool/claude-code:latest" # Image to use for workers
```

The host mantle binary reads this at startup. Changes require rebuilding mantle.

---

## Running

```bash
# From types-first-dev directory
./run-actor-graph.sh                              # default spec
./run-actor-graph.sh path/to/spec.yaml            # custom spec
./run-actor-graph.sh path/to/spec.yaml ./target   # custom spec + target dir
```

### Logs

Session logs live at: `<repo-root>/.mantle/logs/{session-id}.log`

```bash
# List recent sessions (from tidepool root)
ls -lt .mantle/logs/ | head -10

# Tail active session
tail -f .mantle/logs/{session-id}.log

# Search for errors
grep -l "cabal: command not found" .mantle/logs/*.log
```

Log format includes:
- `[MANTLE]` — Container lifecycle, tool calls
- `[EVENT]` — Claude Code streaming events
- `[STRUCTURED_OUTPUT]` — Final JSON response
- `[RESULT_TEXT]` — Human-readable summary

---

## Spec Files

Specs live in `specs/` as YAML. The V2 machinery is domain-blind—all domain knowledge lives in spec files.

```yaml
# specs/url-shortener.yaml
description: |
  An effect-based URL shortener service with Servant API.
  Uses algebraic effects (freer-simple) to separate concerns.

target_path: src/UrlShortener
test_path: test/UrlShortener

acceptance_criteria:
  - |
    UrlService effect GADT with operations:
    - Shorten :: LongUrl -> UrlService ShortUrl
    - Lookup :: ShortCode -> UrlService (Maybe LongUrl)
  - Persistence effect GADT with Store, Fetch, ListAll operations
  # ...more criteria
```

**Fields:**
- `description` — What we're building (multiline markdown OK)
- `acceptance_criteria` — Must-have features (list of strings, multiline OK)
- `target_path` — Where to write code
- `test_path` — Where to write tests
- `parent_branch` — (optional) Parent git branch for children
- `depth` — (optional) Recursion depth, defaults to 0

---

## Troubleshooting

### "cabal: command not found" in worker containers

**Cause:** Worker using old Docker image without GHC/cabal.

**Fix:**
```bash
just rebuild-mantle  # Rebuilds image AND host binary
```

**Verify:**
```bash
docker run --rm tidepool/claude-code:latest cabal --version
# Should show: cabal-install version 3.16.1.0
```

### Workers using wrong image (e.g., `mantle-agent:latest` instead of `tidepool/claude-code:latest`)

**Cause:** Host mantle binary not rebuilt after config change.

**Fix:**
```bash
just rebuild-mantle-bin  # Or full: just rebuild-mantle
```

**Verify:** Check logs for correct image:
```bash
grep "Image:" .mantle/logs/*.log | tail -5
# Should show: [MANTLE] Image: tidepool/claude-code:latest
```

### "fatal: not a git repository" with worktree path

**Cause:** Stale git worktree references. Worktrees created on host have paths that don't exist inside containers.

**Current workaround:** Workers initialize fresh git repos in their working directories. This is acceptable for scaffolding tasks.

**Proper fix (TODO):** Ensure worktrees are created inside the mounted workspace, or don't use worktrees for containerized workers.

### Docker build fails with I/O error

**Cause:** Docker buildkit storage corruption, often from disk space issues.

**Fix:**
```bash
docker builder prune -af  # Clean buildkit cache
just rebuild-mantle       # Retry
```

### Permission denied in `/home/user/.claude`

**Cause:** Docker volume created with wrong ownership.

**Fix:**
```bash
docker-compose down -v              # Remove volume
docker-compose up -d                # Recreate
docker-compose exec auth-shell claude login  # Re-auth
```

---

## Rubric Design Principle

**LLM is sensor, code is controller.**

The LLM reports structured fields describing what it did. The handler code decides if that's good enough. This separation:
- Saves tokens (routing logic never in prompt)
- Keeps policy in version-controlled code
- Enables mechanical evaluation

### Two-Stream Architecture

1. **Mechanical checks** - Handler computes these (build status, grep for undefined, test results). Never asks LLM.
2. **Semantic rubrics** - LLM reports these (approach taken, open questions, unhandled cases). Things we can't mechanically derive.

### Value-Neutral Field Design

**Key insight: Ask for useful information without hinting at consequences.**

The LLM has no reason to game responses when it doesn't know what we'll do with the answers.

| Don't ask | Do ask |
|-----------|--------|
| `confidence: 1-5` (inflatable) | `openQuestions: [Text]` (list length = derived metric) |
| `edgeCaseCoverage: 1-5` | `unhandledCases: [Text]` (content is actionable) |
| `completeness: "complete"` | (handler greps for undefined) |
| `severity: "critical"` | `attemptedSolutions: [Text]` + `wouldUnblock: Text` (infer severity) |

Every rubric field should be:
- **Semantic** - Can't be mechanically derived (otherwise handler should compute it)
- **Neutral** - No value ordering to optimize for
- **Actionable** - Non-empty list tells us exactly what to do next

---

## Types and Architecture (V3 Protocol)

### Session Management: SessionId and SessionOperation

**SessionId**: Opaque identifier for Claude Code session continuation:

```haskell
newtype SessionId = SessionId { unSessionId :: Text }
```

Sessions persist via shared Docker volume (`~/.claude/`). Each session maintains conversation history and file system state.

**SessionOperation**: Determines session strategy in before-handlers:

```haskell
data SessionOperation
  = StartFresh slug
      -- ^ Create new session identified by slug (e.g., "v3/impl")
      -- Used for initial node entry or when no prior context needed
  | ContinueFrom sessionId
      -- ^ Reuse existing session (preserves conversation history)
      -- Used for retry loops where we want to thread context
  | ForkFrom parentId childSlug
      -- ^ Create child session inheriting parent's state
      -- Used for parallel sub-tasks
```

**Pattern: Session Continuation in TDD Loop**

Handlers use Memory effect to thread SessionIds across node transitions:

```haskell
-- TDDWriteTests after-handler: store session ID
tddWriteTestsAfter (exit, sid) = do
  updateMem @TDDMem $ \m -> m { conversationId = Just sid }  -- Store it
  case exit of
    TDDTestsReady ... -> pure $ gotoChoice @"v3Impl" ...

-- Impl before-handler: reuse stored session (thread context across retries)
implBefore input = do
  mem <- getMem @ImplMem
  let sessionOp = case mem.imSessionId of
        Just sid -> ContinueFrom sid    -- REUSE for continuation
        Nothing -> StartFresh "v3/impl" -- Fresh on first attempt
  pure (implContext, sessionOp)

-- Impl after-handler: store for next retry
implAfter input (exit, sid) = do
  updateMem @ImplMem $ \m -> m { imSessionId = Just sid }  -- Store for retry
  case exit of
    ImplTestsPassed ... -> pure $ gotoChoice @"v3TDDReviewImpl" ...
    ImplRequestRetry ... | input.iiAttemptCount < 5 ->
      let retryInput = input { iiAttemptCount = input.iiAttemptCount + 1 }
      pure $ gotoSelf retryInput  -- Self-loop reuses stored session
```

### Tree Decomposition Types

**NodeInfo**: Identifies a node in the execution tree:

```haskell
data NodeInfo = NodeInfo
  { niId :: Text          -- Unique identifier
  , niBranch :: Text      -- Git branch for this node's work
  }
```

Used by parent to refer to children, and by Merger to verify contract compliance across child implementations.

**ChildSpec**: Specification for child sub-task:

```haskell
data ChildSpec = ChildSpec
  { csId :: Text
  , csDescription :: Text
  , csAcceptanceCriteria :: [Text]
  , csTargetPath :: FilePath        -- Where child should write code
  , csTestPath :: FilePath          -- Where child should write tests
  }
```

**ParentContext**: Context passed DOWN from parent to child:

```haskell
data ParentContext = ParentContext
  { pcInterface :: InterfaceFile    -- Parent's exported interface
  , pcAssignedCriteria :: [Text]    -- Which criteria the child owns
  }
```

**Pattern: Scaffold → Fork → Child Graphs**

1. **Scaffold** analyzes spec, creates ChildSpecs via decomposition
2. **Fork** node spawns children in parallel with ParentContext
3. Each **child** runs full TDDGraph recursively:
   - Impl writes code to csTargetPath
   - TDDReviewImpl approves implementation
   - Merger packages result with ParentContext info
4. **ImplBarrier** collects all MergeComplete results
5. **Root** returns aggregated MergeComplete

### Payloads: Cross-Node Communication

**InitWorkPayload**: Scaffold output → TDDWriteTests input:

```haskell
data InitWorkPayload = InitWorkPayload
  { iwpInterface :: InterfaceFile   -- What types to export
  , iwpContractSuite :: FilePath    -- Where property tests live
  }
```

**TestsReadyPayload**: TDDWriteTests output → Impl input:

```haskell
data TestsReadyPayload = TestsReadyPayload
  { trpCommit :: Text              -- Git commit hash after tests written
  , trpTestFiles :: [FilePath]     -- Which test files were created
  , trpPendingCriteria :: [Text]   -- Criteria not yet tested
  }
```

**ImplResult**: Impl output intermediate:

```haskell
data ImplResult = ImplResult
  { irCommit :: Text
  , irIterations :: Int
  , irPassedTests :: [Text]
  }
```

**MergeComplete**: Final result from Merger:

```haskell
data MergeComplete = MergeComplete
  { mcCommit :: Text                -- Merge commit hash
  , mcAuthor :: Text                -- Merge commit author
  , mcImpactLevel :: ImpactLevel    -- Severity (Trivial/Additive/Breaking)
  , mcChanges :: [ChangeEntry]      -- Summary of changes
  }
```

### Memory Structure: Shared vs Private

**ImplMem** (Impl node-private):

```haskell
data ImplMem = ImplMem
  { imConversationId :: Text        -- For logging
  , imSessionId :: Maybe SessionId  -- For retry continuation
  , imPassedTests :: [Text]
  , imAttemptHistory :: [AttemptRecord]
  }
```

Stored at node level, survives across self-loop retries. Enables ContinueFrom strategy.

**TDDMem** (Shared between TDDWriteTests ↔ TDDReviewImpl):

```haskell
data TDDMem = TDDMem
  { tmConversationId :: Text
  , tmCoveredCriteria :: [Text]     -- Tracks test coverage
  , tmPendingTests :: [Text]        -- Tests written but not passing
  }
```

Both nodes see same memory instance. Enables coordinated workflow.

---

## Current State (as of 2025-01-10)

### What's Working
- ✅ Docker auth flow via shared volume
- ✅ mantle config file support (`~/.config/mantle/config.toml`)
- ✅ Docker image with GHC 9.14.1 + cabal 3.16.1
- ✅ Worker containers can create files, init git, commit
- ✅ Structured output parsing from Claude Code
- ✅ Session logging to `.mantle/logs/`

### Known Issues
- ⚠️ Git worktrees broken inside containers (host paths don't exist)
- ⚠️ Must run `just rebuild-mantle` after any Dockerfile/config changes

### Key Files Modified Recently
- `rust/mantle/Dockerfile` — Added ghcup, GHC, cabal
- `rust/mantle/src/config.rs` — Config file parsing (NEW)
- `rust/mantle/src/docker/container.rs` — AuthMount enum, config reading
- `types-first-dev/docker-compose.yml` — Auth shell + volume (NEW)
- `types-first-dev/justfile` — Build recipes (NEW)
- `types-first-dev/CLAUDE.md` — This file
- `types-first-dev/DOCKER-AUTH.md` — Auth setup docs (NEW)
- `types-first-dev/mantle-config.example.toml` — Example config (NEW)

### Next Steps
1. Run `just rebuild-mantle` once Docker is available
2. Verify workers use correct image and have cabal
3. Run full TDD workflow: `./run-actor-graph.sh specs/url-shortener.yaml`
4. Address git worktree issue if it blocks workflows
