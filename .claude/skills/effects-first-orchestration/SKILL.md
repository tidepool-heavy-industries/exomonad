---
name: effects-first-orchestration
description: Use when designing orchestration logic, MCP tools, or any code that interacts with external systems. The DSL is the orchestration engine - all logic lives in effects, not IO.
---

# Effects-First Orchestration

## Core Principle

**The DSL is the orchestration engine.** All logic lives in effects, not IO. Shell scripts and raw IO are anti-patterns.

## Why Effects Matter

| Aspect | IO/Shell Scripts | Effects |
|--------|------------------|---------|
| **Testable** | Integration only | Mock interpreters |
| **Composable** | Subprocess hell | Effect composition |
| **Observable** | Opaque | Tracing built-in |
| **Portable** | Unix only | WASM-compatible |
| **Type-safe** | Strings | Typed data flow |

## Pattern: Effect-Based MCP Tools

### ✅ Correct

```haskell
spawnAgentLogic
  :: (Member GitHub es, Member Git es, Member FileSystem es, Member Zellij es)
  => [IssueNum]
  -> Eff es SpawnResult
spawnAgentLogic issueNums = do
  forM issueNums $ \issueNum -> do
    -- Typed effects, testable, traceable
    issueInfo <- getIssue repo issueNum False    -- GitHub effect
    worktree <- createWorktree (branchName issueNum) -- Git effect

    -- Context injection via effects
    let contextPath = worktree </> ".claude/context/issue.md"
    writeFile contextPath (renderIssue issueInfo) -- FileSystem effect

    -- Launch via effect (not shell)
    tabId <- newTab (TabConfig worktree issueNum) -- Zellij effect

    pure (issueNum, worktree, tabId)
```

### ❌ Anti-Pattern

```haskell
spawnAgentLogic :: [IssueNum] -> IO SpawnResult
spawnAgentLogic issueNums = do
  forM issueNums $ \issueNum -> do
    -- Shelling out - untestable, unobservable, not portable
    callProcess "gh" ["issue", "view", show issueNum]
    callProcess "git" ["worktree", "add", path]
    callProcess "zellij" ["action", "new-tab", ...]
```

## Effect Design Guidelines

### Granularity

Prefer domain-specific effects over generic "Subprocess" effect:

```haskell
-- ✅ Domain-specific (clear intent, better testing)
data Zellij m a where
  NewTab :: TabConfig -> Zellij m TabId
  GoToTab :: TabName -> Zellij m ()
  CloseTab :: TabId -> Zellij m ()

-- ❌ Too generic (loses semantic meaning)
data Subprocess m a where
  Run :: String -> [String] -> Subprocess m ExitCode
```

### Interpreters

Each effect needs interpreters for different contexts:

```haskell
-- Production: actually executes
runZellijIO :: Eff (Zellij ': es) a -> Eff es a

-- Testing: records calls, returns mocks
runZellijPure :: Eff (Zellij ': es) a -> Eff es (a, [ZellijCall])

-- Tracing: wraps production with observability
runZellijTraced :: Eff (Zellij ': es) a -> Eff (Observability ': es) a
```

### Composition

Effects compose naturally:

```haskell
fullSpawnLogic
  :: (Member GitHub es, Member Git es, Member FileSystem es, Member Zellij es, Member Observability es)
  => ...

-- In production
runM
  $ runObservability config
  $ runZellijIO
  $ runFileSystemIO
  $ runGitIO
  $ runGitHubIO config
  $ fullSpawnLogic issueNums

-- In tests
runIdentity
  $ runZellijPure
  $ runFileSystemPure
  $ runGitPure
  $ runGitHubPure
  $ fullSpawnLogic issueNums
```

## Common Effects Needed

| Effect | Operations | Interpreter |
|--------|------------|-------------|
| `GitHub` | getIssue, listIssues, createIssue | gh CLI or direct API |
| `Git` | createWorktree, checkout, commit | git CLI |
| `FileSystem` | writeFile, mkdir, symlink | System.Directory |
| `Zellij` | newTab, goToTab, closeTab | zellij CLI |
| `ProcessCompose` | addProcess, removeProcess, update | REST API |

## Migration Path

When you find IO-based orchestration:

1. **Identify the operations** (what external things does it touch?)
2. **Design effect type** (what's the minimal interface?)
3. **Extract logic** (pure function using effect)
4. **Write interpreters** (IO for prod, Pure for test)
5. **Delete shell scripts** (they're now redundant)

## Red Flags

Watch for these anti-patterns in code review:

- `callProcess`, `system`, `readProcess` in business logic
- `./scripts/*.sh` called from Haskell
- `IO` in type signatures where effects would work
- String-based command construction
- Untyped JSON parsing of CLI output

## References

- `haskell/dsl/core/CLAUDE.md` - Effect system fundamentals
- `haskell/effects/CLAUDE.md` - Interpreter patterns