# Session Executor - Claude Code Subprocess Orchestration

Interprets the `Session` effect by spawning `mantle session` processes for Claude Code execution.

## When to Read This

Read this if you're:
- Understanding how ClaudeCode graph nodes execute
- Debugging session continuation issues
- Implementing new session operations
- Working on the types-first-dev V3 protocol

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ Haskell (Session Effect)                                            │
│   startSession "impl/feat" "Implement feature" Sonnet               │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ spawn subprocess
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ mantle session start --slug impl/feat --model sonnet "..."          │
│   │                                                                 │
│   │ Docker container with Claude Code                               │
│   ▼                                                                 │
│ JSON result to stdout { sessionId, output, toolCalls }              │
└─────────────────────────────────────────────────────────────────────┘
```

## Key Insight: Sessions Not Containers

**Containers are ephemeral** (one turn each). **Sessions persist** via:

- Shared `~/.claude/` volume (conversation history lives here)
- `--resume <session-id>` for continuation
- `--fork-session` for read-only parallel work

**Mantle manages**:
- Worktrees at `.mantle/worktrees/<branch>/`
- Session state in `.mantle/sessions.json`

**Haskell is stateless** - all persistence is in mantle's data structures.

## Effect Types

### Session Effect

```haskell
data Session m a where
  StartSession :: SessionSlug -> Text -> ModelChoice -> Session m SessionOutput
  ContinueSession :: SessionId -> Text -> Session m SessionOutput
  ForkSession :: SessionId -> SessionSlug -> Text -> Session m SessionOutput
  SessionInfo :: SessionId -> Session m (Maybe SessionMetadata)
```

### SessionOperation (Handler Decision)

Handlers return `SessionOperation` to control session strategy:

```haskell
data SessionOperation
  = StartFresh SessionSlug    -- Create new session
  | ContinueFrom SessionId    -- Reuse existing (threads history)
  | ForkFrom SessionId SessionSlug  -- Create child session
```

### SessionOutput

```haskell
data SessionOutput = SessionOutput
  { soSessionId :: SessionId      -- Unique session identifier
  , soOutput :: Text              -- Claude's text output
  , soToolCalls :: [ToolCall]     -- Decision tools called
  , soMetadata :: SessionMetadata -- Duration, tokens, etc.
  }
```

## Usage Pattern

### In a Graph Handler

```haskell
-- Before function returns session strategy
implBefore :: ImplInput -> Eff es (ImplContext, SessionOperation)
implBefore input = do
  mem <- getMem @ImplMem
  let op = case mem.imSessionId of
        Just sid -> ContinueFrom sid    -- Reuse for retry
        Nothing  -> StartFresh "v3/impl" -- Fresh on first attempt
  pure (buildContext input, op)

-- After function stores session ID
implAfter :: ImplInput -> (ImplExit, SessionId) -> Eff es (GotoChoice targets)
implAfter input (exit, sid) = do
  updateMem @ImplMem $ \m -> m { imSessionId = Just sid }
  case exit of
    ImplTestsPassed ... -> pure $ gotoChoice @"v3TDDReviewImpl" ...
    ImplRequestRetry ... -> pure $ gotoSelf (input { iiAttemptCount = ... })
```

### Direct IO Usage

```haskell
import Tidepool.Session.Executor (runSessionIO, defaultSessionConfig)
import Tidepool.Effect.Session

config = defaultSessionConfig "/path/to/repo"

result <- runM $ runSessionIO config $ do
  out <- startSession "impl/feat-x" "Implement feature X" Sonnet
  case detectForkRequest out of
    Just (childSlug, prompt) ->
      forkSession (soSessionId out) childSlug prompt
    Nothing ->
      continueSession (soSessionId out) "Continue implementation"
```

## Configuration

```haskell
data SessionConfig = SessionConfig
  { scMantlePath :: FilePath    -- Path to mantle binary (default: "mantle")
  , scRepoRoot :: FilePath      -- Git repo root for worktrees
  }

defaultSessionConfig :: FilePath -> SessionConfig
```

## Session Continuation Pattern

For retry loops where context should be preserved:

```haskell
-- First attempt: StartFresh
-- Retry 1: ContinueFrom session1  -- Context preserved!
-- Retry 2: ContinueFrom session1  -- Same conversation continues
-- Retry 3: ContinueFrom session1  -- Claude remembers previous attempts
```

This is critical for TDD loops where Claude needs to remember what it already tried.

## Fork Pattern (Parallel Work)

For tree decomposition with parallel children:

```haskell
-- Parent creates child sessions
forkSession parentId "child-1" "Implement module A"
forkSession parentId "child-2" "Implement module B"

-- Children get read-only access to parent's context
-- Each child has its own conversation history
-- Parent can continue after children complete
```

## Testing

```bash
cabal test tidepool-session-executor
```

Test modules:
- `JsonSpec.hs` - JSON parsing for session output
- `DecisionToolsIntegrationSpec.hs` - Tool call parsing E2E

## Related Documentation

- [dsl/core/CLAUDE.md](../../dsl/core/CLAUDE.md) - ClaudeCodeLLMHandler, SessionOperation
- [types-first-dev/CLAUDE.md](../../../types-first-dev/CLAUDE.md) - V3 protocol using sessions
- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter pattern
