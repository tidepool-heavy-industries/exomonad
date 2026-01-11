# DevLog Executor - Session-Scoped Agent Logging

Interprets the `DevLog` effect for structured, greppable session logs.

## When to Read This

Read this if you're:
- Debugging agent sessions via log analysis
- Understanding the observability pipeline
- Adding new loggable events to agents
- Working with sleeptime log analysis

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ Agent Effects                                                        │
│   devLogGraph GraphTransitionInfo / devLogLLMRequest LLMRequestInfo │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ DevLog effect
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ DevLog Executor                                                      │
│   1. Generate session ID + timestamp                                │
│   2. Filter by verbosity level                                      │
│   3. Format events (greppable patterns)                             │
│   4. Write to file/stderr/both                                      │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ File IO
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Session Log Files                                                    │
│   logs/{session-id}.log + logs/latest.log (symlink)                 │
└─────────────────────────────────────────────────────────────────────┘
```

## Usage

```haskell
import Tidepool.Effect.DevLog
import Tidepool.DevLog.Executor (runDevLog)
import Tidepool.DevLog.Config

main :: IO ()
main = do
  let config = defaultDevLogConfig
        { dcOutput = OutputFile "./logs"
        , dcSymlinkLatest = True
        }
  runM $ runDevLog config $ do
    devLogGraph GraphTransitionInfo
      { gtiFromNode = "entry"
      , gtiToNode = "classify"
      , gtiTrigger = "user_input"
      }
    devLogLLMRequest VNormal LLMRequestInfo
      { lriModel = "claude-3"
      , lriPromptTokens = 100
      , lriCompletionTokens = 50
      }
```

## Event Types

| Event | Log Prefix | Purpose |
|-------|------------|---------|
| Graph transition | `GRAPH` | Node-to-node moves |
| State change | `STATE.{field}` | Agent state mutations |
| LLM request | `LLM.{model}` | API call metadata |
| Error | `ERROR` | Failures with context |

## Greppable Patterns

```bash
# All graph transitions
grep "GRAPH" logs/latest.log

# All state changes
grep "STATE\." logs/latest.log

# All LLM calls
grep "LLM\." logs/latest.log

# All errors
grep "ERROR" logs/latest.log

# Transitions to specific node
grep "→ bargain" logs/latest.log
```

## Configuration

```haskell
data DevLogConfig = DevLogConfig
  { dcOutput :: OutputMode         -- Stderr, File, Both
  , dcVerbosity :: Verbosity       -- VQuiet, VNormal, VVerbose
  , dcSessionId :: Maybe UUID      -- Override session ID
  , dcSessionName :: Maybe Text    -- Human-readable name
  , dcSymlinkLatest :: Bool        -- Create latest.log symlink
  }

data OutputMode
  = OutputStderr           -- Write to stderr only
  | OutputFile FilePath    -- Write to file only
  | OutputBoth FilePath    -- Write to both

defaultDevLogConfig :: DevLogConfig
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `Executor.hs` | Effect interpreter, file management |
| `Config.hs` | Configuration types |
| `Formatter.hs` | Event formatting for grep |

## Integration with Sleeptime

DevLog output is the input to sleeptime analysis:

```bash
# Analyze session logs
sleeptime analyze logs/latest.log

# Find slow LLM calls
sleeptime slow-calls logs/

# Trace graph execution path
sleeptime trace logs/session-id.log
```

See `tools/sleeptime/CLAUDE.md` for sleeptime usage.

## Related Documentation

- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter pattern
- [effects/observability-executor/CLAUDE.md](../observability-executor/CLAUDE.md) - OTLP traces (external)
- [tools/sleeptime/CLAUDE.md](../../tools/sleeptime/CLAUDE.md) - Log analysis CLI
