# Claude Code Session Resumption

This document describes how to resume and fork Claude Code sessions, based on patterns from Gas Town's `seance` command.

## Overview

Claude Code sessions can be resumed or forked using command-line flags. This enables:
- **Debugging**: Ask a previous session what it did and why
- **Handoffs**: Pick up where a predecessor left off
- **Continuity**: Resume work across tool invocations

## Command-Line Flags

### `--resume <session-id>`

Resumes an existing session by ID. The session continues with full context from the previous run.

```bash
claude --resume abc123def456
```

### `--fork-session`

Combined with `--resume`, loads a predecessor's context **without modifying their session**. Creates a new session branched from the original.

```bash
claude --fork-session --resume abc123def456
```

### `--print <prompt>` (One-Shot Mode)

Combined with `--resume`, sends a single prompt and exits. Useful for scripted queries.

```bash
claude --fork-session --resume abc123def456 --print "Where did you put the config changes?"
```

## Session Discovery

Sessions can be discovered from:

1. **Events file** (`~/gt/.events.jsonl` in Gas Town)
   - Contains `session_start` events with session IDs
   - Emitted by `SessionStart` hooks

2. **Claude's `/resume` command**
   - Lists recent sessions with searchable metadata
   - The `[GAS TOWN]` beacon makes sessions findable

### Event Format

```json
{
  "ts": "2025-01-04T10:30:00Z",
  "type": "session_start",
  "actor": "polecat:gastown",
  "payload": {
    "session_id": "abc123def456",
    "topic": "refactoring auth module"
  }
}
```

## Use Cases

### 1. Seance Pattern (Ask Predecessors)

"Talk to your predecessor" - ask a previous session's Claude questions about what it did:

```bash
# Interactive mode - full conversation with predecessor context
claude --fork-session --resume <session-id>

# One-shot query
claude --fork-session --resume <session-id> --print "Why did you make this decision?"
```

### 2. Session Continuity in Agents

For long-running agents that spawn Claude Code subprocesses:

```haskell
-- Capture session ID from first run
firstResult <- runClaudeCode prompt
let sessionId = extractSessionId firstResult

-- Resume in subsequent runs
subsequentResult <- runClaudeCodeResume sessionId followupPrompt
```

### 3. Tool Invocation Continuity

When a graph node needs multiple Claude Code calls that should share context:

```haskell
-- First call establishes session
result1 <- execClaudeCode Sonnet cwd "Analyze the codebase" schema Nothing
let sessionId = result1.sessionId

-- Subsequent calls resume same session
result2 <- execClaudeCodeResume sessionId "Based on your analysis, suggest refactoring"
```

## Implementation Considerations

### Session ID Capture

Claude Code returns session IDs in its output. To capture:
1. Parse the JSON output for session metadata
2. Store in state for subsequent resumption
3. Emit as event for discovery by other agents

### Session Lifetime

- Sessions persist in Claude's storage
- Can be resumed until they expire (exact policy TBD)
- Fork creates a new session with inherited context

### Effect Integration

For ExoMonad's effect system, session resumption could be:

```haskell
data ClaudeCodeExec r where
  -- Existing: new session
  ClaudeCodeExecOp :: ModelChoice -> Maybe FilePath -> Text -> Maybe Value -> Maybe Text
                   -> ClaudeCodeExec Value

  -- New: resume existing session
  ClaudeCodeResumeOp :: Text -> Text -> Maybe Value
                     -> ClaudeCodeExec Value

  -- New: fork and query (one-shot)
  ClaudeCodeForkQueryOp :: Text -> Text
                        -> ClaudeCodeExec Value
```

## References

- Claude Code documentation: `/help` within a Claude Code session
