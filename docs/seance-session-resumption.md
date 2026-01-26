# Claude Code Session Resumption Research

**Date**: 2026-01-04

Research on Claude Code session resumption patterns from Gas Town's `seance` command.

## Key Claude Code CLI Flags

### `--resume <session-id>`
Resumes an existing Claude Code session, continuing from where it left off. The session retains full context from the previous conversation.

### `--fork-session`
When combined with `--resume`, creates a **read-only fork** of the session. This allows querying a predecessor session without modifying their session history. The forked session has full context but mutations don't affect the original.

### `--print <prompt>`
One-shot mode: executes a single prompt and exits without entering interactive mode.

## Session ID Format

Session IDs are UUIDs assigned by Claude Code. Example: `a1b2c3d4-e5f6-7890-abcd-ef1234567890`

## How Sessions Are Discovered (Gas Town Pattern)

### 1. Hook Captures Session ID

Claude Code sends session metadata to hooks via stdin JSON:

```json
{"session_id": "uuid", "transcript_path": "/path", "source": "startup|resume|clear|compact"}
```

The `SessionStart` hook (configured in `.claude/settings.json`) receives this:

```json
{
  "hooks": [{"type": "command", "command": "gt prime --hook"}]
}
```

### 2. Session ID Persistence

When `gt prime --hook` runs, it:
1. Reads session ID from stdin JSON
2. Persists to `.runtime/session_id` (both town root and cwd)
3. Sets environment: `GT_SESSION_ID`, `CLAUDE_SESSION_ID`
4. Outputs beacon: `[session:<uuid>]`

### 3. Event Emission

`gt prime` emits a `session_start` event to `~/gt/.events.jsonl`:

```json
{
  "ts": "2026-01-04T12:00:00Z",
  "type": "session_start",
  "actor": "gastown/crew/joe",
  "payload": {
    "session_id": "uuid",
    "role": "gastown/crew/joe",
    "topic": "",
    "cwd": "/path/to/workdir"
  }
}
```

### 4. Discovery via `gt seance`

`gt seance` reads `~/.events.jsonl`, filters for `session_start` events, and displays them:

```
SESSION_ID   ROLE                    STARTED           TOPIC
a1b2c3d4-…   gastown/crew/joe        2026-01-04 12:00  -
```

### 5. Resumption

```bash
# Interactive seance (talk to predecessor)
gt seance --talk <session-id>
# Executes: claude --fork-session --resume <session-id>

# One-shot question
gt seance --talk <session-id> -p "Where did you put the auth changes?"
# Executes: claude --fork-session --resume <session-id> --print "<prompt>"
```

## Session ID Resolution Priority

When Gas Town needs a session ID, it checks in order:

1. `GT_SESSION_ID` environment variable (new canonical)
2. `CLAUDE_SESSION_ID` environment variable (legacy)
3. Persisted file (`.runtime/session_id`)
4. Fallback: `<actor>-<pid>` (generated)

## Architecture Summary

```
┌─────────────────────────────────────────────────────────────┐
│                    Claude Code                               │
│  ┌─────────────────────────────────────────────────────────┐│
│  │ SessionStart Hook                                       ││
│  │   stdin: {"session_id": "uuid", ...}                    ││
│  │   → gt prime --hook                                     ││
│  └──────────────────────┬──────────────────────────────────┘│
└─────────────────────────┼───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                    gt prime --hook                           │
│  1. Parse stdin JSON                                         │
│  2. Persist to .runtime/session_id                           │
│  3. Set GT_SESSION_ID env                                    │
│  4. Emit session_start to events.jsonl                       │
│  5. Output [session:uuid] beacon                             │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                 ~/gt/.events.jsonl                           │
│  {"type":"session_start", "payload":{"session_id":"..."}}    │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                    gt seance                                 │
│  1. Read events.jsonl, filter session_start                  │
│  2. Display discoverable sessions                            │
│  3. --talk <id> → claude --fork-session --resume <id>        │
└─────────────────────────────────────────────────────────────┘
```

## Implications for ExoMonad Seance Integration

To implement seance-style session resumption in ExoMonad:

1. **Capture session ID at startup** - Read from `CLAUDE_SESSION_ID` env or stdin hook JSON
2. **Persist session ID** - Store in a known location (`.runtime/session_id` pattern)
3. **Emit discoverable events** - Log `session_start` events with payload
4. **Resume mechanism** - Shell out to `claude --fork-session --resume <id>`

The key insight: Claude Code stores full session transcripts indefinitely. Gas Town doesn't duplicate them—just maintains pointers (session IDs) for discovery and resume.
