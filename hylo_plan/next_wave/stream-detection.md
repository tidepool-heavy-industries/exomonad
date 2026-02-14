# Stream Detection: Parent-Defined Watchers

## Idea

Parents could define lightweight watchers on child agent output streams. The parent says "watch for X" and a classifier monitors the child's terminal output, triggering an event if the pattern matches.

## Motivation

LLM agents exhibit predictable tropes when they're stuck, drifting, or hallucinating:
- Repeating the same approach after failure
- Generating increasingly verbose "let me try another approach" text
- Hallucinating nonexistent APIs or flags
- Going silent (no output for extended period)

A parent with architectural context knows what "off-track" looks like for each child's specific task. It could define watchers at spawn time:

```
watch_for:
  - pattern: "let me try a different approach"
    count: 3
    action: notify_parent
  - pattern: "error: command not found"
    action: notify_parent_immediately
  - silence_timeout: 300s
    action: notify_parent
```

## Architecture Options

1. **Regex on terminal output** — Simple, fast, no LLM cost. Catches specific phrases.
2. **Small classifier model** — Run a tiny model (or even a Gemini Flash call) on buffered output every N seconds. More semantic but costs tokens.
3. **Heuristic rules** — Count tool call failures, track time-since-last-commit, detect repeated identical tool calls. No LLM needed.

## Interaction with Hylo Tree

- Parent defines watchers in spawn prompt or via MCP tool
- Watchers run as background tasks in the MCP server
- Events route through the standard event router (ADR-001)
- Parent receives "[DRIFT DETECTED]" messages, can intervene

## Status

Post-Phase-1. Interesting but orthogonal to the core coordination loop.
