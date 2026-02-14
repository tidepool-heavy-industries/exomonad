# ADR-001: Event Delivery via Zellij STDIN Injection

## Status

Accepted

## Context

Parent nodes in the hylo tree need to receive events from children (completion, questions, PR status changes) without burning tokens polling. The parent is a Claude Code session running in a Zellij pane. Claude Code sessions accept user input via STDIN.

## Decision

**Events are delivered as synthetic user messages injected into the parent's Zellij pane via `zellij action write-chars`.**

The MCP server is the router. When an event occurs:

1. Source generates a structured event (child hook, GitHub poll, etc.)
2. Server resolves the target parent session from the child's identity (birth-branch hierarchy)
3. Server renders the event through a natural language template
4. Server injects the rendered message into the parent's Zellij pane via `write-chars`
5. Server sends Enter keystroke to submit
6. Parent Claude sees it as a user message and responds

### Event Sources

| Source | Trigger | Mechanism |
|--------|---------|-----------|
| Child completion | Child's SessionEnd hook | Hook → HTTP POST → server |
| Child question | Child calls `question` tool | Tool → effect → server |
| Child note | Child calls `note` tool | Tool → effect → server |
| PR ready | Child goes dormant after Copilot approves | Child hook → server |
| Copilot review posted | GitHub API poll | Polling loop → server |
| CI status change | GitHub API poll | Polling loop → server |

### Message Templates

Messages are natural language rendered from structured data. The parent Claude reads them naturally without needing to parse JSON.

```
[CHILD COMPLETE] Agent 'auth-module' on branch main.feature.auth has completed.
PR #42 filed against your branch. Copilot status: approved. CI: passing.
Review with: gh pr view 42
```

```
[QUESTION from auth-module] I'm implementing the JWT middleware but the
token validation types aren't in the scaffold commit. Should I define them
locally or is another sibling handling this?
Reply with: answer_question agent=auth-module message="..."
```

```
[COPILOT REVIEW] PR #42 (auth-module) received new Copilot review.
Status: changes_requested. 3 comments.
View: https://github.com/owner/repo/pull/42
```

### Event Registration

**Implicit from spawn.** When a parent spawns a child via `spawn_subtree` or `spawn_workers`, the server automatically registers the parent as the event target for that child's lifecycle events. The mapping is derived from the branch hierarchy:

- Child branch: `main.feature.auth`
- Parent branch: `main.feature`
- Parent session ID: `main.feature` (birth-branch)

No explicit subscribe/unsubscribe needed.

### Pane Resolution

The server needs to find the parent's Zellij pane to inject messages. Resolution:

1. Server stores `(session_id → zellij_pane_id)` mapping at spawn time
2. The parent's pane ID is captured when the parent session starts
3. For the root TL, pane ID is captured at `exomonad init` time

## Consequences

**Positive:**
- Zero token cost while dormant (no polling tool calls)
- Natural integration with Claude Code's conversation model
- Events feel like a human collaborator providing updates
- Simple: no custom event bus, no WebSocket, just Zellij primitives

**Negative:**
- Coupled to Zellij (can't run headless without adaptation)
- Message ordering depends on injection timing (no guaranteed ordering)
- Parent must be in a state where it accepts user input (not mid-tool-call)
- Rate limiting needed to avoid overwhelming parent with rapid events

**Risks:**
- If parent is mid-tool-call when event arrives, the message may be queued by the terminal but arrive at an awkward time. Mitigation: events are informational, not transactional. Parent processes them when ready.
- Zellij pane ID may change if pane is closed/recreated. Mitigation: health check before injection, re-resolve if stale.

## Implementation Notes

- The `write-chars` injection mechanism is already built (Zellij STDIN injection effect)
- Template rendering can be a simple Haskell function in WASM (structured event → Text)
- GitHub polling loop runs as a background task in the MCP server process
- Polling interval: 30s for active PRs, 5m for idle (backoff when no open PRs)
