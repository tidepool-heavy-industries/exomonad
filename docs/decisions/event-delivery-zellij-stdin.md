# Event Delivery via Zellij STDIN Injection

**Status:** Implemented

## Decision

Events are delivered as synthetic user messages injected into an agent's Zellij pane. The MCP server is the router — no agent polls for events.

### How It Works

1. Source generates a structured event (child completion, Copilot review, CI status change)
2. Server resolves the target agent's Zellij pane from its identity (birth-branch hierarchy)
3. Server renders the event as natural language text
4. Server injects the text into the target pane via the Zellij plugin pipe (`inject_input`)
5. Agent sees it as a user message and responds

The key constraint is the **Ink paste problem**: React Ink (used by Claude Code and Gemini CLI) treats multi-byte stdin writes arriving in the same event loop tick as clipboard paste events. The Zellij plugin defers the Enter keypress by ~100ms via `set_timeout(0.1)` + `Timer` event, ensuring Ink processes it as an isolated keypress and fires `onSubmit`.

### Event Sources

| Source | Trigger | Mechanism |
|--------|---------|-----------|
| Child completion | Child calls `notify_parent` | MCP tool → server resolves parent → Zellij injection |
| Copilot review | GitHub API poll | Background poller → server → Zellij injection |
| CI status change | GitHub API poll | Background poller → server → Zellij injection |

### Event Registration

Implicit from spawn. The branch hierarchy (`main.feature.auth` → parent `main.feature`) determines routing. No explicit subscribe/unsubscribe.

### Implementation

- **Event router**: `notify_parent` → server resolves parent tab from caller's birth-branch → `inject_input` into parent's Zellij pane via plugin pipe
- **GitHub poller**: Background service polls PR/CI status, injects notifications into agent panes
- **Event log**: `.exo/events.jsonl` — append-only JSONL. Events: `agent.spawned`, `agent.completed`, `pr.filed`, `pr.merged`, `copilot.review`, `ci.status_changed`

## Consequences

- Zero token cost while an agent is dormant (no polling loop)
- Natural integration with Claude Code's conversation model (injected text = user message)
- Coupled to Zellij — can't run headless without adaptation
- Message ordering depends on injection timing (no causal ordering guarantees)

## Known Gaps

- **Headless mode**: No way to run without Zellij. Would need an alternative delivery mechanism (HTTP long-poll, WebSocket) for CI/cloud environments.
- **Message ordering**: If two children complete simultaneously, injection order is non-deterministic. No sequence numbers or causal ordering.
- **Event persistence**: Events are fire-and-forget. If the target pane is dead or the agent has exited, the event is lost. The JSONL log is the only durable record.
- **Retry**: No retry on injection failure. If Zellij plugin pipe is unavailable, the event is silently dropped.
