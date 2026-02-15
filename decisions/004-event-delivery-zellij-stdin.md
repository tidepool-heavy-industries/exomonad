# ADR 004: Event Delivery via Zellij STDIN Injection

**Status:** Accepted (design decided, implementation in progress)

## Context

Parent nodes in the hylo tree need to receive events from children (completion, questions, PR status changes) without burning tokens polling. Parents are Claude Code sessions running in Zellij panes that accept user input via STDIN.

## Decision

Events are delivered as synthetic user messages injected into the parent's Zellij pane via `zellij action write-chars`.

The MCP server is the router:
1. Source generates a structured event (child hook, GitHub poll, etc.)
2. Server resolves target parent session from child's identity (birth-branch hierarchy)
3. Server renders the event through a natural language template
4. Server injects the rendered message into the parent's Zellij pane
5. Parent Claude sees it as a user message and responds

### Event Sources

| Source | Trigger | Mechanism |
|--------|---------|-----------|
| Child completion | Child's SessionEnd hook | Hook → HTTP POST → server |
| Child question | Child calls `question` tool | Tool → effect → server |
| Child note | Child calls `note` tool | Tool → effect → server |
| Copilot review | GitHub API poll | Polling loop → server |
| CI status change | GitHub API poll | Polling loop → server |

### Event Registration

Implicit from spawn. The branch hierarchy (`main.feature.auth` → parent `main.feature`) determines routing. No explicit subscribe/unsubscribe.

## Implementation Status

- The `write-chars` injection mechanism exists (Zellij STDIN injection effect)
- `note`, `question`, `answer_question` messaging tools are implemented
- `notify_parent` + `wait_for_event` tools are implemented
- Event router (pane registry + injection endpoint) is not yet built — this is the next major piece
- GitHub poller is not yet built

## Consequences

- Zero token cost while parent is dormant
- Natural integration with Claude Code's conversation model
- Coupled to Zellij (can't run headless without adaptation)
- Message ordering depends on injection timing
