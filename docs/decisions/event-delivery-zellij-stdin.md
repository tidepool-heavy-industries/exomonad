# Event Delivery: Teams Inbox + Zellij Fallback

**Status:** Implemented

## Decision

Events are delivered via a two-tier strategy: **Claude Teams inbox first, Zellij STDIN injection as fallback.** This is implemented in a single shared helper (`services/delivery.rs`).

### How It Works

1. Source generates a structured event (child completion, Copilot review, CI status change)
2. Server calls `delivery::deliver_to_agent()` with the target agent's identity
3. Helper tries Teams inbox delivery first:
   - Looks up agent in `TeamRegistry` (populated by `session.register_team` effect at SessionStart)
   - Writes to `~/.claude/teams/{team}/inboxes/{inbox}.json` via `teams_mailbox::write_to_inbox()`
   - Claude Code polls the inbox file and injects the message into the conversation
4. If Teams delivery fails (no registry entry, directory missing, write error), falls back to Zellij injection:
   - Resolves the target agent's Zellij pane from its identity (birth-branch hierarchy)
   - Injects text into the pane via the Zellij plugin pipe (`inject_input`)
   - Agent sees it as a user message and responds

The Zellij fallback has a constraint: the **Ink paste problem**. React Ink (used by Claude Code and Gemini CLI) treats multi-byte stdin writes arriving in the same event loop tick as clipboard paste events. The Zellij plugin defers the Enter keypress by ~100ms via `set_timeout(0.1)` + `Timer` event, ensuring Ink processes it as an isolated keypress.

### Event Sources

| Source | Trigger | Mechanism |
|--------|---------|-----------|
| Child completion | Child calls `notify_parent` | MCP tool → `deliver_to_agent()` → Teams inbox or Zellij injection |
| Copilot review | GitHub API poll | Background poller → `deliver_to_agent()` → Teams inbox or Zellij injection |
| CI status change | GitHub API poll | Background poller → `deliver_to_agent()` → Teams inbox or Zellij injection |

### Delivery Helper (`services/delivery.rs`)

```rust
pub async fn deliver_to_agent(
    team_registry: Option<&TeamRegistry>,
    agent_key: &str,          // TeamRegistry lookup key
    zellij_tab_name: &str,    // Fallback tab name
    from: &str,               // Sender name
    message: &str,            // Message content
    summary: &str,            // Short summary for Teams
    color: &str,              // Teams message color
) -> DeliveryResult { ... }

pub enum DeliveryResult { Teams, Zellij, Failed }
```

All callers (events.rs, github_poller.rs) use this single code path.

### Event Registration

Implicit from spawn. The branch hierarchy (`main.feature.auth` → parent `main.feature`) determines routing. No explicit subscribe/unsubscribe.

### Implementation

- **Delivery helper**: `services/delivery.rs` — shared Teams+Zellij delivery
- **Event handler**: `handlers/events.rs` — `notify_parent` calls `deliver_to_agent()`
- **GitHub poller**: `services/github_poller.rs` — PR/CI notifications call `deliver_to_agent()`
- **Event log**: `.exo/events.jsonl` — append-only JSONL (durable record, independent of delivery)

## Consequences

- Teams inbox provides durable delivery (messages survive agent turn boundaries)
- Zellij fallback provides immediate delivery when Teams is unavailable
- Zero token cost while an agent is dormant (no polling loop on agent side)
- Natural integration with Claude Code's conversation model
- Coupled to Zellij for fallback — can't run fully headless without adaptation

## Known Gaps

- **Headless mode**: Zellij fallback unavailable. Teams-only delivery works but depends on Claude Code polling.
- **Message ordering**: If two children complete simultaneously, delivery order is non-deterministic.
- **Inbox growth**: Teams inbox files are JSON arrays that grow unboundedly (idle notifications every 2-4s contribute). No rotation mechanism.
- **Lock coordination**: Our `teams_mailbox` uses atomic temp+rename. Claude Code uses `.lock` sidecar files. We don't currently coordinate with their lock protocol — potential for lost updates under high concurrency.
- **Retry**: No retry on delivery failure. If both Teams and Zellij fail, the event is lost (JSONL log is the durable record).
