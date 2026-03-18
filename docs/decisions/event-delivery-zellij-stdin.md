# Event Delivery: Teams Inbox + tmux Fallback

**Status:** Superseded — tmux replaced the previous engine (2026-03-13). Delivery model unchanged: Teams inbox primary, tmux STDIN injection fallback.

## Decision

Events are delivered via a two-tier strategy: **Claude Teams inbox first, tmux STDIN injection as fallback.** This is implemented in a single shared helper (`services/delivery.rs`).

### How It Works

1. Source generates a structured event (child completion, Copilot review, CI status change)
2. Server calls `delivery::deliver_to_agent()` with the target agent's identity
3. Helper tries Teams inbox delivery first:
   - Looks up agent in `TeamRegistry` (populated by `session.register_team` effect at SessionStart)
   - Writes to `~/.claude/teams/{team}/inboxes/{inbox}.json` via `teams_mailbox::write_to_inbox()`
   - Claude Code polls the inbox file and injects the message into the conversation
4. If Teams delivery fails (no registry entry, directory missing, write error), falls back to tmux injection:
   - Resolves the target agent's tmux pane from its identity (birth-branch hierarchy)
   - Injects text into the pane via the tmux buffer pattern (`load-buffer` + `paste-buffer`)
   - Agent sees it as a user message and responds

### Event Sources

| Source | Trigger | Mechanism |
|--------|---------|-----------|
| Child completion | Child calls `notify_parent` | MCP tool → `deliver_to_agent()` → Teams inbox or tmux injection |
| Copilot review | GitHub API poll | Background poller → `deliver_to_agent()` → Teams inbox or tmux injection |
| CI status change | GitHub API poll | Background poller → `deliver_to_agent()` → Teams inbox or tmux injection |

### Delivery Helper (`services/delivery.rs`)

```rust
pub async fn deliver_to_agent(
    team_registry: Option<&TeamRegistry>,
    acp_registry: Option<&AcpRegistry>,
    project_dir: &Path,
    agent_key: &str,          // TeamRegistry lookup key
    tmux_target: &str,        // Fallback tmux target
    from: &str,               // Sender name
    message: &str,            // Message content
    summary: &str,            // Short summary for Teams
) -> DeliveryResult { ... }

pub enum DeliveryResult { Teams, Acp, Uds, Tmux, Failed }
```

All callers (events.rs, github_poller.rs) use this single code path.

### Event Registration

Implicit from spawn. The branch hierarchy (`main.feature.auth` → parent `main.feature`) determines routing. No explicit subscribe/unsubscribe.

### Implementation

- **Delivery helper**: `services/delivery.rs` — shared Teams+tmux delivery
- **Event handler**: `handlers/events.rs` — `notify_parent` calls `deliver_to_agent()`
- **GitHub poller**: `services/github_poller.rs` — PR/CI notifications call `deliver_to_agent()`
- **Event log**: `.exo/logs/{agent_id}.jsonl` — per-agent append-only JSONL (durable record, independent of delivery)

## Consequences

- Teams inbox provides durable delivery (messages survive agent turn boundaries)
- tmux fallback provides immediate delivery when Teams is unavailable
- Zero token cost while an agent is dormant (no polling loop on agent side)
- Natural integration with Claude Code's conversation model
- Coupled to tmux for fallback — can't run fully headless without adaptation

## Open Work

See [teams-roadmap.md](teams-roadmap.md) for consolidated open items (lock coordination, delivery retry, headless mode, etc.).
