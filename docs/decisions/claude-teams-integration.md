# Claude Teams Integration

**Status:** Implemented (core flow working)

## Decision

ExoMonad bridges Claude Code Teams and Gemini workers so that Claude agents use native `SendMessage` for ALL teammates — zero tool shadowing. Gemini workers appear as first-class team members from Claude's perspective.

Claude Code Teams provides the orchestration UX (task lists, messaging, idle/shutdown). ExoMonad provides the infrastructure (heterogeneous agent support, typed effects, Zellij multiplexing, delivery routing).

## Architecture

```
Claude Code Teams (UX layer)
    ├── Task list (shared, file-based at ~/.claude/tasks/{team}/)
    ├── SendMessage / broadcast
    ├── Idle / shutdown negotiation
    └── Plan approval flow

ExoMonad (infrastructure layer)
    ├── Synthetic member registration (config.json manipulation)
    ├── Inbox watcher (inotify → Zellij injection for Gemini workers)
    ├── Unified delivery (Teams inbox first, Zellij fallback)
    ├── Heterogeneous agent support (Claude + Gemini)
    └── Hot-reloadable WASM policy
```

### Team Lifecycle

ExoMonad does NOT create team directories — Claude Code owns team lifecycle via `TeamCreate`.

1. **SessionStart hook** fires when Claude Code starts
2. Hook registers team info in `TeamRegistry` (in-memory, for delivery routing)
3. Hook returns `additionalContext` instructing Claude to call `TeamCreate`
4. Claude Code's `TeamCreate` creates `~/.claude/teams/{name}/` with `config.json` and `inboxes/`
5. `teams_mailbox.rs` expects directories to exist; returns error if not (delivery helper falls back to Zellij)

Team name convention: `exo-{agentId}` (e.g., `exo-root`, `exo-main.feature-a`).
TL inbox name: `team-lead`.

## On-Disk Format (Claude Code Teams)

### Team config (`~/.claude/teams/{team}/config.json`)

```json
{
  "name": "exo-root",
  "description": "ExoMonad root team",
  "createdAt": 1708700000000,
  "leadAgentId": "team-lead@exo-root",
  "leadSessionId": "...",
  "members": [
    {
      "agentId": "team-lead@exo-root",
      "name": "team-lead",
      "agentType": "team-lead",
      "backendType": "in-process"
    },
    {
      "agentId": "rust-impl@exo-root",
      "name": "rust-impl",
      "agentType": "gemini-worker",
      "backendType": "exomonad",
      "tmuxPaneId": "synthetic"
    }
  ]
}
```

### Inbox files (`~/.claude/teams/{team}/inboxes/{agent}.json`)

JSON array of messages. Claude Code does read-modify-write with `.lock` sidecar.

```json
[
  {
    "from": "rust-impl",
    "text": "Task completed successfully.",
    "summary": "Agent completion: rust-impl",
    "timestamp": "2026-02-22T20:30:00.000Z",
    "color": "green",
    "read": false
  }
]
```

Required fields: `from`, `text`, `timestamp`, `read`.
Optional: `summary`, `color`.
Protocol messages (idle, shutdown) use stringified JSON inside `text`.

### Task files (`~/.claude/tasks/{team}/{id}.json`)

Individual JSON files per task. `.lock` and `.highwatermark` files in the directory.

## Message Flows

### Claude → Gemini (via InboxWatcher)

```
Claude calls SendMessage(recipient="rust-impl", content="...")
  → Claude Code writes to ~/.claude/teams/{team}/inboxes/rust-impl.json
  → ExoMonad InboxWatcher (inotify) detects write
  → Looks up rust-impl in synthetic member registry
  → Injects message text into Gemini's Zellij pane via plugin pipe
  → Deferred Enter keypress (~100ms) to avoid Ink paste problem
```

### Gemini → Claude (via delivery helper)

```
Gemini calls notify_parent(status="success", message="...")
  → ExoMonad resolves parent's team info from TeamRegistry
  → delivery::deliver_to_agent() tries Teams inbox first:
    → Appends to ~/.claude/teams/{team}/inboxes/team-lead.json
    → Claude Code polls the file and delivers as <teammate-message>
  → If Teams fails, falls back to Zellij injection into parent pane
```

## Implementation Status

| Component | Status | File |
|-----------|--------|------|
| Delivery helper (Teams + Zellij) | Built | `services/delivery.rs` |
| SyntheticMemberService | Built | `services/synthetic_members.rs` |
| InboxWatcher (inotify) | Built | `services/inbox_watcher.rs` |
| notify_parent → delivery helper | Built | `handlers/events.rs` |
| GitHub poller → delivery helper | Built | `services/github_poller.rs` |
| teams_mailbox (array append) | Built | `services/teams_mailbox.rs` |
| Push notification pipeline | Built | Zellij plugin + deferred Enter |
| SessionStart → TeamRegistry | Built | `handlers/session.rs` |

## What's Left

- **Lock coordination**: Our `teams_mailbox` uses atomic temp+rename but doesn't coordinate with Claude Code's `.lock` sidecar protocol. Risk of lost updates under high concurrency.
- **Inbox rotation**: Inbox files grow unboundedly (idle notifications every 2-4s). No compaction or rotation.
- **Permission cascade**: 3-tier model (agent < TL < human) for typed permission checking. Not yet built.
- **Zellij popup approval UI**: Human escalation via structured popup dialogs. Infrastructure exists but disabled (blocks WASM plugin lock).

## Consequences

- Claude agents use native `SendMessage` for ALL teammates — no tool shadowing
- ExoMonad is invisible transport between Claude Teams and Zellij
- Gemini workers appear as first-class teammates from Claude's perspective
- Filesystem is the message bus — simple, debuggable, no custom protocol
- inotify dependency (Linux-only, acceptable — ExoMonad is Linux-only via Zellij)
- ~100ms latency for Claude→Gemini messages (inotify debounce + injection delay)
