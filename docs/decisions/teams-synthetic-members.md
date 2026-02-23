# Claude Teams Synthetic Members

## Status

Implemented

## Context

ExoMonad orchestrates heterogeneous agent teams: Claude (expensive, capable) and Gemini (cheap, fast). Claude Code has native Teams support — `TeamCreate`, `SendMessage`, `TaskCreate` — with file-based transport at `~/.claude/teams/{team}/`.

The problem: Claude is trained to use `SendMessage` for teammate communication. Gemini workers don't speak Teams. We need Claude to communicate with Gemini workers without tool shadowing (competing tools that do similar things, forcing the agent to remember which to use).

## Decision

**Synthetic team members + filesystem watcher.**

ExoMonad registers Gemini workers as "synthetic" members in the Claude team's `config.json`. When Claude sends a `SendMessage` to a synthetic member, Claude Code writes to `inboxes/{member}.json` as usual. ExoMonad's serve process watches these inbox files (inotify) and routes messages to the Gemini worker's Zellij pane.

The reverse direction (Gemini→Claude) uses the unified delivery helper (`services/delivery.rs`): Teams inbox first, Zellij injection fallback.

### On-disk format

Team config (`~/.claude/teams/{team}/config.json`):
```json
{
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

Inbox file (`~/.claude/teams/{team}/inboxes/rust-impl.json`):
```json
[
  {
    "from": "team-lead",
    "text": "Switch to using BTreeMap instead of HashMap",
    "summary": "Change data structure",
    "timestamp": "2026-02-22T20:30:00Z",
    "color": "blue",
    "read": false
  }
]
```

### Message flow

```
Claude→Gemini:
  Claude calls SendMessage(recipient="rust-impl", content="...")
  → Claude Code writes to inboxes/rust-impl.json
  → ExoMonad InboxWatcher (inotify) detects write
  → Looks up rust-impl in synthetic member registry
  → Injects message text into Gemini's Zellij pane

Gemini→Claude:
  Gemini calls notify_parent(status="success", message="...")
  → delivery::deliver_to_agent() resolves parent's team info from TeamRegistry
  → Writes to inboxes/team-lead.json (or falls back to Zellij injection)
  → Claude Code polls the file and delivers as <teammate-message>
```

### Naming convention

- Team name: `exo-{birth-branch}` (e.g., `exo-root`, `exo-main.feature-a`)
- Claude members: real, managed by Claude Code
- Gemini members: synthetic, managed by ExoMonad
- Inbox name = agent slug (e.g., `rust-impl`, `haskell-impl`)

## Implementation

### Components

1. **SyntheticMemberService** (`services/synthetic_members.rs`) — Manages synthetic member entries in team config.json
   - `register_synthetic(team_name, member_name, agent_type)` — append to members array
   - `remove_synthetic(team_name, member_name)` — remove from members array

2. **InboxWatcher** (`services/inbox_watcher.rs`) — inotify-based service watching synthetic member inbox files
   - On write: parse new messages, route to Gemini pane via Zellij injection
   - Track read cursor (last seen array length) to only process new messages
   - Debounce: 100ms after inotify event before reading (atomic writes)

3. **Delivery helper** (`services/delivery.rs`) — Unified Teams+Zellij delivery for Gemini→Claude direction
   - Called by `notify_parent` (events.rs) and GitHub poller
   - Replaces the old `.exo/messages/` inbox system (deleted)

4. **teams_mailbox.rs** — Writes to Claude Code's Teams inbox format (JSON array, atomic temp+rename)

5. **spawn_workers/spawn_leaf_subtree** — After spawning a Gemini agent, registers it as a synthetic member in the parent's team

### What changes where

| Component | File(s) | Status |
|-----------|---------|--------|
| SyntheticMemberService | `services/synthetic_members.rs` | Built |
| InboxWatcher | `services/inbox_watcher.rs` | Built |
| Delivery helper | `services/delivery.rs` | Built |
| teams_mailbox | `services/teams_mailbox.rs` | Built |
| Register on spawn | `services/agent_control.rs` | Built |
| Wire into server | `exomonad/src/main.rs` | Built |

## Open Work

See [teams-roadmap.md](teams-roadmap.md) for consolidated open items (lock coordination, inbox compaction, version sensitivity, etc.).

## Consequences

- Claude agents use native `SendMessage` for ALL teammates — zero tool shadowing
- ExoMonad becomes invisible transport layer between Claude Teams and Zellij
- Gemini workers appear as first-class teammates from Claude's perspective
- Filesystem is the message bus — simple, debuggable, no custom protocol
- inotify adds a dependency on Linux (acceptable — ExoMonad is Linux-only via Zellij)
- Small latency (100ms debounce + inotify delay) for Claude→Gemini messages

## Alternatives Rejected

**A. MCP tool (`send_to_agent`)** — Tool shadowing problem. Claude is trained on `SendMessage` and will reach for it instinctively. Two competing tools = cognitive load + silent failures.

**B. PreToolUse hook intercept** — Block `SendMessage` for synthetic members, reroute via ExoMonad. Complex, requires spoofing tool results, fragile.

**C. Claude adapter per Gemini worker** — Spawn a tiny Claude that bridges Teams↔Zellij. Works but absurdly expensive in Claude tokens.
