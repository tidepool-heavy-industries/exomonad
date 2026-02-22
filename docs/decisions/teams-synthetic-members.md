# Claude Teams Synthetic Members

## Status

Accepted

## Context

ExoMonad orchestrates heterogeneous agent teams: Claude (expensive, capable) and Gemini (cheap, fast). Claude Code now has native Teams support — `TeamCreate`, `SendMessage`, `TaskCreate` — with file-based transport at `~/.claude/teams/{team}/`.

The problem: Claude is trained to use `SendMessage` for teammate communication. Gemini workers don't speak Teams. We need Claude to communicate with Gemini workers without tool shadowing (competing tools that do similar things, forcing the agent to remember which to use).

## Decision

**Synthetic team members + filesystem watcher.**

ExoMonad registers Gemini workers as "synthetic" members in the Claude team's `config.json`. When Claude sends a `SendMessage` to a synthetic member, Claude Code writes to `inboxes/{member}.json` as usual. ExoMonad's serve process watches these inbox files (inotify) and routes messages to the Gemini worker's Zellij pane.

The reverse direction (Gemini→Claude) already works: `notify_parent` writes to the Claude's Teams inbox.

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
    "read": false
  }
]
```

### Message flow

```
Claude→Gemini:
  Claude calls SendMessage(recipient="rust-impl", content="...")
  → Claude Code writes to inboxes/rust-impl.json
  → ExoMonad inotify watcher detects write
  → Looks up rust-impl in synthetic member registry
  → Injects message text into Gemini's Zellij pane

Gemini→Claude:
  Gemini calls notify_parent(status="success", message="...")
  → ExoMonad resolves parent's team info from TeamRegistry
  → Appends message to inboxes/team-lead.json
  → Claude Code's native file watcher delivers it as <teammate-message>
```

### Naming convention

- Team name: `exo-{birth-branch}` (e.g., `exo-root`, `exo-main.feature-a`)
- Claude members: real, managed by Claude Code
- Gemini members: synthetic, managed by ExoMonad
- Inbox name = agent slug (e.g., `rust-impl`, `haskell-impl`)

## Implementation

### Components

1. **SyntheticMemberService** — Manages synthetic member entries in team config.json
   - `register_synthetic(team_name, member_name, agent_type)` — append to members array
   - `remove_synthetic(team_name, member_name)` — remove from members array

2. **InboxWatcher** — inotify-based service watching synthetic member inbox files
   - On write: parse new messages, route to Gemini pane via Zellij injection
   - Track read cursor (last seen array length) to only process new messages
   - Debounce: 100ms after inotify event before reading (atomic writes)

3. **teams_mailbox.rs fixes** — Current implementation writes a single JSON object; needs to append to the JSON array format that Claude Code uses.

4. **spawn_workers/spawn_leaf_subtree changes** — After spawning a Gemini agent, register it as a synthetic member in the parent's team.

5. **Spawn prompt for Claude subtrees** — Include instruction to create a team named `exo-{slug}` on startup.

### What changes where

| Component | File(s) | Change |
|-----------|---------|--------|
| SyntheticMemberService | `rust/exomonad-core/src/services/synthetic_members.rs` | New service |
| InboxWatcher | `rust/exomonad-core/src/services/inbox_watcher.rs` | New service (inotify + tokio) |
| Fix inbox format | `rust/exomonad-core/src/services/teams_mailbox.rs` | Write array append, not single object |
| Register on spawn | `rust/exomonad-core/src/services/agent_control.rs` | After Gemini spawn, call SyntheticMemberService |
| Wire into server | `rust/exomonad/src/main.rs` | Start InboxWatcher, pass to services |

### Dependencies between components

```
Fix inbox format (independent)
SyntheticMemberService (independent)
InboxWatcher (depends on: SyntheticMemberService for knowing which inboxes to watch)
Register on spawn (depends on: SyntheticMemberService)
Wire into server (depends on: all above)
```

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
