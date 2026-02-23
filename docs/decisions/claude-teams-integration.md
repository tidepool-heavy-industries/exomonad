# Claude Teams Integration

**Status:** In Progress

## Decision

ExoMonad bridges Claude Code Teams and Gemini workers so that Claude agents use native `SendMessage` for ALL teammates — zero tool shadowing. Gemini workers appear as first-class team members from Claude's perspective.

Claude Code Teams provides the orchestration UX (task lists, messaging, idle/shutdown). ExoMonad provides the infrastructure (heterogeneous agent support, typed effects, Zellij multiplexing, push notifications).

## Architecture

```
Claude Code Teams (UX layer)
    ├── Task list (shared, file-based)
    ├── SendMessage / broadcast
    ├── Idle / shutdown negotiation
    └── Plan approval flow

ExoMonad (infrastructure layer)
    ├── Synthetic member registration
    ├── Inbox file watcher (inotify)
    ├── Zellij STDIN injection (push notifications)
    ├── Heterogeneous agent support (Claude + Gemini)
    └── Hot-reloadable WASM policy
```

### Team Lifecycle

ExoMonad does NOT create team directories — Claude Code owns team lifecycle via `TeamCreate`.

1. **SessionStart hook** fires when Claude Code starts
2. Hook registers team info in `TeamRegistry` (in-memory, for `notify_parent` routing)
3. Hook returns `systemMessage` instructing Claude to call `TeamCreate`
4. Claude Code's `TeamCreate` creates `~/.claude/teams/{name}/` with `inboxes/` directory
5. `teams_mailbox.rs` expects directories to exist; returns error if not (callers fall back to Zellij injection)

Team name convention: `exo-{agentId}` (e.g., `exo-root`, `exo-main.feature-a`).
TL inbox name: `team-lead`.

## Message Flows

### Claude → Gemini

```
Claude calls SendMessage(recipient="rust-impl", content="...")
  → Claude Code writes to ~/.claude/teams/{team}/inboxes/rust-impl.json
  → ExoMonad inotify watcher detects write
  → Looks up rust-impl in synthetic member registry
  → Injects message text into Gemini's Zellij pane via plugin pipe
  → Deferred Enter keypress (~100ms) to avoid Ink paste problem
```

### Gemini → Claude

```
Gemini calls notify_parent(status="success", message="...")
  → ExoMonad resolves parent's team info from birth-branch hierarchy
  → Appends message to ~/.claude/teams/{team}/inboxes/team-lead.json
  → Claude Code's native file watcher delivers it as <teammate-message>
```

### Synthetic Members

ExoMonad registers Gemini workers as "synthetic" members in the Claude team's `config.json`:

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

Team name convention: `exo-{birth-branch}` (e.g., `exo-root`, `exo-main.feature-a`).

## Push Notification Pipeline

The TL does not poll. It spawns workers, finishes its turn, and gets poked by the system when events occur.

1. TL spawns workers and **returns** (no blocking wait)
2. Each worker gets `EXOMONAD_SESSION_ID` env var (parent's birth-branch)
3. When a worker completes, it calls `notify_parent`
4. Server resolves parent's Zellij pane from caller identity
5. Server injects `[CHILD COMPLETE: agent-id] message` into parent's pane
6. TL sees injected text as a new user message and wakes up

The injection uses the Zellij plugin pipe with deferred Enter keypress to work around the Ink paste problem (see `event-delivery-zellij-stdin`).

## Implementation Status

| Component | Status |
|-----------|--------|
| SyntheticMemberService | Built |
| InboxWatcher (inotify) | Built |
| notify_parent → Teams inbox | Built |
| SendMessage → Zellij injection | Built |
| Push notification pipeline | Built |
| Zellij STDIN injection + Ink paste fix | Built |
| GitHub poller (Copilot review → notifications) | Built |

## What's Left

- **Permission cascade**: 3-tier model (agent < TL < human) for typed permission checking. Not yet built.
- **Gemini pretool hooks**: Gemini's pretool hook → ExoMonad policy layer. Not yet built.
- **Zellij popup approval UI**: Human escalation via structured popup dialogs. Infrastructure exists but disabled (blocks WASM plugin lock).
- **End-to-end validation**: Verify full flow from SessionStart → TeamCreate → spawn workers → notify_parent → inbox delivery.

## Consequences

- Claude agents use native `SendMessage` for ALL teammates — no tool shadowing
- ExoMonad is invisible transport between Claude Teams and Zellij
- Gemini workers appear as first-class teammates from Claude's perspective
- Filesystem is the message bus — simple, debuggable, no custom protocol
- inotify dependency (Linux-only, acceptable — ExoMonad is Linux-only via Zellij)
- ~100ms latency for Claude→Gemini messages (inotify debounce + injection delay)
