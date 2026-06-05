# exo-scry — derive a Claude Code session's active team from live OS state

**Zero registration.** Given a process (or its own pid), figure out which Claude Code Teams team that session leads — by *observing* live OS state (process tree, kernel inotify bookkeeping, on-disk team configs), never by reading a registry. An observed team can't go stale, can't be lost on restart, and can't collide across sessions (the PID anchors it).

This is what makes native `<teammate-message>` delivery work in the node-mode swarm: `exo-node`'s `dispatch` calls `resolve_self()` to find the team whose lead inbox it should write. Also ships a small `exo-scry` CLI (`src/bin/`).

> Part of the v2 node-mode swarm. See `rust/CLAUDE.md`.

## Why observation, not a registry

CC's own teammate bookkeeping desyncs — we observed phantom teammates, stale `isActive: true` after an unclean kill, ghost spinners, and a frozen `CLAUDE_CODE_SESSION_ID`. Only **process observation** (pane alive + a Claude in it) tells the truth. And a third party's session UUID is *not observable* on Linux (absent from its fds/env/watches), so resolving someone else's team requires the inotify path, not a UUID scan.

## Resolution paths (public API in `lib.rs`)

| Fn | Mechanism | Use |
|----|-----------|-----|
| `resolve_self()` | inotify, Linux | **The sidecar's entry point** — its own team. Walks self→parent `claude`, reads its inotify-watched `tasks/{team}` dir. Robust when sessions share a cwd. |
| `resolve_active_team(target)` | inotify, Linux | Same, for an arbitrary `ProbeTarget` (pid / tmux pane / self). The only way to resolve a *third party's* team. |
| `resolve_by_session(uuid)` | config scan, portable | Match a known session UUID against team configs' `leadSessionId`. For self/sidecar contexts where CC hands the process its own `session_id`; works off-Linux. |
| `resolve_via_transcript(target)` | cwd→transcript, portable-ish | Find the session's newest transcript in its cwd's project dir → UUID → team. Fails loud (`AmbiguousCwd`) if multiple live Claudes share the cwd. |

`ActiveTeam { team, tasks_dir, lead_inbox, lead_session_id, me, claude_pid }` is the resolved result; `lead_inbox` is the routing target `dispatch` writes.

## Layout

| Module | Role |
|--------|------|
| `resolve` | Orchestration: `ProbeTarget` → Claude pid → active-team dir → `ActiveTeam`. |
| `signal` | `ActiveTeamSignal` strategy trait + `InotifyWatchSignal`. |
| `proc` (Linux) | Process-tree walking (`find_claude_ancestor`/`_descendant`, `self_pid`, cwd reads via `/proc`). |
| `inotify` / `pathmap` (Linux) | Read `/proc/{pid}/fdinfo` inotify watches → watched paths. |
| `teams` | On-disk team config discovery/loading (`tasks_root`, `find_team_by_session`). |
| `inbox` | `send_message(team, to, from, text, summary)` — write a CC Teams inbox line (the native-delivery write). |
| `transcript` | cwd → project dir → newest session UUID. |
| `identity` `target` `tmux` `error` | `ActiveTeam`/`Pid`/`TeamName`; `ProbeTarget`; pane→pid; `ScryError`. |

## Gaps / not-yet

- The inotify path is **Linux-only**; the portable fallbacks (`resolve_by_session`, `resolve_via_transcript`) exist but the node's `dispatch` only wires `resolve_self` (Linux), so non-Linux native delivery is untested.
