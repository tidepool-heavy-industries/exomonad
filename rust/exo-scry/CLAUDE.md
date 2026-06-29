# exo-scry — derive a Claude Code session's active team from live OS state

**Zero registration.** Given a process (or its own pid), figure out which Claude Code Teams team that session leads — by *observing* live OS state (process tree, kernel inotify bookkeeping, on-disk team configs), never by reading a registry. An observed team can't go stale, can't be lost on restart, and can't collide across sessions (the PID anchors it).

Used by the runtime's **`fork_session`** path (`exo-runtime/src/spawner.rs`): when a child is spawned with context inheritance, `resolve_self_or_portable()` finds the parent session's UUID so the child can launch with `--resume <uuid> --fork-session`. Also ships a small `exo-scry` CLI (`src/bin/`).

> **Note:** `exo-scry` no longer drives message delivery. The node's last hop is tmux-paste (see `exo-node/CLAUDE.md` § Delivery); CC Agent Teams native delivery was retired (a solo session-lead never drains its teammate inbox as of CC 2.1.178). The team-resolution machinery here remains only for `fork_session`'s session-UUID lookup.

> Part of the v2 node-mode swarm. See `rust/CLAUDE.md`.

## Why observation, not a registry

CC's own teammate bookkeeping desyncs — we observed phantom teammates, stale `isActive: true` after an unclean kill, ghost spinners, and a frozen `CLAUDE_CODE_SESSION_ID`. Only **process observation** (pane alive + a Claude in it) tells the truth. And a third party's session UUID is *not observable* on Linux (absent from its fds/env/watches), so resolving someone else's team requires the inotify path, not a UUID scan.

## Resolution paths (public API in `lib.rs`)

| Fn | Mechanism | Use |
|----|-----------|-----|
| `resolve_self()` | inotify, Linux | **The sidecar's entry point** — its own team. Walks self→parent `claude`, reads its inotify-watched `tasks/{team}` dir. Robust when sessions share a cwd. |
| `resolve_active_team(target)` | inotify, Linux | Same, for an arbitrary `ProbeTarget` (pid / tmux pane / self). The only way to resolve a *third party's* team. |
| `resolve_by_session(uuid)` | config scan, portable | Match a known session UUID against team configs' `leadSessionId`. For self/sidecar contexts (portable); works off-Linux. |
| `resolve_via_transcript(target)` | cwd→transcript, portable-ish | Find the session's newest transcript in its cwd's project dir → UUID → team. Fails loud (`AmbiguousCwd`) if multiple live Claudes share the cwd. |
| `resolve_self_or_portable()` | inotify → cwd→transcript | **The `fork_session` entry point** (`spawner.rs`). `resolve_self()` first (primary); on no-team/transient-error, falls back to the portable `resolve_via_transcript(SelfProcess)` (which ends in `resolve_by_session`). Off-Linux the portable cwd reader is absent, so it yields `None` (context inheritance is skipped; the child launches fresh). |

`ActiveTeam { team, tasks_dir, lead_inbox, lead_session_id, me, claude_pid }` is the resolved result; `fork_session` reads `lead_session_id` (the parent session's UUID) for `--resume`.

## Layout

| Module | Role |
|--------|------|
| `resolve` | Orchestration: `ProbeTarget` → Claude pid → active-team dir → `ActiveTeam`. |
| `signal` | `ActiveTeamSignal` strategy trait + `InotifyWatchSignal`. |
| `proc` (Linux) | Process-tree walking (`find_claude_ancestor`/`_descendant`, `self_pid`, cwd reads via `/proc`). |
| `inotify` / `pathmap` (Linux) | Read `/proc/{pid}/fdinfo` inotify watches → watched paths. |
| `teams` | On-disk team config discovery/loading (`tasks_root`, `find_team_by_session`). |
| `transcript` | cwd → project dir → newest session UUID. |
| `identity` `target` `tmux` `error` | `ActiveTeam`/`Pid`/`TeamName`; `ProbeTarget`; pane→pid; `ScryError`. |

## Gaps / not-yet

- `ActiveTeam.me` is currently always `None`; it remains in the type for a future teammate-self-resolution path but is presently vestigial.
- The inotify path is **Linux-only**. `fork_session` resolves via `resolve_self_or_portable()`, which falls back to the portable `resolve_via_transcript` path (→ `resolve_by_session`) when the inotify signal finds nothing or errors transiently. Off-Linux the portable cwd reader is absent, so `resolve_self_or_portable()` yields `None` and the spawned child simply launches without context inheritance — a clean degrade (no delivery depends on this anymore).
