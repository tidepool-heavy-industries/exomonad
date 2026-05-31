# Crates & the One Binary

> **Status: mostly settled.** Crate names provisional; mode surface to firm up.

## Crate graph

```
exo-scry      — identity resolution from live OS state (EXISTS, validated)
exo-mailbox   — the durable file-MPSC queue primitive (append/consume/commit)
   ↑    ↑       no exo-specific deps; the one systems-heavy piece, tested in isolation
exo-caps      — capability traits + domain newtypes (the seam)        [stub: 03]
   ↑    ↑
exo-policy  exo-runtime   — policy depends on caps; runtime depends on caps + exo-mailbox
   ↑    ↑                    policy MAY depend on runtime (IO escape hatch), but
   │    │                    caps is the preferred path — not a hard wall.
  exomonad (bin)          — links all; wires policy → runtime, serves rmcp,
                            self-IDs via exo-scry. The node.
```

| Crate | Holds | Status |
|---|---|---|
| `exo-scry` | pane/team/identity resolution | **exists** |
| `exo-mailbox` | durable file-MPSC queue: atomic append / byte-offset cursor / inotify / restart resume. `Bus` impl + inbound loop are thin adapters over it. No exo-specific deps. | not started ([02](02-bus-and-sidecar.md)) |
| `exo-caps` | capability traits, `Bus`, domain types | stub ([03](03-capabilities.md)) |
| `exo-policy` | tools/roles/hooks/events (no phases) | partial ([04](04-policy.md)) |
| `exo-runtime` | cap impls (git/gh/tmux/bus/spawner) over caps + exo-mailbox | not started |
| `exomonad` (bin) | modes + DI + lifecycle | the target |

`teams-mcp` (exists) is the prototype of the node's outbound half; it folds into
the binary's node mode as the design lands.

## Module layout (one concern per file)

Clean modules are a goal, not incidental — the swarm builds this in parallel leaves,
so non-overlapping files matter:

```
exo-caps/src/     bus.rs  spawner.rs  git.rs  github.rs  tmux.rs  fs.rs
                  process.rs  log.rs  kv.rs                  — one cap trait per file
                  types.rs                                   — domain newtypes + NodeKind/Persona/…
exo-policy/src/   tools/{file_pr,merge_pr,spawn,messaging,tasks}.rs  — one tool (type+Args+adapter) per file
                  hooks.rs   — pre_tool_use / stop / session_start (shared fns roles point at)
                  events.rs  — on_world_event + WorldEvent
                  roles.rs   — RoleDef + role_def(NodeKind) table (the lightweight specs)
exo-runtime/src/  runtime.rs — the `Runtime` struct (holds EffectContext: agent identity)
                  git.rs github.rs tmux.rs bus.rs spawner.rs …  — `impl <Cap> for Runtime`, one per file
exomonad/src/     main.rs + mode handlers (init / mcp-stdio sidecar / hook / probe-team)
```

`Runtime` is one struct implementing ~10 cap traits, but each `impl` lives in its own
file — modular by trait even though it's a single concrete type (the `R` policy
monomorphizes against).

## One binary, several modes

`exomonad <mode>` — not separate binaries:

| Mode | Role |
|---|---|
| `init` | bootstrap the **root** node: write root papers, tmux session, launch root agent |
| `mcp-stdio` (node / sidecar) | the per-node sidecar: self-ID, serve role tools (outbound), watch ingestion inbox (inbound) |
| `hook …` | hook handler (pre-tool-use / stop / session-start), if retained |
| `probe-team` | the `exo-scry` diagnostic |
| `serve` | **the old central server — stays during transition**, retired once tools port |

## Recursion

The binary is fractal. A `spawn` tool (via the `Spawner` cap) births a child by:
`git worktree add` → `tmux new-pane` → write child papers (incl. `parent_inbox`) →
launch `exomonad mcp-stdio` in the pane. The child boots and self-IDs. Same binary,
one level down. The process tree *is* the agent tree.

> Why one binary: partly architectural (self-similar nodes), partly pragmatic — we
> dogfood the *current* exomonad tree to build this in-repo, so it all lives
> together.
