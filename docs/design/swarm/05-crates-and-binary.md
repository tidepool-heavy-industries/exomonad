# Crates & the One Binary

> **Status: mostly settled.** Crate names provisional; mode surface to firm up.

## Crate graph

```
exo-scry      — identity resolution from live OS state (EXISTS, validated)
exo-caps      — capability traits + domain newtypes (the seam)        [stub: 03]
   ↑    ↑
exo-policy  exo-runtime   — policy depends on caps; runtime depends on caps
   ↑    ↑                    policy MAY depend on runtime (IO escape hatch), but
   │    │                    caps is the preferred path — not a hard wall.
  exomonad (bin)          — links all; wires policy → runtime, serves rmcp,
                            self-IDs via exo-scry. The node.
```

| Crate | Holds | Status |
|---|---|---|
| `exo-scry` | pane/team/identity resolution | **exists** |
| `exo-caps` | capability traits, `Bus`, domain types | stub ([03](03-capabilities.md)) |
| `exo-policy` | tools/roles/phases/hooks/events | partial ([04](04-policy.md)) |
| `exo-runtime` | cap impls (git/gh/tmux/bus/spawner) | not started |
| `exomonad` (bin) | modes + DI + lifecycle | the target |

`teams-mcp` (exists) is the prototype of the node's outbound half; it folds into
the binary's node mode as the design lands.

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
