# Identity & Type-1 Papers

> **Status: settled.**

A node's identity has three layers, in increasing specificity:

| Layer | Source | Present for | Used for |
|---|---|---|---|
| **Pane** (universal key) | `$TMUX_PANE` (env) | any claude/gemini + sidecar | keying the inbox, self-ID |
| **CC membership** (team, member) | `exo-scry`: pane → Teams config | any CC agent in a team | nice Teams delivery |
| **Type-1 papers** (tree) | written by parent at spawn | exomonad-spawned nodes only | role/parent/routing |

The pane is the floor: it's unique per agent (even when CC co-locates several in
one cwd), observable with **zero team dependency**, and stable across session-id
churn (unlike `CLAUDE_CODE_SESSION_ID`, which we observed go phantom). Everything
else enriches it.

## Assigned-at-birth, not derived

The exomonad identity — `role`, `parent`, tree position, `agent_type` — exists in
no runtime's live state, so it is **recorded at spawn**, not inferred. The parent
already knows the child's identity (it assigned it); it just writes it down.

This is *not* the mutable registry we killed. The distinction is **mutable vs
immutable**, not written-vs-derived: birth facts never change after birth, so
frozen-at-spawn is correct for them. (The `CLAUDE_CODE_SESSION_ID` bug was a value
that *did* change being treated as fixed — the opposite case.)

`exo-scry`'s live derivation remains for: bootstrapping the **root** (un-parented,
so no one assigned it), resolving **CC membership** for delivery, and third-party
probing/debug.

## Papers shape (Type-1)

Per node, written by the parent at spawn (by `init` for the root). Proposed:

```jsonc
// {cwd}/.exo/node.json
{
  "path":       ["dev", "auth-claude", "oauth-gemini"], // tree address (list of segments)
  "branch":     "dev.auth-claude.oauth-gemini",          // git branch — SEPARATE, generated safely
  "role":       "dev",                                    // root | tl | dev | worker
  "agent_type": "gemini",                                 // claude | gemini | shoal
  "pane":       "%317",                                   // tmux pane (delivery + key derivation)
  "parent_inbox": "/…/auth-claude/.exo/inbox/pane-311.jsonl" // path to parent's ingestion inbox; null for root
}
```

- **`path` is a list, not a dot-string.** Branch names can contain `.`, so a joined
  string can't be round-tripped to segments. `AgentName = path.last()`,
  `parent.path = path[..len-1]` (the tree is prefix containment).
- **`branch` is decoupled** from `path` — git's concern, generated safely (so a `.`
  in a segment can't corrupt it).
- **`parent_inbox`** is a direct path to the parent's ingestion inbox (see
  [bus](02-bus-and-sidecar.md)). Whoever spawns you sets it — so the tree extends
  correctly even for spawns exomonad didn't plan (CC-native sub-claudes).

## Self-identification

A node's sidecar identifies itself by:
1. `$TMUX_PANE` → the **key** (always available, team-free).
2. Its own `{cwd}/.exo/node.json` (or `--agent-id/--role` flags passed at spawn) →
   the **enrichment** (role/parent/tree), if exomonad-spawned.
3. `exo-scry` pane → Teams config → **CC membership** (team, member name), if the
   agent is in a CC team — needed only to choose the nice delivery path.

A CC-Teams-spawned sub-claude with no papers is still a valid node: it has a pane
(key) and a CC membership (delivery); it just lacks tree-enrichment, which is fine.

## Location

Per-worktree (`{cwd}/.exo/node.json`) — co-located with the node, so deleting a
worktree GCs its papers for free; no central registry to drift. Enumeration of all
nodes = glob `.exo/worktrees/*/.exo/node.json` (∪ the root's at the repo root); the
worktrees directory *is* the registry.
