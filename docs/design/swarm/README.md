# Swarm-Sidecar Architecture

> **Status:** built and runnable behind `exomonad experimental`, beside current exomonad
> (non-destructive). `exo-scry` (team/identity resolution) underpins it; `teams-mcp` was
> the prototype testbed and has been removed now that its techniques live in
> `exo-node` (outbound rmcp) and `exo-scry` (inbox/teams). Sections below are marked
> **settled** or **stub**.

## The idea in one paragraph

Replace the central `exomonad serve` + Haskell-WASM + brittle team-create
interception with a **recursive swarm**: one `exomonad` binary, run in *node
mode*, attached to every agent (CC or gemini). Each node self-identifies from
live OS state, routes messages through a per-node append-only inbox on the
filesystem, and spawns its children as more instances of the same binary. There
is **no central server, no singleton, no registry that drifts.** The filesystem
is the bus; the process tree is the topology.

## What it replaces

| Today (central) | New (swarm) |
|---|---|
| `exomonad serve` daemon hosting WASM | per-node sidecar (same binary, node mode) |
| in-memory `TeamRegistry` (stale on restart) | identity derived/assigned per node, immutable |
| team-create *interception* kept in sync | **observe, don't intercept** — papers + live state |
| `notify_parent` via registry lookup | append to the parent's inbox (a path you hold) |
| Haskell-WASM DSL + ~37k LOC boundary | Rust policy lib behind a capability seam |

## Locked principles

- **Recursive: one binary, fractal.** Root spawns children; each child is another
  `exomonad` in node mode with its own sidecar. No special-cased layers.
- **No central authority.** No server, no singletons, no coordination mutex, no GC
  daemon. Every function decomposes to per-node local action or a
  filesystem-mediated primitive (locks, inboxes). See [singletons](07-open-questions.md).
- **Pane is the universal key.** A node is keyed by its tmux pane (`$TMUX_PANE`) —
  unique per agent even when CC co-locates several in one cwd, observable with
  zero team dependency, stable across session-id churn.
- **Teams is a delivery *nicety*, tmux-paste is the floor.** CC's structured
  `<teammate-message>` needs a team; when there isn't one, paste into the pane
  (the gemini path). Delivery always works, only degrades.
- **Capabilities preferred, IO as escape hatch.** The crate split is for ergonomics
  and reuse, *not* a sandbox. Policy *may* drop to raw IO; good capabilities make
  it rarely want to.
- **No HList, no macros, no god-trait.** Plain `Tool`s + hooks/events as functions
  **generic over the individual caps they need** (no `dyn Caps`) + hand-written
  per-role tables.
- **One shared `exomonad` binary** for everything (modes, not separate bins).
- **Reuse tested components, don't rewrite.** Adapt `exo-scry`, the tmux injection
  (works for Gemini today), CC-inbox delivery, and exomonad-core's git/github/tmux
  services + poller logic — greenfield only the genuinely-new pieces.
- **Use the type system fully.** Validated newtypes + enums (make illegal states
  unrepresentable); no `pub` fields on domain types; observe-don't-store.
- **Feature parity with native teammates.** A worker spawned via exomonad — any
  runtime (Gemini, Shoal) — is a *first-class teammate* with the same verbs as a
  native Claude Teams teammate (send, receive, shut down, share tasks). The only
  difference is the spawn path; the sidecar is the adapter that grants parity. This
  is why shutdown is "just a message" — parity means reusing the same mechanism, not
  a parallel one.

> An adversarial review pass (7 independent angles) has been folded in — see
> [07](07-open-questions.md) for the resulting decisions and remaining risks.

## Two kinds of state

1. **Type-1 — swarm state (identity/topology).** Immutable birth papers written
   once per node (root at `init`, children at spawn). The registry that *can't*
   drift. See [identity](01-identity.md).
2. **Type-2 — the messaging bus.** Per-node append-only ingestion inboxes on the
   filesystem; sidecars consume their own and route into their agent. See
   [bus & sidecar](02-bus-and-sidecar.md).

## Hard-won rationale (why, not just what)

- **The exomonad identity isn't observable.** role / parent / tree-position /
  agent-type are exomonad's own concepts — in no runtime's live state. So they
  must be *recorded* (papers); they can't be derived. Live derivation (`exo-scry`)
  recovers only runtime-native facts (pane, CC team), and only for CC.
- **CC's teammate state is bookkeeping that desyncs.** We observed phantom
  teammates, stale `isActive: true` after an unclean kill, an un-interruptible
  ghost spinner, and a frozen/phantom `CLAUDE_CODE_SESSION_ID`. Only
  *process observation* (pane alive + a Claude in it) tells the truth — which is
  the entire reason this design observes rather than trusts.
- **The crate boundary replaces the WASM boundary.** WASM physically prevented the
  policy layer from doing IO; a crate that doesn't link the runtime gets the same
  separation at zero runtime cost — except we keep IO as an escape hatch rather
  than a wall.

## File map

| File | Covers | Status |
|---|---|---|
| [01-identity.md](01-identity.md) | Type-1 papers, pane key, name/path/branch | settled |
| [02-bus-and-sidecar.md](02-bus-and-sidecar.md) | ingestion inboxes, two-loop sidecar, routing, delivery | settled |
| [03-capabilities.md](03-capabilities.md) | `exo-caps` traits, `Message`, `Bus`, `Spawner`, IO escape hatch | settled |
| [04-policy.md](04-policy.md) | tools / roles / hooks / events (no phases) | settled |
| [05-crates-and-binary.md](05-crates-and-binary.md) | crate graph, one binary, modes | settled |
| [06-migration.md](06-migration.md) | impl plan as a scaffold-fork-converge agent tree | settled (plan) |
| [07-open-questions.md](07-open-questions.md) | empirical unknowns + mechanical TODO | living |

**Architecture settled (01–05); build plan settled (06).** `06` decomposes the
implementation into the agent tree that builds it (Wave 0 foundation+spike → Wave 1
runtime caps → Wave 2 node → Wave 3 policy → Wave 4 cutover). The one empirical
unknown (CC multi-team behavior) is the Wave-0 spike with an explicit decision
rule, and the design is robust to either outcome. Remaining is purely build-time:
mechanical cap signatures + executing the waves.
