# Model-gradient forking: the monotone descent lattice (MG.1)

**Status:** accepted 2026-06-10 (design; decided in an interactive design review — two
interview rounds, decisions by the user). Implementation pending: runbook task **MG.1**,
wave 3 of [`plans/2026-06-orchestration-tree.md`](../../plans/2026-06-orchestration-tree.md)
(sequenced there because trust's T1.1 touches `spawn.rs` and T3.7 reshapes the `Tool` impls
this rides on). Mechanics compile-proven at the CLI level (see [Proven mechanics](#proven-mechanics)).

## Problem

Every node in the tree runs the same model — whatever the CLI default is. But the tree's
whole economics is an intelligence gradient: expensive context decomposes, cheap leaves
implement. Today that gradient is only expressible across the *provider* boundary
(Claude TLs / Gemini leaves, via the lossy `spawn_gemini` reseed). Within the Claude side
there is no gradient at all: a root running a frontier-tier model (fable-class) forks TLs at
the same tier, so either the whole tree runs at top-tier rates or the root manually downshifts
itself. The fable→sonnet gap — in cost especially — is too large to leave on the table.

What we want: a root that plans and scaffolds at the expensive tier, then **forks its context
window down-gradient** — Sonnet TL waves that inherit the full worked reasoning (the why, the
rejected alternatives, the escalation tripwires) at sonnet rates, and fork further at sonnet
with a warm cache.

## The design sentence

> **Capability only flows down; only messages flow up.** Fork is monotone non-increasing in
> model tier, enforced by construction at the tool boundary; escalation (`notify_parent`) is
> the sole up-channel. Cost is thereby bounded statically: no subtree can spawn cognition more
> expensive than its birth tier.

## Doctrine

- **Monotone descent lattice.** Model tiers form a total order (fable > opus > sonnet >
  haiku, as cost classes — the concrete model IDs behind each tier drift with releases and
  live in one place). A fork may only name a tier ≤ the parent's. There is **no upshift
  primitive of any kind** — no consult forks, no self-relaunch at a higher tier. A child that
  hits work above its grade escalates; the parent (up-gradient by construction) re-decomposes,
  absorbs it, or re-forks at a higher tier ≤ its own.
- **Cost bounded by construction.** A subtree's per-token rate is capped at its birth tier —
  statically, like a type system for spend. The maximal reasoner (fable-class) exists exactly
  once per tree: at the root, where the human is. It is not a forkable tier (see Decisions).
- **The schema is the policy surface.** Each node's `fork_wave` schema renders the `model`
  enum as the tiers ≤ its own — invalid choices are *unrepresentable to the child*, not
  rejected after the fact. (Backstopped at serde/runtime, since schemas are advisory to
  models.)
- **Work descends as forks; variance ascends as messages.** The expensive model handles
  variance (surprises, escalations, fold judgment); cheap models handle expectation (executing
  pre-compiled plans). Plans are compressions of expected futures; escalation is the variance
  channel; the gradient is priced accordingly.
- **Cache economics: switch at fan-outs.** The KV cache is keyed per model, so a cross-tier
  fork is cache-cold exactly once — a re-prefill of the inherited prefix at the *child* tier's
  input rates. That one cold prefill amortizes over the entire same-tier subtree below it
  (every further same-tier fork is warm). So the downshift belongs where the fan-out is: one
  prefill seeds the whole wave.
- **The preamble shifts genre: autobiography → testimony.** An inherited transcript untreated
  reads as the child's own prior turns — it inherits the parent's *confidence* without the
  *competence* that earned it. The injected transition preamble recasts the prefix as a
  stronger predecessor's worked notes and installs the asymmetric-trust stance: trust the
  plan above your own re-derivation (same information, stronger reasoner); trust your
  observations above the plan (you have information it didn't).

## Decisions

1. **North star: downshift at fan-out.** The design center is: expensive root/TL plans +
   scaffolds, then forks down-gradient TL waves with inherited context (`fork_session: true`
   + `model`). The full-gradient tree, strategy-as-artifact, and braid shapes are derivatives
   or out of scope (below).
2. **Model is birth-fixed identity.** Set at fork, immutable for the node's life — like
   birth-branch. A node's output is attributable to one model. The human-driven root remains
   `/model`-mutable de facto (human-supervised; the sidecar doesn't track it).
3. **No upshift, period.** Rejected: consult forks (context-inheriting up-tier advisors) and
   phase-mutable nodes (self-relaunch `--resume <own-session> --model X`). Rationale: cost
   overrun risk; escalation already exists and suffices. The fold happens at the parent,
   which is up-gradient by construction.
4. **Authority: parent-explicit, downshift-only enum, sonnet-anchored default.** Optional
   per-child `model` on `fork_wave`; absent ⇒ **`min(sonnet, parent_tier)`** (a haiku parent
   defaults haiku — the default can never be an accidental upshift). Spawning above sonnet is
   always deliberate.
5. **Fable is not a forkable tier.** The fork-args enum is `{opus, sonnet, haiku}` — fable is
   not a variant, so "fork a fable" is unrepresentable even for the root. Opus is the
   deliberate strong-TL tier for genuinely hard subtrees (near-frontier planning, much
   cheaper). Consequence: parallel fable-grade planning does not exist; hard-subtree planning
   either rides opus or stays in the root's window.
6. **Trust: the reviewer rides the gradient at the cheap tier.** Reviewers stay gemini- or
   sonnet-class (under the same monotone cap); the parent skims at merge — the existing
   scope-check discipline (`git diff --name-only` vs the charter boundary), not a formal
   review. No system-enforced floor; the gradient does the "expensive joints" work because
   parents are up-gradient from children by construction.
7. **Naming: tier does NOT enter the branch coordinate.** Suffix stays `-claude`
   (provider-typed); the tier is recorded in the agent's metadata (`.exo/agents/…`) and
   surfaced by `tree`/`list_agents`. Zero naming churn; the git DAG stays blind to the tier
   (accepted trade).
8. **Transition preamble: role-context template, auto-injected.** A
   `.exo/roles/devswarm/context/model-transition.md` template, interpolated with
   `{parent_model}`/`{child_model}` and prepended to the fork's prompt file by the spawner
   **iff `fork_session` is true and the tiers differ** (no inherited transcript ⇒ nothing to
   mis-attribute ⇒ no preamble). Hot-editable prompt engineering, no rebuild to iterate.
   Only a *downshift* preamble exists (no upshift ⇒ no upshift preamble). Draft content below.
9. **No auto-fallback on quota/unavailability.** A spawn whose model can't launch fails loud;
   the parent re-decides. Consistent with tools-require-wellformed-args and the quota-stall
   lessons. (A *distinct, loud* quota signal is separate work — T2.2.)
10. **Cross-provider stays outside the lattice.** Gemini has no Claude session to fork;
    `spawn_gemini`'s reseed path (self-contained spec) is unchanged. The `model` enum is
    Claude tiers only.

**Proposed, veto-able:** a new escalation tag **`[ABOVE-GRADE: reason]`** — the child judges
the task above its tier and stops *without burning retries*. Distinct from `[FAILED: id]`
(exhausted retries): the parent's response differs (re-fork up-tier ≤ its own / absorb /
re-decompose vs. re-spec).

## Out of scope (named, deliberately)

- **Strategy programs as first-class artifacts.** Under this north star, orchestration
  strategies (fork shapes, fold instructions, tripwires) ride as *prose in the inherited
  context* — authoring discipline, no new representation. Note the T3.7 authoring DSL is a
  different layer (tool/hook/role authoring in Rust, not runtime orchestration content) and
  does not carry this.
- **The braid** (phase-switching within one lineage) — exists only at the root, via the
  human's `/model`.
- **Per-node schema rendering** ships *after* T3.7's schema layer (the DSL derives schemas
  statically via `JsonSchema`; a per-node enum is a post-process at `tools/list` time off the
  node's own tier). v1 ships the static `{opus, sonnet, haiku}` enum + the runtime
  monotonicity check.

## Proven mechanics

CLI-level fork test, 2026-06-10: seed a session on haiku (plant a codeword), then
`claude --resume <sid> --fork-session --model claude-sonnet-4-6 -p "what's the codeword?"`:

- Context carried (codeword recalled) — the fork inherits the transcript.
- `modelUsage` shows **only** `claude-sonnet-4-6` — the override is honored on a forked resume.
- A new `session_id` is minted and returned in the JSON (what a spawner captures to chain).
- Cache telemetry confirms per-model keying: the fork's first turn shows ~18k
  `cache_creation` (the cold re-prefill, now sonnet-keyed); subsequent same-model forks read
  it warm.

## The preamble (draft v1 — lives at `.exo/roles/devswarm/context/model-transition.md`)

```markdown
You are running as {child_model}. The conversation you have inherited was produced by
{parent_model} — a stronger reasoner. You were forked, not resumed: the prior turns are a
predecessor's worked notes, not your own commitments.

Calibrate accordingly:
- Trust the inherited plan above your own re-derivation — it was made with the same
  information by a stronger reasoner. Do not re-litigate settled decisions.
- Trust your own observations above the plan — you have information it didn't. When reality
  contradicts the plan, the plan is wrong; say so rather than bending your observations.
- The plan's escalation tripwires are binding commitments. When you hit one — or anything you
  judge above your grade — stop and notify_parent with [ABOVE-GRADE: reason] rather than
  improvising past it.
```

## Implementation map (MG.1)

- **`rust/exo-caps`** — `ModelTier` enum `{Opus, Sonnet, Haiku}`: `Ord` (descending
  capability), `Display`/serde (lowercase tier aliases — the Claude CLI accepts them as
  `--model` values), and the one place the tier→concrete-ID note lives.
  `SpawnSpec::model(&self) -> Option<ModelTier>` (default `None`).
- **`rust/exo/src/tools/spawn.rs`** — `ForkChildArgs.model: Option<ModelTier>` (doc comment
  becomes the schema description: states the monotone rule and the `min(sonnet, parent)`
  default); threaded through `ForkWave::run`.
- **`rust/exo/src/spawn.rs`** — `ExoSpawn.model` + the `SpawnSpec` impl.
- **`rust/exo-runtime/src/spawner.rs`** — resolve the default (`min(Sonnet, own_tier)`);
  enforce monotonicity (`child ≤ own_tier`; the root counts as ⊤) — violation is a loud
  `SpawnError`, never a clamp; record the tier into the child's agent metadata; prepend the
  interpolated transition preamble to the prompt-file content when `fork_session && child !=
  parent`; pass the tier into `ClaudeSpawnFlags`.
- **`rust/exomonad-shared/src/services/agent_control/types.rs`** —
  `ClaudeSpawnFlags.model: Option<String>` (stringly at the CLI boundary is correct — it is
  literally a flag). **`launch.rs::build_agent_command`** — Claude arms (both the
  fork-session arm and the plain arm) append ` --model <tier>`.
- **`tree` / `list_agents`** — render the tier from metadata.
- **Node's own tier** — carried to the sidecar at birth through the same channel as
  role/parent identity (exact carrier — env vs. agent metadata read — decided at impl time);
  absent ⇒ the node treats itself as sonnet for the monotonicity check (conservative).
- **Tests** — launch.rs unit (`--model` flag present/absent); serde rejection of `"fable"`
  (unknown variant fails loud); monotonicity check unit (sonnet parent forking opus ⇒ error);
  preamble-prepend unit (fires iff fork_session && tier differs).

## Validation (post-deploy)

Live probe: root forks one sonnet child with `fork_session: true` and a task whose inherited
context includes settled plan content. Confirm: child's `modelUsage` is sonnet-only; the
preamble is the prompt file's head; a sonnet child attempting `model: "opus"` fails loud at
spawn. (The CLI-level mechanics are already proven; this validates the threading.)

## Open questions

- **Haiku forks** — a context-inheriting haiku is a new capability class (mechanical
  multi-file edits with full plan context, vs. today's Gemini reseed leaves). Use cases TBD;
  the tier is in the enum from day one.
- **Per-node schema rendering** — mechanics once T3.7's `Adapter` owns `schema()`: post-process
  at `tools/list` off the node's tier.
- **Tier-order maintenance** — the order is by cost class; as generations shift (a future
  sonnet may beat an old opus), the ordering is revisited in `exo-caps` only.
- **`fork_session` default under downshift** — kept orthogonal (still default-false). If
  downshift-with-inheritance becomes the dominant TL pattern, revisit.
- **Reviewer-model surface** — today the reviewer tier is fixed config; whether a charter can
  choose it per-subtree is deferred.
