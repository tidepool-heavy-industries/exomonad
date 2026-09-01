# Prompt Authority and Model UX — Next Iteration Handoff

**Status:** Draft implementation brief, distilled from the first successful Codex/Exomonad loop

## Goal

Make Exomonad teach models how to *see and think as a node* without turning the highest-authority
prompt channel into an object-level workflow script.

The developer instruction should establish role, responsibility, judgment, and the few durable
invariants of the tree. A spawn prompt should carry information about one concrete task. Tool
descriptions should explain mechanics. Hard guarantees should live in code and gates where possible.

The first iteration should optimize a small, reliable loop rather than hyper-parallel execution:

```text
parent plans and scaffolds
  -> one or a few fresh child contexts implement focused slices
  -> parent reviews, folds, integrates, and continues
```

Once this loop produces good behavior consistently, larger Terra swarms and richer reviewer flows
can build on it.

## Product model agreed in the design session

### A node manages a branch and a scope

Root and TL nodes are primarily managers of vision, decomposition, branch state, coordination, and
integration. They preserve context for those responsibilities. They are not prohibited from reading,
investigating, or implementing directly; they use judgment.

Delegation is valuable when the work can be serialized into a useful handoff. Size alone is not the
decision rule. A tiny but easily described edit may be worth delegating, while a parent may need to
work directly long enough to discover the right boundaries for a poorly understood problem.

Explicit human or parent steering wins over workflow preferences. In particular, instructions such
as "do this yourself" or "no agents" must not conflict with an injected categorical delegation rule.

### Scaffold commits distill understanding into the next context

Each level turns what it has learned into repository artifacts before forking when that improves the
handoff. A scaffold commit may contain interfaces, types, fixtures, placeholders, failing tests, and
local instructions for the fresh contexts that inherit it.

Prefer executable or inline prompts close to the relevant code, for example:

```rust
todo!("implement parser recovery using the surrounding ErrorKind contract; preserve offsets")
```

These are instructions to the next context, not shameful residue. A scaffold checkpoint is an
internal decomposition artifact and need not represent a finished or globally green branch. Use
compilable placeholders when practical, but do not make compilation a precondition for creating a
useful scaffold commit or spawning from it.

Children complete their assigned slices. A child branch can legitimately retain sibling TODOs that
were present in the shared scaffold; that does not make the child's own assignment incomplete. The
parent folds sibling results, resolves their interactions, and restores the intended wave-level
coherence.

### Submission retains a strong meaning

Do **not** add a partial submission state or a `partial` flag to `submit_branch`.

Partiality belongs inside a parent's in-progress branch while it scaffolds and coordinates a wave.
`submit_branch` means the node has completed its assigned subtree and is offering a merge-ready
result upward. It should normally compile and satisfy its declared verification. Existing explicit
deviation receipts can describe unavoidable departures, but do not bypass submission preconditions;
they must not quietly redefine READY as "checkpoint only."

### Fresh contexts are part of the review architecture

Context isolation is a feature. The parent distills intent into the scaffold and task information; a
fresh child re-derives the implementation from those artifacts; the parent then sees the result
again at fold time. Each level is another independent pass over the work.

Keep context inheritance opt-in. Do not assume a child needs the parent's transcript. The desired
default is to transfer distilled wisdom, not accumulated conversation noise.

### Reviewers are optional in this iteration

The reliable baseline is:

```text
scaffold -> child implementation -> submit -> parent review and fold
```

`review_enabled` remains off by default. A spawn may opt a risky child into review, and
`request_review` may enable it later. Do not redesign the reviewer lifecycle in this pass. In
particular, do not make prompt or tool text promise that every submission receives a one-shot
reviewer when the gate is configuration-dependent.

Reviewers may later become a deliberate context-preserving convergence layer. That design should be
informed by real traces from the simpler parent-review loop.

## Authority model

| Surface | It should contain | It should not contain |
|---|---|---|
| Developer instruction | Identity, role, way of seeing the tree, responsibility, judgment, durable lifecycle/worktree invariants | One task's paths, exact steps, mandatory fan-out, cost-shaming, categorical bans on exploration or implementation |
| Spawn prompt | Objective, relevant context, intended outcome, scope, constraints, useful starting points, verification information | A second persona, repeated role protocol, generic orchestration doctrine |
| MCP tool description/schema | What the operation does, preconditions, important effects, argument meaning, result meaning | Global behavioral policy, model-cost rhetoric, unconditional idle instructions, backend-specific model names |
| Repository guidance | Project facts, architecture, local commands and conventions | Claims about live tool behavior that can silently drift from the runtime |
| Mechanical gates | Clean-tree, boundary, lifecycle, and fold/reclaim invariants that truly must hold | Taste or workflow preferences disguised as enforcement |
| Messages | Live human/parent steering and new information | Attempts to override actual safety boundaries without explicit authority |

The distinction matters especially for Codex: injected `developer_instructions` survive compaction and
outrank user messages. Soft workflow preferences written as absolutes in that channel become hard to
override during a live session.

## Proposed role charter style

The implementation may improve the exact prose, but preserve this level of abstraction.

### Shared manager charter for root and TL

> You own the outcome within your scope and manage this branch as its coordinating agent. Use your
> judgment to plan, investigate, work directly, scaffold, delegate, review, and integrate.
>
> Preserve context for vision and coordination when work can be clearly handed off. Scaffolding
> commits distill your current understanding for fresh child contexts; they may contain explicit
> `todo!("next action")` placeholders and need not represent a finished state.
>
> Coordinate with children as needed, incorporate their results, and verify the branch to the depth
> appropriate for its current lifecycle stage. Child events are pushed; do not repeatedly poll.
>
> Stay in your worktree and branch. Use Exomonad lifecycle tools for managed children. Explicit human
> or parent direction overrides workflow preferences; mechanical safety and fold constraints remain.

Root then gets one short role-specific sentence: it owns the user's intent, overall vision, and final
result. A spawned TL gets one short sentence: it owns a subtree, its children, integration on its
branch, and a complete submission upward.

### Leaf charters

- **Dev:** Own the assigned slice in this branch. Inspect the scaffold and task information, use
  judgment within scope, implement and verify the slice, commit it, and submit it when merge-ready.
  Ask the parent when resolving an ambiguity would change shared architecture or scope.
- **Worker:** Perform the bounded task in the parent's worktree and report the useful result. Its
  isolation and commit restrictions are mechanics, not a general persona.
- **Reviewer:** When enabled, independently judge the submitted slice against its acceptance context
  and communicate a useful verdict. Keep the existing reviewer behavior otherwise out of scope.

Avoid cost-model prose such as "every line you implement is wasted". It produces pathological
delegation and makes tools themselves compete with live human steering.

## Spawn prompt shape

The spawn prompt is task information, not another developer instruction. A compact default shape is:

```text
OBJECTIVE
<what outcome this child owns>

CONTEXT
<only decisions or facts not already legible from the scaffold/repository>

DONE WHEN
- <observable outcomes>

SCOPE
- <mechanically checked paths when available>

READ FIRST
- <small set of high-value anchors, when useful>

CONSTRAINTS
- <task-specific constraints, when useful>

VERIFY
- <relevant commands or observations>
```

All optional sections should disappear when empty. Steps are optional hints, not a ritual or a demand
for line-by-line implementation. The child's role charter already owns git, escalation, and handoff,
so generic `EXECUTION CONTRACT` and `HANDOFF` boilerplate need not be repeated in every task.

The scaffold should usually be the richest context. The prompt should add what cannot be expressed
well in code, tests, types, or nearby TODOs.

## Tool-surface changes for this pass

Keep the current tools and lifecycle. Improve the language before redesigning the API.

1. Make spawn tool descriptions backend-neutral. Describe TL/dev/worker roles, not Claude, Opus, or
   Sonnet.
2. Remove global policy from tool descriptions:
   - `PREFER DELEGATING OVER DOING WORK YOURSELF`
   - token-cost or "wasted budget" rhetoric
   - unconditional "return immediately" / "idle and wait"
3. State push delivery mechanically: child events are pushed, so repeated status polling is
   unnecessary. Do not tell a parent to stop if it still has useful coordination or integration work.
4. Describe review conditionally. A child submits upward; review occurs only when enabled.
5. Keep the Codex routing distinction explicit: Exomonad orchestration uses
   `mcp__exomonad__*`, not native `collaboration.*`. Prefer role-specific discovery language derived
   from the actual roster rather than enumerating unavailable operations.
6. Keep `merge` as the managed fold/reclaim operation and preserve its hard safety language.

Do not unify the spawn tools, redesign receipts, change reviewer topology, or add structured MCP
outputs in this pass. Those remain worthwhile follow-ups after the prompt experiment produces data.

## Prompt provenance and stale-runtime problem

The first live Codex test exposed a real deployment failure mode:

- The checked-in protocol had been softened.
- The installed/running `exo` binary predated that commit.
- The running Codex command still carried the old categorical Root TL protocol.
- Compaction correctly preserved that old developer message.
- The live deferred MCP inventory likewise exposed descriptions from the older binary.

Prompt changes are therefore not validated merely because source tests pass. For this iteration:

1. Ensure Codex root and spawned nodes resolve the intended authoritative protocol consistently. The
   root currently constructs its identity directly from `exo::protocol::ROOT`, while spawned nodes
   have override-or-const resolution. Remove or deliberately document that asymmetry.
2. Make active prompt provenance visible enough to diagnose: at minimum log the binary version/commit
   and a role-protocol hash at launch. If a small read-only diagnostic fits naturally, expose the
   effective role/protocol source and hash; do not turn this into a prompt-management subsystem.
3. Treat install plus session recreate/resume as part of live verification. Confirm the new process's
   actual command/instructions and its live `tools/list`, not only the source renderer.

## Stale and conflicting surfaces to audit

Runtime behavior is the priority, but remove or quarantine model-facing guidance that still teaches
the Classic/Gemini/Copilot/PR workflow or categorical non-implementation. Known examples include:

- `.exo/rules/exomonad.md`
- `.claude/output-styles/tech-lead.md`
- `.claude/skills/tl-subagent-dispatch/SKILL.md`
- `.claude/skills/tl-sprint-planning/SKILL.md`

Determine whether each is active, generated, Classic-only, or obsolete. Do not blindly rewrite
Classic documentation as v2 documentation. The desired outcome is that an active model cannot load
two contradictory Exomonad operating doctrines without a clear architecture/version boundary.

Also update `docs/decisions/spec-commits.md`: retain its three-layer artifact model, but remove the
assumption that full conversation inheritance is the default. The scaffold plus compact task prompt
is the primary handoff; session forking is opt-in.

## First-pass implementation boundaries

Likely authoritative files include:

- `rust/exo/src/protocol.rs`
- `rust/exo/src/spawn.rs`
- `rust/exo/src/tools/spawn.rs`
- `rust/exo/src/init.rs`
- `rust/exo-runtime/src/codex.rs`
- `rust/exo-runtime/src/spawner.rs`
- `.exo/roles/devswarm/context/root.md`
- the active v2 rules/documentation identified by the audit

Read the nested `CLAUDE.md` files governing any touched paths. Preserve worktree isolation, clean
spawn gates, boundary enforcement, push delivery, managed merge/reclaim, and strong submission
semantics.

## Non-goals

- Hyper-parallel orchestration tuning or fixed fan-out formulas
- Automatic parsing or ownership validation of scaffold TODO markers
- A `partial` submission state
- Reviewer redesign, multiple reviewers, or reviewer/implementer chat topology
- Automatic parent review or a new `inspect_submission` tool
- Unifying `fork_wave`, `spawn_dev`, and `spawn_worker`
- Redesigning receipts
- MCP output-schema / `structuredContent` work
- Changing Classic behavior except to clearly separate stale Classic guidance from active v2 guidance

## Acceptance criteria

### Prompt behavior

- Root and TL protocols describe branch/scope management, vision, scaffolding, coordination,
  integration, and judgment.
- Explicit human/parent direction to work directly is compatible with the injected prompt.
- Delegation is encouraged through role responsibility and context preservation, not prohibition or
  cost-shaming.
- Root/TL may inspect enough context to find good serialization boundaries.
- Scaffold commits and embedded TODO prompts are described as legitimate decomposition artifacts;
  scaffold compilation is not stated as a spawn prerequisite.
- `submit_branch` remains complete/merge-ready; no partial flag is introduced.
- Push delivery is described without either polling or mandatory post-spawn idleness.
- Review is described conditionally and remains off by default.

### Surface coherence

- Developer instructions contain role-level thinking and invariants, not one task's details.
- Spawn prompts contain task information and do not repeat the role persona or generic handoff ritual.
- Tool descriptions are backend-neutral and mechanically descriptive.
- Active v2 surfaces contain none of the old categorical phrases such as `You do not implement`,
  `Never implement alone`, or `every line you implement ... is wasted`.
- Root and child prompt resolution have deliberate, tested provenance.
- Stale Classic-only guidance is clearly scoped or no longer auto-loadable as current v2 doctrine.

### Verification

- Add focused prompt-content tests asserting the semantic properties above without snapshotting every
  word.
- Test the rendered Codex developer instructions for root and at least one spawned role.
- Test the rendered task prompt with sparse and full structured input.
- Test relevant tool descriptions for backend neutrality and absence of global policy language.
- Run crate-scoped formatting and the narrow prompt/config/tool tests, then `git diff --check`.
- Build/install the changed binary and perform a fresh or recreated Codex smoke session. Inspect the
  live injected instructions and deferred Exomonad tool descriptions to prove the running surface is
  the one just built.

## Questions intentionally deferred until after the experiment

Collect live traces before deciding:

- Whether Exomonad tools should be surfaced directly in Codex instead of through deferred
  `functions.exec` discovery
- Whether spawn argument names and grouping should change
- Whether MCP outputs should expose typed `structuredContent`
- What work a future reviewer should own and whether reviewer iteration preserves parent context
- Whether scaffold TODOs need a standard marker or mechanical inventory
- What fan-out and model policy performs best at larger scale

The immediate hypothesis is deliberately simple: if the model clearly understands what it owns and
how its branch participates in the tree, it can exercise better judgment than a long imperative
workflow prompt permits.
