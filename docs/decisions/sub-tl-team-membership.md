# Sub-TL Team Membership

**Status:** Proposed

## Question

When a TL calls `fork_wave` to spawn a Claude sub-TL, should the sub-TL:

- **(A) Create its own team** via `TeamCreate` (the current SessionStart hook nudges it to)
- **(B) Join the parent's team directly** — zero `TeamCreate`, zero team-per-subtree
- **(C) Own a subtree team** nested under the parent (distinct team, per-subtree task isolation)

## Context

The SessionStart hook currently prompts every spawned Claude agent with
"Create a team using TeamCreate before proceeding." This produced every
sub-TL creating its own team (e.g. `fixup-wave`, `messaging-tl`,
`cleanup-tl`, etc.) — one team per subtree.

In parallel, `AgentHandler::propagate_team_to_child` registers the sub-TL
in the **parent's** `TeamRegistry` under the child's identity keys, so
that when the sub-TL spawns workers/leaves, `register_synthetic_member`
writes them into the parent's `~/.claude/teams/{parent-team}/config.json`.

Net effect today: **two sources of truth**. The sub-TL owns a team
(from TeamCreate) AND is simultaneously a member of the parent's team
(from propagation). Messages sent to the sub-TL land in whichever lookup
wins — which is why the sub-TL-inbox bug (#864) required a targeted fix.

## Recommendation: Option B (flat team per tree)

One team per top-level root. All sub-TLs, leaves, and workers are
synthetic members of that single team. Sub-TLs do not call `TeamCreate`.

### Rationale

**The hylomorphic model is about context isolation, not team isolation.**
Each sub-TL has its own worktree, own context window, own branch — the
tree structure. The team is just a **messaging namespace**, and a flat
namespace per-tree is the simplest thing that works.

**Parent's team already works for everyone in the subtree.**
`propagate_team_to_child` registers the sub-TL in the parent's team;
`register_synthetic_member` writes grandchildren there too. Task lists
are shared. Names are unique via the `{slug}-{type}` convention. No
nesting needed.

**Two sources of truth is a bug farm.** When a sub-TL creates its own
team AND is registered in parent's team, any messaging code that
resolves by name might hit either registry and get a different answer.
#864 was one such bug; the `model: "gemini"` hardcode was another.
Collapsing to one source eliminates a class of drift bugs.

**The TeamCreate prompt is a footgun.** If a sub-TL forgets (or the
hook fails to fire), the tree still partly works — propagation saves
it — but messaging is subtly wrong. Silent partial failure is the worst
kind.

### Tradeoffs

- **Task list crowding at depth.** A 3-deep tree with 3-branch fanout has
  ~13 agents, all sharing one task list. Mitigation: structured task
  naming + ownership filtering in `task_list`. This is already
  conventional — the alternative is 13 separate task lists scattered
  across teams, which is worse for surface-level visibility.

- **Cross-subtree noise.** Gemini in subtree A sees tasks for subtree B.
  Mostly harmless: agents filter by `owner`. If it becomes painful,
  add team-scoped task views without changing team topology.

### Implementation

1. **Remove the SessionStart `additionalContext` nudge** that tells
   Claude sub-TLs to call `TeamCreate`. Root still creates the team
   (via the initial `exomonad init` session).

2. **Make `propagate_team_to_child` the only source of truth.** Already
   wired — no change required. After removing the TeamCreate nudge,
   sub-TLs simply *are* members of the parent's team via propagation.

3. **Document the convention in `rust/CLAUDE.md`:** "One team per
   root TL. Sub-TLs and leaves are synthetic members. Agents are
   named uniquely via `{slug}-{type}`."

4. **Followup — extend `TeamInfo`** to carry `agent_type`/`model`, so
   the reconcile-add-missing-member path in `claude-teams-bridge/
   src/registry.rs:284` stops hardcoding "gemini". Orthogonal to this
   decision but removes the last source of model drift.

## Why not Option C (subtree teams)

On paper, per-subtree task isolation is appealing. In practice:

- CC's Teams SendMessage/inbox targeting is per-team. Cross-team messaging
  requires resolver code that knows about the hierarchy — more drift risk.
- The delivery pipeline already resolves through one `TeamRegistry`. Adding
  a tree of teams means either each agent needs to know about multiple
  teams (complex) or delivery does N lookups (slow).
- The problem Option C solves (task crowding) is better solved at the
  *query* layer (filter by owner/parent) than at the *storage* layer
  (split into N teams).

Option C is the "correct" model if you squint at the hylomorphism, but
the practical cost of threading multi-team awareness through every
delivery path is not worth the task-list cleanliness.

## Why not the current hybrid

Every sub-TL owning its own team AND being registered in parent's team
is the worst of both worlds: doubled storage, ambiguous resolution,
no isolation benefit (tasks still end up shared via propagation into
parent's team anyway). The current behavior exists because both
mechanisms grew independently — not because anyone chose it.
