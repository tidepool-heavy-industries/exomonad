# Spec Commits (Decomposition Artifacts)

**Status:** Accepted

## Context

When a parent node decomposes work into children, it needs artifacts that define interfaces (compiler-enforced), communicate intent (human-readable), and prevent overlap between children.

## Decision

The parent produces a **spec commit** before spawning children, containing three layers applied based on depth:

| Layer | Form | When |
|-------|------|------|
| **Types** | Type signatures, trait definitions, function stubs with `todo!()` | Always |
| **Intent** | Markdown: ADR-style docs, design rationale | Shallow nodes (depth 0-1) |
| **Acceptance** | Failing tests, property-based test stubs | When feasible |

Children fork from this commit. The type stubs define module boundaries — each child owns specific files. The spawn prompt reinforces file ownership explicitly.

### Session Forking Interaction

The scaffold commit plus a compact task prompt is the primary handoff. The repository artifact
carries interfaces, executable acceptance context, and nearby intent; the prompt adds decisions that
are not legible there and names any mechanical scope. A fresh child context independently re-derives
the implementation from those artifacts, which is part of the review architecture.

Session forking is opt-in. When a task truly benefits from the parent's conversational context,
`fork_session` may add it, but full transcript inheritance is not the default reasoning channel.

## Consequences

- Compiler catches interface mismatches between children
- Children can't accidentally overlap if modules are clearly partitioned
- Parent invests tokens upfront in the spec commit (prevents rework downstream)
- Fresh child contexts receive distilled intent instead of accumulated conversation by default
- If a child needs to change a shared type, it sends a question to the parent
