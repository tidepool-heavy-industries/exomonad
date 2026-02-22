# ADR 006: Spec Commits (Decomposition Artifacts)

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

With `--resume --fork-session`, children inherit the parent's full conversation context. The spec commit is the *code* artifact; the forked session is the *reasoning* artifact. Together:
- **What** to implement (spec commit — types, tests)
- **Why** it's structured this way (forked session — parent's decomposition reasoning)
- **Where** boundaries are (spawn prompt — explicit file ownership)

## Consequences

- Compiler catches interface mismatches between children
- Children can't accidentally overlap if modules are clearly partitioned
- Parent invests tokens upfront in the spec commit (prevents rework downstream)
- If a child needs to change a shared type, it sends a question to the parent
