# ADR-003: Spec Commits (Decomposition Artifacts)

## Status

Accepted

## Context

When a parent node decomposes work into children, it needs to produce artifacts that:
1. Define the interfaces children implement against (compiler-enforced)
2. Communicate intent, context, and acceptance criteria (human-readable)
3. Minimize overlap between children's work (non-overlapping partition)

Children fork from the parent's spec commit, so whatever the parent writes becomes the shared starting point.

## Decision

### Spec Commit Contents

The parent produces a **spec commit** containing three layers, applied based on depth and complexity:

| Layer | Form | Purpose | When to include |
|-------|------|---------|-----------------|
| **Types** | Code: type signatures, trait definitions, module structure, function stubs with `todo!()` | Compiler-enforced interfaces between children's work | Always |
| **Intent** | Markdown: ADR-style docs, design rationale, context | Why the decomposition looks this way, architectural constraints | Shallow nodes (depth 0-1) |
| **Acceptance** | Code: failing tests, property-based test stubs | What "done" means, mechanically verifiable | When feasible, especially for leaf-adjacent nodes |

### Depth-Dependent Rigor

Deeper nodes have narrower scope and need less ceremony:

```
Root (depth 0):    Full ADRs + type stubs + integration tests + architecture diagrams
Subtree (depth 1): Type stubs + brief markdown + key test cases
Leaf-adjacent (depth 2): Type stubs + inline comments + maybe test stubs
```

### Example Spec Commit (Rust project)

```
commit: "scaffold: auth module decomposition"

src/auth/mod.rs          ← module structure, pub use declarations
src/auth/types.rs        ← JWT claims struct, AuthError enum, all fields defined
src/auth/middleware.rs    ← fn validate_token(token: &str) -> Result<Claims, AuthError> { todo!() }
src/auth/storage.rs      ← trait TokenStore { async fn store(...); async fn revoke(...); }
tests/auth_integration.rs ← #[test] fn test_roundtrip_jwt() { todo!() }
docs/adr-auth.md         ← "We use JWT because..., middleware validates on every request because..."
```

Children fork from this commit:
- Child A implements `middleware.rs` (has the type signature to implement against)
- Child B implements `storage.rs` (has the trait to implement)
- Child C implements the integration tests (has the stubs to fill in)

### Non-Overlapping Partition

The spec commit's primary job is ensuring children don't collide. The type stubs define module boundaries. Each child owns specific files/modules. The parent's spawn prompt reinforces this:

```
"Implement src/auth/middleware.rs — the validate_token function and
supporting helpers. DO NOT modify types.rs or storage.rs — those are
other children's responsibility. Your interface is defined by the
types in types.rs and the trait in storage.rs."
```

### Session Forking Interaction

With `--resume --fork-session`, children inherit the parent's full conversation context including the reasoning behind the decomposition. The spec commit is the *code* artifact; the forked session is the *reasoning* artifact. Together they give the child:

- **What** to implement (spec commit — types, tests)
- **Why** it's structured this way (forked session — parent's decomposition reasoning)
- **Where** boundaries are (spawn prompt — explicit file ownership)

## Consequences

**Positive:**
- Compiler catches interface mismatches between children
- Children can't accidentally overlap if modules are clearly partitioned
- Tests as acceptance criteria are mechanically verifiable
- Session forking reduces the need for verbose spawn prompts

**Negative:**
- Parent must invest tokens upfront in the spec commit (worthwhile — prevents rework)
- Type stubs may need updating if a child discovers the interface is wrong (child sends question to parent)
- Not all languages have strong enough type systems for this to be fully compiler-enforced

**Mitigations:**
- If a child needs to change a shared type, it sends a question to the parent. Parent updates the type and notifies siblings to rebase.
- For dynamically-typed languages, interface contracts are in docstrings/tests instead of types.
