---
paths:
  - "**/*.rs"
---

# Functional Rust

## Core Philosophy

**Think -> Type -> Implement.** Sketch in comments, get types compiling with `todo!()`, then fill in implementation. Don't implement to discover - discover, then implement.

```rust
fn process(data: RawData) -> Result<Clean, Error> {
    todo!("validate, transform, ensure invariants")
}
```

## Type Design

- Newtype pattern by default for domain concepts
- Validate at construction: public constructor returns `Result`, private fields ensure invariants
- Once a type exists, it's valid (`UserId::new(s)` validates, then `UserId` is always valid)
- State transitions expressed through the type system

## Iterator Chains

Default to functional composition: `.iter().map().filter().collect()`

Iterator chains are acts of communication - readable as declarative descriptions of intent. Use combinators for straightforward cases, pattern matching for complex logic.

## Mutation & Cloning

- `clone()` by default, `mut` only when performance is proven necessary
- Local `&mut` is fine - controlled state transformation within scope
- Mutation within function boundaries acceptable; APIs should appear immutable
- Never use `unsafe` - restructure the design instead

## Error Handling

- `?` operator for propagation by default
- `thiserror` for domain-specific error types
- Structured errors over stringly-typed
- Fail fast and loud with clear messages
- Compose errors at module boundaries using `#[from]}

## Module Organization

- ~500 lines before considering a split
- Orthogonality principle: separate things that could exist independently
- Clear boundaries with focused responsibilities
- Use visibility to highlight conceptual boundaries

## Comments

Comments describe what is, not what was. Git history tracks what was.

```rust
// BAD: "Changed from HashMap to BTreeMap for ordering"
// GOOD: "Maintains sorted order for consistent iteration"

// BAD: "Removed caching because it caused issues"
// GOOD: No comment, or: "Direct fetch ensures fresh data"
```

If something was removed, don't comment about its absence.
