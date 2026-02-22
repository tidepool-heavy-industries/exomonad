# Agent Identity Model

**Status:** Accepted

## Decision

An agent's identity is its birth-branch. The branch name is assigned at spawn time, is immutable for the agent's lifetime, and encodes the agent's position in the tree.

There are no UUIDs, no registration databases, no coordination protocols. The filesystem IS the registry.

## How It Works

### Identity = Birth-Branch

When `spawn_subtree` creates a new agent on branch `main.feature.auth`, that agent's identity is `main.feature.auth`. The root TL's identity is `root`.

The identity is:
- **Immutable**: Set at spawn, never changes
- **Deterministic**: Derived from parent branch + slug, no randomness
- **Hierarchical**: `main.feature.auth` → parent is `main.feature` → grandparent is `main`
- **Human-readable**: You can see the tree structure in the name

### Parent Resolution

An agent's parent is derived mechanically from its branch name by stripping the last dot-separated segment:

```
main.feature.auth.middleware → parent: main.feature.auth
main.feature.auth            → parent: main.feature
main.feature                 → parent: main (root)
```

This means `notify_parent` doesn't need a "parent address" parameter — the server resolves it from the caller's identity.

### Discovery

Agents are discovered by scanning the filesystem:
- **Worktree agents**: `.exo/worktrees/{slug}/` directories
- **Ephemeral workers**: `.exo/agents/{name}/` directories
- **Liveness**: Cross-referenced with Zellij tab/pane presence

No central registry to keep in sync. If a worktree directory exists, the agent exists (or existed).

### EffectContext

Each agent gets its own backend with identity baked in at construction:

```rust
struct EffectContext {
    agent_name: String,    // e.g., "auth-middleware"
    birth_branch: String,  // e.g., "main.feature.auth.middleware"
}
```

The server maintains a per-agent backend cache keyed by birth-branch. When an MCP request arrives, the server looks up the caller's EffectContext and routes the request to the right backend instance.

### MCP URL Routing

Each agent connects to the MCP server at a URL that encodes its identity:

```
http://localhost:7432/agents/{role}/{name}/mcp
```

For example:
- Root TL: `http://localhost:7432/agents/tl/root/mcp`
- Feature subtree: `http://localhost:7432/agents/tl/main.feature/mcp`
- Leaf worker: `http://localhost:7432/agents/dev/main.feature.auth/mcp`

The server extracts `role` and `name` from the URL path and constructs the EffectContext.

## Consequences

- No coordination protocol needed for identity assignment
- Parent resolution is O(1) string manipulation
- Tree structure is visible in branch names (`git branch` shows the full tree)
- Agent identity survives server restarts (it's in the branch name, not server memory)
- Branch names can get long with deep nesting (mitigated by depth cap of 2)
- Renaming a branch would break the identity model (but we never rename branches)
- Dot-separated naming collides with branch names that naturally contain dots (mitigated by convention — slugs use hyphens, not dots)
