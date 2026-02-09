# Tree of Agents: Git as Hylomorphism

## Core Idea

Recursive decomposition of work into a tree of git worktrees, where each
node is simultaneously a TL (to its children) and a dev (to its parent).
Git operations ARE the coordination primitives — no separate task layer.

## The Hylomorphism

### Unfold (anamorphism — specs decompose downward)

- Root branches off `main`, receives project spec
- Root commits plan + stubs + tests, spawns child branches off its HEAD
- Each child repeats: plan, stub, spawn children from *its* HEAD
- Recurses until specs are leaf-sized (base case)

### Fold (catamorphism — PRs merge upward)

- Leaves finish first, PR to parent branch
- Parent merges, its HEAD advances
- Parent PRs to *its* parent when all children merged
- Continues until root PRs to `main`

### Streaming (fold starts before unfold finishes)

- Siblings run parallel
- First sibling done merges first → parent HEAD advances
- Other in-flight siblings rebase onto new parent HEAD
- Rebase cascades to *their* descendants
- Conflicts surface early, at rebase — not at final merge

**Invariant:** Every in-flight branch is based on parent HEAD including
all merged sibling work. The tree is always in a valid integration state.

## Why Git

Branches *are* the agent tree. Commits *are* the work product. PRs *are*
the fold operation. Rebase *is* the synchronization primitive. No separate
coordination layer — git is the coordination layer.

## Node Roles

Each node wears two hats:

- **As TL** (looking down): decomposes spec into child tasks, spawns
  children off its HEAD, reviews child PRs, merges, triggers rebase
  cascade to remaining children
- **As dev** (looking up): implements its portion, PRs to parent when
  done (or when all children have merged into it)

The root is pure TL (parent is `main`). Leaves are pure devs (no children).
Interior nodes are both.

## Messaging

All coordination flows through parent-child edges. No sibling-to-sibling
communication — the parent is the TL for that subtree.

### Child → Parent

- **"Done"**: PR to parent branch (the fold step)
- **"Blocked"**: rebase conflict with sibling's merged work → message
  parent with conflict details
- **"Bigger than expected"**: child discovers complexity, spawns its
  own children (unfold continues deeper)
- **"Need clarification"**: spec ambiguity, push question to parent

### Parent → Child

- **"Sibling merged, rebase"**: automatic after merging a child PR.
  Parent's HEAD advances, remaining children must rebase. Cascade
  propagates to grandchildren.
- **"Scope change"**: parent amends spec (new commits on parent branch),
  children rebase to pick up changes
- **"Abort"**: parent decides subtree approach is wrong, can prune

### Sibling Awareness

None directly. A sibling's work becomes visible only after:
1. Sibling PRs to parent
2. Parent merges
3. Parent's HEAD advances
4. Remaining siblings rebase

This means conflicts are discovered incrementally as siblings complete,
not all at once at the end.

## Complexity Discovery

The key property that makes this a *streaming* hylomorphism: a leaf can
discover it's not actually a leaf and unfold further. The tree grows
during traversal.

Example: Agent receives "implement auth middleware" as a leaf task.
Discovers it needs OAuth, JWT validation, session management, and rate
limiting. Decomposes into 4 child branches, becomes an interior node.
The fold will collect their work before this node PRs to its parent.

## Relationship to Current ExoMonad

Current `spawn_agents` is the degenerate case: depth-1 tree with manual
fold. One TL (Claude) spawns N leaves (Gemini agents), each PRs to main.
No automatic rebase cascade, no recursive decomposition.

The path forward:
1. Depth-1 with automatic fold (merge PRs, rebase siblings) ← near term
2. Depth-N with streaming fold ← the full hylomorphism
3. Complexity discovery (nodes can unfold further) ← adaptive

## Implementation Notes

### MCP Server as Tree Coordinator

The exomonad MCP server process is a singleton that outlives any individual
agent. It maintains the tree structure and orchestrates:
- Branch creation (unfold steps)
- PR detection and merge (fold steps)
- Rebase cascade triggers
- Message routing between parent/child pairs

### Haskell DSL

The recursion scheme is expressed in typed Haskell WASM. The `ana` and
`cata` with streaming interleave is too complex for prompt-based
orchestration but natural in recursion schemes.

### Git as State

No database needed. The tree state is recoverable from git:
- Branch naming convention encodes parent-child: `tree/{root}/{depth}/{n}`
- Open PRs = pending fold steps
- Merged PRs = completed fold steps
- Branch existence = live node

### Rebase Cascade Algorithm

When parent merges child C_i:
1. Parent HEAD advances
2. For each remaining child C_j (j ≠ i):
   a. Rebase C_j onto new parent HEAD
   b. If conflict: pause C_j, notify parent
   c. If clean: notify C_j agent to continue from new base
   d. Recurse: for each of C_j's children, rebase onto C_j's new HEAD
