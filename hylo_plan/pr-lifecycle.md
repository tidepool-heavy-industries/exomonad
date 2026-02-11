# PR Lifecycle (The Fold)

## Branch Topology

```
main
 └── gh-42.feature          (root subtree)
      ├── gh-42.feature.auth    (child subtree)
      │    ├── gh-42.feature.auth.middleware  (worker)
      │    └── gh-42.feature.auth.tests       (worker)
      └── gh-42.feature.api     (child subtree)
           └── gh-42.feature.api.endpoints    (worker)
```

PRs always target parent branch, not main. The branch tree mirrors the task tree. Branch naming uses `.` as hierarchy separator to avoid git collisions.

## Child Completion Flow

```
1. Child completes work
2. Stop hook enforces: commit, push, file PR against parent branch
3. GitHub triggers: Copilot review + CI
4. Iteration loop (child ↔ Copilot):
   a. Copilot posts review comments
   b. Child addresses comments, pushes
   c. Repeat until Copilot approves + CI passes
5. Parent receives "PR ready" message
6. Parent reviews (architectural fit, integration with other children)
   a. Approve → merge
   b. Request changes → child iterates
7. On merge: parent sends "rebase on parent" message to remaining siblings
```

## Review Token Economics

| Reviewer | Cost | Catches |
|----------|------|---------|
| CI | Infrastructure cost only | Compilation, test failures |
| Copilot | Free (GitHub) | Style, conventions, obvious bugs |
| Parent node | LLM tokens | Architectural fit, integration concerns, decomposition intent |

Parent only burns tokens on PRs that clear the mechanical bar. Most child ↔ review iteration is zero marginal cost.

## Merge Strategy

Parent performs the merge, not the child. Parent has context about:
- What other children are doing
- Whether this child's work fits the decomposition intent
- Whether siblings need to be notified/rebased

After merge, parent:
1. Messages remaining children: "parent branch updated, rebase"
2. Checks if all children are done → file own PR upward
3. Or identifies need for additional children → spawn more

## Rebase Propagation

When child A merges into parent branch, siblings B and C need to rebase.

**With git:** Parent sends message "rebase on parent branch." Child runs `git pull --rebase origin {parent_branch}`.

**With jj:** Automatic. jj tracks the parent relationship and rebases automatically when the base branch moves.

jj is the better long-term answer. git works for phase 1.

## Failure Modes

**Child PR fails CI repeatedly:** Parent gets notified after N failures. Can send guidance, kill and restart with different approach, or absorb the work.

**Child goes silent:** Long-poll timeout + periodic "are you alive?" pings. Parent can kill unresponsive children.

**Merge conflict between siblings:** Shouldn't happen if decomposition was non-overlapping. If it does, parent arbitrates — may need to kill one sibling, merge the other, then respawn with updated context.

**Decomposition was wrong:** Parent realizes mid-fold that the split doesn't work. Can kill remaining children, re-decompose, spawn new subtrees. The fold is not committed until the parent merges upward.
