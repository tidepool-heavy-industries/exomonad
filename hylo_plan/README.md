# Hylomorphism Agent Swarm

Recursive agent tree where each node is a worktree + agent session. Unfold (decompose tasks into subtrees) then fold (merge via PRs bubbling up).

## Core Model

```
seed (branch, task)
  → decompose into non-overlapping subtasks
    → spawn subtree nodes (can further decompose) or leaf nodes (execute and finish)
      → children work, bubble messages up to parent
        → children file PRs against parent branch
          → Copilot + CI first-pass review
            → parent reviews PRs that pass mechanical bar
              → merge, notify siblings to rebase
                → repeat until parent's work is done
                  → parent files PR against its parent
```

## Node Types

**Worktree node** (Claude): Has coordination tools (`spawn_subtree`, `spawn_leaf`, messaging, PR lifecycle). Can decompose further. Owns a branch + worktree. Runs event-driven idle loop via long-poll `get_messages`.

**Leaf node** (Gemini): No spawn tools. Executes bounded task in parent's worktree or own worktree. Works and exits. Cheap, disposable.

**Researcher** (either): Deep research task. Gets a tmp dir, can clone repos, read code. No write access to the tree. v0.1 = ping human to paste into Gemini.

## Every Node Is a Claude/Gemini Session

All nodes use native builtins (Bash, Write, Read, etc). ExoMonad provides coordination tools on top. The worktree directory is the cage. The spawn prompt is the constraint. ExoMonad goes *around* the agent, not *inside* it.

```
Every node:      Claude/Gemini + native builtins + jj/git
Worktree nodes:  + exomonad spawn/message/task/PR tools
Leaf nodes:      no spawn tools, that's it
```

## Key Design Decisions

- **Worktrees, not Claude Teams** for isolation. Can migrate to Teams bus later as it matures.
- **PRs as fold operation.** Child → parent branch targeting. GitHub PRs are reviewable, reversible, auditable.
- **Copilot + CI as first-pass filter.** Parent only reviews PRs that pass mechanical bar. Zero marginal cost for most iteration.
- **jj for rebase propagation.** Agents use jj directly. Automatic rebasing when siblings merge into parent branch.
- **Long-poll messaging.** `get_messages` blocks until message or timeout (5m). One tool call per idle period.
- **Tool descriptions encode the base case.** `spawn_subtree` vs `spawn_leaf` teaches the agent when to unfold vs execute.
- **Intelligence gradient at every level.** Claude reasons about decomposition. Gemini executes. Copilot reviews mechanically. Parent reviews architecturally.

## Coordination Architecture

Single exomonad MCP server is the coordination bus. All nodes talk to it. It outlives any individual agent session. State lives there.

Messages flow child → parent only. Parent chooses what to forward upward. No grandchild → grandparent bypass. This bounds each node's message volume.

## Human's Role

Human supervises and converses with nodes (usually root, subnodes as needed). Root node can `ask_question` which bubbles up to human via TUI. Human is polled for advice, not blocking execution.

## Files

- [phase-1.md](phase-1.md) — Concrete implementation plan
- [tools.md](tools.md) — Tool surface design
- [pr-lifecycle.md](pr-lifecycle.md) — PR review and merge flow
