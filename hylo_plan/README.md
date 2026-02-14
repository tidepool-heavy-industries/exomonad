# Hylomorphism Agent Tree

Recursive agent tree where each node is a worktree + Claude/Gemini session. **Unfold** (decompose tasks into subtrees via spec commits) then **fold** (merge via PRs bubbling up).

## Core Model

```
seed (branch, task)
  → parent writes type stubs / interfaces / tests / ADRs
    → parent commits scaffold, spawns children (fork from that commit)
      → children implement (or further decompose)
        → children file PRs against parent branch
          → Copilot reviews, child iterates autonomously
            → child goes dormant when PR is green
              → parent wakes, reviews PR (architectural fit)
                → parent merges → siblings rebase (stop-hook checks)
                  → repeat until all children merged
                    → parent files PR against its parent
```

## Node Types

**Subtree node** (Claude): Owns a branch + worktree. Has coordination tools (`spawn_subtree`, `spawn_workers`, messaging, PR lifecycle). Can decompose further. Runs event-driven: works actively during decomposition, then goes dormant waiting for child events delivered as synthetic user messages via Zellij STDIN injection.

**Worker node** (Gemini): No spawn tools. Executes bounded task in its own worktree branch. Files PR, iterates with Copilot, goes dormant when green. Cheap, disposable.

**Human**: Can drop into any Zellij tab/pane to steer directly. Messages from children bubble up through the chain of command (child → parent → ... → root → human). Human is the ultimate escalation point but doesn't block execution.

## Key Design Decisions

| Decision | Rationale |
|----------|-----------|
| Session forking (`--resume --fork-session`) | Children inherit parent's full conversation context. Spawn prompts can be minimal. |
| Dormant parents via Zellij STDIN injection | Parent is just a Claude session waiting for "user messages" that are actually system events. Zero token cost while dormant. |
| Git as distributed coordination substrate | Branches encode the task tree. PRs are the fold operation. Rebase propagates state. No separate coordination DB needed. |
| Copilot as first-pass reviewer | Zero marginal cost. Children iterate autonomously with Copilot. Parent only reviews PRs that pass mechanical bar. |
| Implicit event registration from spawn | Spawning a child automatically routes that child's lifecycle events to the parent. No explicit subscribe step. |
| Stop-hook rebase checks | Before exiting, children check if parent branch moved. If so, rebase. Pull-based, no injection needed for rebase cascade. |
| Chain of command messaging | Messages flow child → parent only. No grandchild → grandparent bypass. Parent decides what to escalate. Bounds message volume per node. |
| Tool descriptions encode the recursion decision | "Use `spawn_subtree` when work needs further decomposition" vs "Use `spawn_workers` when work is concrete enough to execute directly." Agent's tool choice IS the anamorphism base case. |
| Spec commit = types + markdown + tests | Parent writes type stubs (compiler-enforced interfaces), markdown (intent/context), and tests where feasible. Depth-dependent: deeper nodes may skip ADRs. |
| Soft depth limits with human approval | No hard cap. Deeper spawning requires human approval (via chain of command escalation). |

## Event Delivery Architecture

```
Child event (completion, question, PR ready)
  → child's hook fires → HTTP to MCP server
    → server resolves parent session from child identity
      → server renders natural language message from structured template
        → Zellij write-chars injects message into parent's pane
          → parent Claude sees it as a "user message"
            → parent processes, takes action, returns to dormant

GitHub events (Copilot review, CI status)
  → polling loop checks PR status via GitHub API
    → on change, renders natural language message
      → Zellij write-chars injects into relevant node's pane
```

## Node Lifecycle

```
                    ┌─────────────┐
                    │   ACTIVE    │ ← decomposing, writing specs
                    │  (working)  │
                    └──────┬──────┘
                           │ spawns children, goes idle
                    ┌──────▼──────┐
                    │   DORMANT   │ ← waiting for events
                    │  (idle)     │ ← zero token cost
                    └──────┬──────┘
                           │ event arrives (synthetic user msg)
                    ┌──────▼──────┐
                    │   ACTIVE    │ ← reviewing PR, answering question
                    │ (reviewing) │
                    └──────┬──────┘
                           │ action complete, back to idle
                    ┌──────▼──────┐
                    │   DORMANT   │ ← or exits if all children merged
                    └──────┬──────┘
                           │ all children done
                    ┌──────▼──────┐
                    │   ACTIVE    │ ← files own PR upward
                    │ (completing)│
                    └──────┬──────┘
                           │ PR green, goes dormant
                    ┌──────▼──────┐
                    │   DORMANT   │ ← waiting for parent to merge
                    └─────────────┘
```

## Files

- [ADR-001: Event Delivery](adr-001-event-delivery.md) — Zellij STDIN injection as event bus
- [ADR-002: Fold Protocol](adr-002-fold-protocol.md) — PR lifecycle, merge, rebase cascade
- [ADR-003: Spec Commits](adr-003-spec-commits.md) — How parents decompose work for children
- [tools.md](tools.md) — Complete tool surface with schemas
- [phase-1.md](phase-1.md) — Implementation roadmap
