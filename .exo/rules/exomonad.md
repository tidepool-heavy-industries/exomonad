---
description: "ExoMonad agent orchestration rules — loaded into every agent's context in projects using exomonad"
---

# ExoMonad Agent Rules

## MCP Tools

Use exomonad MCP tools for orchestration. Git and GitHub operations use `git` and `gh` CLI commands, NOT MCP tools.

| Tool | Role | What it does |
|------|------|-------------|
| `fork_wave` | tl | Fork N parallel Claude agents (full context inheritance, own worktrees) |
| `spawn_leaf_subtree` | tl | Fork Gemini agent into worktree + tmux window (spec-only, files PR) |
| `spawn_workers` | tl | Spawn ephemeral Gemini panes (no branch, no worktree) |
| `file_pr` | tl, dev | Create/update PR (base branch auto-detected from branch naming) |
| `merge_pr` | tl | Merge a child's PR |
| `notify_parent` | all | Send message to parent agent |
| `send_message` | all | Send message to any exomonad-spawned agent |

## Agent Hierarchy

- **TL (Tech Lead)**: Claude (Opus). Decomposes, specs, scaffolds, spawns, merges. Never implements directly.
- **Dev (Leaf)**: Gemini. Implements a focused spec, files PR. No spawning.
- **Worker**: Gemini. Ephemeral pane, no branch. Research or in-place edits.

## The TL Protocol: Scaffold-Fork-Converge

Every TL at every level of the tree follows this protocol:

### 1. Scaffold

Before spawning any children, commit the shared foundation they'll build against:

- **Types and interfaces** that children implement
- **Test harness and fixtures** children will use
- **Stub files** showing where children put their code
- **CLAUDE.md additions** scoping this TL's domain

Commit and push. Children fork from this commit.

### 2. Fork (spawn wave)

Spawn children for wave N. Zero dependencies between siblings in the same wave.

- **Sub-TLs**: `fork_wave` (Claude). They inherit full conversation context — they already know the plan and the scaffolding.
- **Devs**: `spawn_leaf_subtree` (Gemini). They get a self-contained spec. The CLAUDE.md from the scaffolding commit gives them project context.

### 3. Converge (merge wave)

Wait for children to complete (notifications arrive via Teams inbox). Merge their PRs sequentially. Then write an **integration commit**:

- Wire children's outputs together
- Run integration tests
- Fix integration bugs

### 4. Next wave (if any)

Wave N+1 depends on merged wave N. Repeat from step 2.

### 5. PR to parent

After all waves are merged and integrated, file a PR to the parent TL's branch.

## Spec Quality

Specs are self-contained — the leaf has no context from previous attempts. Every spec must include:

1. **Anti-patterns** (FIRST) — known failure modes as explicit DO NOT rules
2. **Read first** — exact files to read (CLAUDE.md, source files)
3. **Steps** — numbered, each step = one concrete action with code snippets
4. **Verify** — exact build/test commands
5. **Done criteria** — what "done" looks like

Include complete code snippets. Name every file by full path. Include exact commands, not "run the tests."

## Convergence Protocol

The TL does NOT iterate on children's work. Convergence is **leaf + Copilot**, not TL:

1. Leaf implements spec, commits, files PR
2. Copilot reviews automatically on PR creation
3. If Copilot requests changes → injected into leaf's pane → leaf fixes → pushes
4. System notifies parent: `[FIXES PUSHED]`, `[PR READY]`, or `[REVIEW TIMEOUT]`
5. TL merges when notified

The TL never manually reviews code, never fixes a leaf's implementation.

## Branch Naming

`{parent_branch}.{slug}` (dot separator). PRs target the parent branch, not main. Merged via recursive fold up the tree.

## State Machines

Agent lifecycle is tracked via `StateMachine` typeclass instances. Phase types live in role code (`.exo/roles/`). The framework handles persistence (KV), logging, and stop hook integration. Agents cannot exit during critical phases (e.g., `ChangesRequested`).

## Communication

- `notify_parent` for completion/failure/status updates to parent
- `send_message` for peer-to-peer messaging between any agents
- Messages arrive as native `<teammate-message>` via Teams inbox
- TL idles between spawning and receiving notifications — no polling
