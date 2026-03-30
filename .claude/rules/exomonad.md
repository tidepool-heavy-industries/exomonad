---
description: "ExoMonad agent orchestration rules — loaded into every agent's context in projects using exomonad"
---

# ExoMonad Agent Rules

## Model

ExoMonad is a hylomorphism over context windows. Unfold = plan + scaffold + spawn. Fold = merge + integrate + PR upward. Each agent is a triad: worktree (filesystem) + context window (attention) + actor (messages). See `CLAUDE.md` § Model for the full conceptual framework.

## MCP Tools

Use exomonad MCP tools for orchestration. Git and GitHub operations use `git` and `gh` CLI commands, NOT MCP tools.

| Tool | Role | What it does |
|------|------|-------------|
| `fork_wave` | root, tl | Fork N parallel Claude agents (own worktrees, context inherited by default via `fork_session`) |
| `spawn_gemini` | root, tl | Spawn Gemini agent in own worktree+branch (files PR). Structured spec fields: steps, verify, boundary, context, read_first |
| `spawn_worker` | root, tl | Spawn ephemeral Gemini worker in tmux pane (no branch, no PR). Just name + task |
| `file_pr` | tl, dev | Create/update PR (base branch auto-detected from branch naming) |
| `merge_pr` | root, tl | Merge a child's PR |
| `notify_parent` | tl, dev, worker | Send message to parent agent |
| `send_message` | all | Send message to any exomonad-spawned agent |
| `task_list` | dev, worker | List tasks from the shared task list |
| `task_get` | dev, worker | Get a task by ID |
| `task_update` | dev, worker | Update task status, owner, or activeForm |

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
- **Devs**: `spawn_gemini` (Gemini, worktree). They get a self-contained spec. The CLAUDE.md from the scaffolding commit gives them project context.
- **Workers**: `spawn_worker` (Gemini, ephemeral pane). Research, boilerplate, or non-conflicting edits.

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

`{parent_branch}.{slug}-{type}` (dot separator, suffixed). The last dot-segment IS the `AgentName` — one namespace, zero translation. PRs target the parent branch, not main. Merged via recursive fold up the tree.

## State Machines

Agent lifecycle is tracked via `StateMachine` typeclass instances. Phase types live in role code (`.exo/roles/`). The framework handles persistence (KV), logging, and stop hook integration. Agents cannot exit during critical phases (e.g., `ChangesRequested`).

## Communication

- `notify_parent` for completion/failure/status updates to parent
- `send_message` for peer-to-peer messaging between any agents
- Messages arrive as native `<teammate-message>` via Teams inbox
- TL idles between spawning and receiving notifications — no polling
