---
name: tl-subagent-dispatch
description: Use when spawning subagents, monitoring their progress, or intervening when stuck. Covers worktree creation, human oversight patterns, and completion protocol.
---

# Tech Lead Subagent Dispatch

## Core Principles

1. **Fire-and-forget execution.** TL decomposes, specs, spawns, and idles. Convergence is leaf + Copilot + event handlers, not TL.

2. **Dispatch is heterogeneous.** Claude for decomposition, Gemini for implementation, Copilot for review.

## Agent Types

| Tool | Agent | Role | Use When |
|------|-------|------|----------|
| `spawn_subtree` | Claude | TL (can spawn children) | Multi-step work requiring sub-decomposition |
| `spawn_leaf_subtree` | Gemini | Dev (files PR) | Focused implementation in isolated worktree |
| `spawn_workers` | Gemini | Worker (ephemeral) | Investigation, hypothesis testing, no commits |

## Dispatch Protocol

### 1. Write the Spec

Every spec follows this structure:

```
1. ANTI-PATTERNS      — Known failure modes as "DO NOT" rules (FIRST)
2. READ FIRST         — Exact files to read (CLAUDE.md, source files)
3. STEPS              — Numbered, each step = one concrete action
4. VERIFY             — Exact build/test commands
5. DONE CRITERIA      — What "done" looks like
```

### 2. Spawn

```
# Focused implementation
spawn_leaf_subtree(
  task="[full spec here]",
  branch_name="feature-name"
)

# Investigation (multiple hypotheses)
spawn_workers(specs=[
  { name: "h1", task: "Check if X causes the bug" },
  { name: "h2", task: "Check if Y causes the bug" }
])
```

### 3. Return Immediately

After spawning, idle. Do not watch, poll, or narrate.

## Convergence Flow

1. Leaf works → commits → files PR via `file_pr`
2. Copilot reviews automatically (first review only)
3. If Copilot posts comments → event handler injects into leaf's pane → leaf fixes → pushes
4. Event handler fires `[FIXES PUSHED]` to TL (Copilot does NOT re-review)
5. TL merges if CI passes

**Alternative paths:**
- Copilot approves first time → `[PR READY]` → TL merges
- No Copilot review → `[REVIEW TIMEOUT]` after 15min (5min after addressing changes) → TL merges if CI passes
- Leaf fails → `[FAILED: agent-id]` → TL re-decomposes

## When TL Gets Notified

| Message | Source | Action |
|---------|--------|--------|
| `[FIXES PUSHED]` | Event handler | Merge if CI passes |
| `[PR READY]` | Event handler | Merge |
| `[REVIEW TIMEOUT]` | Event handler | Merge if CI passes |
| `[from: agent-id]` | Agent message | Read, do not auto-merge |
| `[FAILED: agent-id]` | Agent failure | Re-decompose or escalate |

## Spec Anti-Patterns (Front-Load These)

| Known Failure Mode | Anti-Pattern Rule |
|---|---|
| Adds unnecessary dependencies | "ZERO external dependencies" |
| Invents escape hatches | "No `todo!()`, `Raw(String)`, `Other(Box<dyn Any>)`" |
| Writes thinking-out-loud comments | "No stream-of-consciousness comments" |
| Renames types/variants | "Use EXACT type signatures below" |
| Makes architectural decisions | "Do not change the module structure" |
| Uses `git add .` | "Always `git add` specific files by name" |
| Shells out to nonexistent CLIs | "Use MCP tools, NOT bash commands for notify_parent" |
| Corrupts Haskell pragmas | "Closing delimiter is `#-}` not `#}`" |

## Anti-Patterns (TL Behavior)

### TL explores codebases
That exploration belongs in a worker's spec. Spawn a worker to investigate.

### TL writes implementation code
That belongs in a leaf's task. Gemini agents are smart enough.

### TL manually reviews intermediate output
Convergence is leaf + Copilot. TL only reviews at merge time.

### TL iterates on a failing leaf
Escalation, not iteration. If leaf fails 3+ rounds, re-decompose with a different approach.
