---
name: tl-subagent-dispatch
description: Use when spawning subagents, monitoring their progress, or intervening when stuck. Covers worktree creation, human oversight patterns, and completion protocol.
---

# Tech Lead Subagent Dispatch

## Core Principles

1. **Human oversight is non-negotiable.** Subagents run in Zellij tabs where the TL can observe, intervene, and review. Never spawn headless background agents for implementation work.

2. **Dispatch is heterogeneous.** A wave can mix tactical (haiku) and strategic (opus) agents with different oversight levels.

## Agent Classification

| Type | Model | Harness | Oversight | Use When |
|------|-------|---------|-----------|----------|
| **Tactical** | Haiku | `claude` | Light - batch review PRs | Formulaic: docs, tests, simple tools |
| **Strategic** | Opus | `exomonad init` | Heavy - active collaboration | Ambiguous: architecture, refactors, new subsystems |

**Strategic agents serve dual purpose:**
- Execute complex work
- Provide UX feedback on tooling (they use the harness, they feel the friction)

## Dispatch Protocol

### 1. Create Worktrees

```bash
# Via MCP tool (creates git worktrees only)
spawn_agents(["123", "456", "789"])

# Returns worktree paths
# /Users/.../dev/.worktrees/exomonad/gh-123-description
```

### 2. Human Launches Agents (Manual)

TL opens Zellij tabs and starts agents based on classification:

**Tactical agent (haiku):**
```bash
cd /path/to/worktree
# Issue context injected via SessionStart hook
claude                    # Uses default model (haiku)
```

**Strategic agent (opus):**
```bash
cd /path/to/worktree
claude                    # Uses exomonad MCP tools via exomonad serve
```

## Monitoring Patterns

### Tactical Agents (Light Touch)
- Glance at tabs periodically
- Batch review PRs after wave completes
- Intervene only on visible failure

### Strategic Agents (Heavy Touch)
- Active observation
- Answer questions as they arise
- Provide context agent can't discover alone
- Collect UX feedback ("what's frustrating?")

### Check-in Commands
```bash
gh issue list --assignee @me   # Who's working on what
gh pr list                     # PR status
git worktree list              # Active worktrees
```

## Intervention Patterns

### For Tactical Agents

**Redirect (async):**
```bash
# Write to worktree's CLAUDE.md or .claude/rules/, agent discovers on next file read
echo "STOP: Don't modify X, use Y instead" >> /path/to/worktree/.claude/rules/redirect.md
```

**Kill and restart (if off track):**
```bash
# In agent's tab: Ctrl+C
git status && git diff    # Review state
claude                    # Fresh start
```

### For Strategic Agents

**Direct collaboration:**
Switch to agent's tab, engage directly:
```
I see you're approaching X this way. Have you considered Y?
The constraint you're missing is Z.
```

**UX feedback collection:**
```
What's the most annoying part of this workflow?
What tool do you wish existed?
What information did you have to hunt for?
```

## Completion Protocol

Agents should follow this sequence:

```
1. Implementation complete
2. cabal build / cargo build (verify compiles)
3. Commit with [#issue-num] prefix
4. git push -u origin <branch>
5. gh pr create
6. (TL merges after review)
```

**For strategic agents, also:**
- Capture UX feedback as new issues
- Document architectural decisions made

## Anti-Patterns

### ❌ Background Task agents
```python
Task(prompt="implement X", run_in_background=True)
```
Problems:
- No visibility into progress
- Can't intervene
- Competes for context with TL session
- No human review before actions

### ❌ Treating strategic work as tactical
Complex/ambiguous issues need opus + full harness + heavy oversight.
Haiku on a refactor = wasted tokens and poor results.

### ❌ Treating tactical work as strategic
Simple/formulaic issues don't need opus or heavy oversight.
Opus on a docs update = wasted money.

### ✅ Correct Pattern
```
1. Classify issues (tactical vs strategic)
2. Create worktrees via spawn_agents
3. Launch agents with appropriate model/harness
4. Monitor with appropriate intensity
5. Review PRs before merge
6. Collect UX feedback from strategic agents
```

## Worktree Management

### Listing
```bash
git worktree list
```

### Cleanup (after merge)
```bash
git worktree remove /path/to/worktree
# Or batch cleanup
git worktree prune
```

### Reuse
If worktree exists for an issue, cd into it rather than creating new.