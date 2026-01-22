# ADR 003: Tools as Workflows

**Date:** 2026-01-21
**Status:** Accepted
**Context:** Designing MCP tool philosophy for Tidepool

## Decision

MCP tools should encode complete workflows, not expose individual capabilities.

## Core Principle

**Tools encode domain knowledge.**

Instead of providing building blocks that agents compose, tools should capture:
- Best practices we'd prompt humans to follow
- Common failure modes and how to handle them
- Context-aware automation of manual steps
- The "right way" to accomplish an intent

## What This Means

### Design Process

When creating a new MCP tool, document workflow steps **before** coding:

1. **What is the user trying to accomplish?** (Intent, not mechanism)
2. **What are all the steps to get there?** (Complete workflow)
3. **What context can we leverage to automate steps?** (State, metadata, conventions)
4. **What can go wrong and how do we handle it?** (Error recovery)
5. **What happens after this workflow completes?** (Postconditions, next steps)
6. **How does this compose with other workflows?** (Integration points)

### Example: file_pr

**Bad (capability-oriented):**
```
create_pull_request(title, body)
```
Agent must remember to:
- Push commits first
- Format PR body correctly
- Update bead status
- Handle branch protection

**Good (workflow-oriented):**
```
file_pr(bead_id)
```
Tool handles:
- Validate: Check branch exists, commits present
- Push: Auto-push with -u flag if needed
- Format: Build PR body from bead metadata + git log
- Create: File PR with --head and --repo flags
- Update: Mark bead with PR link
- Guide: Return next steps (e.g., "PR #123 filed, awaiting review")

## Why This Matters

### The Prompt Problem

Traditional approach: Prompt agents with instructions
```
"Remember to push before filing PR. Format the PR body with:
- Bead ID in title
- Closes tidepool-XXX in body
- Summary of changes
..."
```

**Problems:**
- Agents forget steps
- Instructions get stale
- No error handling
- Inconsistent execution

### The Tidepool Solution

Encode instructions as deterministic code:
```haskell
filePRWorkflow :: BeadId -> Eff '[Git, BD, GitHub, ...] PRResult
filePRWorkflow beadId = do
  -- All the best practices, encoded
  validateBranch
  autoPush
  bead <- getBead beadId
  body <- formatPRBody bead
  pr <- createPR body
  updateBead beadId pr.url
  pure $ PRResult pr.number pr.url
```

**Benefits:**
- Never forget steps
- Single source of truth
- Automatic error handling
- Consistent execution
- Testable workflows

## This Is Why Tidepool DSL Exists

The DSL supports:
- **Arbitrary Haskell effects** - Compose Git, BD, GitHub, LSP, TUI operations
- **Inline LLM calls** - Use AI where helpful (format text, analyze code, suggest fixes)
- **Typed templates** - Generate prompts/messages with compile-time validation
- **Error recovery** - Algebraic effects enable sophisticated retry/fallback logic

We can build rich, opinionated workflows that encode institutional knowledge.

## Workflow Composition

**Workflows are atomic** - they don't call other workflows.

Instead, use a layered architecture:
- **Workflows** (agent-facing): Intent-driven, multi-step, context-aware
- **Utilities** (internal): Reusable helpers, single-purpose, composable
- **Primitives** (escape hatch): Raw Bash commands when needed

This prevents workflow spaghetti while enabling code reuse.

## Trade-offs

### What We Lose

- **Flexibility**: Can't compose workflows in novel ways
- **Learnability**: More to understand per tool (workflows are complex)

### Why We Accept This

The trade-off is worth it because:
- Most agents don't need novel composition
- Learning one workflow is easier than remembering many steps
- Opinionated tools guide correct usage
- Best practices are encoded, not documented

### Escape Hatch

Agents can always use the **Bash** tool when:
- The workflow tool doesn't exist yet
- They need ad-hoc exploration
- They're debugging a problem

## Migration Strategy

**Redesign, don't wrap.**

Existing capability-oriented tools should be redesigned as workflows, not wrapped:

- `exo_status` → `get_work_context` (status + analysis + next steps)
- `file_pr` → Enhanced with auto-push, validation, bead updates
- `exo_complete` → `complete_and_cleanup` (mark closed + sync + cleanup)

Don't create hybrid systems with both capabilities and workflows exposed. Commit to the workflow model.

## Examples

### Good Workflow Tools

**spawn_agents**: Complete parallel dispatch
1. Validate beads, templates, binaries
2. Create git worktrees + branches
3. Bootstrap config (copy template, symlink .env, write context)
4. Launch Zellij tabs with process-compose
5. Return monitoring info (tab IDs, paths)

**file_pr**: Complete PR filing
1. Validate branch + commits
2. Auto-push if needed
3. Build PR body from bead metadata
4. Create PR with proper flags
5. Update bead with PR link
6. Guide next steps

**get_work_context** (proposed redesign of exo_status):
1. Fetch bead details
2. Analyze git state (commits, diff, branch status)
3. Check dependencies (what's blocked, what's blocking)
4. Identify next steps
5. Return structured guidance

### Bad Capability Tools

**run_git_push**: Just wraps a command
- No value over Bash tool
- Agent must know when to call it
- No error handling
- No integration with bead state

**update_bead_status**: Single operation
- Building block, not destination
- Requires agent to orchestrate other steps
- Easy to forget or misuse
- Should be part of larger workflow

## Guidance for Tool Design

### When to Create a New Tool

Create a new MCP tool when you find yourself writing the same prompt instructions repeatedly.

**Indicators:**
- "Remember to push before filing PR"
- "Don't forget to update the bead status"
- "Make sure you check for X before doing Y"

These are workflows waiting to be encoded.

### When to Use Bash Instead

Use the Bash tool when:
- The workflow tool doesn't exist yet (valid escape hatch)
- One-off debugging or exploration
- Ad-hoc operations that won't be repeated

Don't create capability tools just to wrap Bash commands.

## Related Decisions

- **tidepool-ttx**: Workflow and MCP tool philosophy (implementation)
- **tidepool-bx7**: Role-based tool assignment (which workflows per role)
- **tidepool-98v**: file_pr auto-push (example workflow enhancement)

## References

- Interview: 2026-01-21 popup survey
- Core insight: "I don't want to provide capabilities via MCP tool, I want to provide whole workflows as MCP tools"
- Key realization: "Takes all the best practices, all the stuff we might prompt re: how to work in this setup, and moves it to deterministic code"
