# Tools as Workflows

**Date:** 2026-01-21
**Status:** Accepted
**Context:** Designing MCP tool philosophy for ExoMonad

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
- Link to issue
- Handle branch protection

**Good (workflow-oriented):**
```
file_pr(issue_number)
```
Tool handles:
- Validate: Check branch exists, commits present
- Push: Auto-push with -u flag if needed
- Format: Build PR body from issue metadata + git log
- Create: File PR with --head and --repo flags
- Update: Link PR to issue
- Guide: Return next steps (e.g., "PR #123 filed, awaiting review")

## Why This Matters

### The Prompt Problem

Traditional approach: Prompt agents with instructions
```
"Remember to push before filing PR. Format the PR body with:
- Issue number in title
- Closes #XXX in body
- Summary of changes
..."
```

**Problems:**
- Agents forget steps
- Instructions get stale
- No error handling
- Inconsistent execution

### The ExoMonad Solution

Encode instructions as deterministic code:
```haskell
filePRWorkflow :: Int -> Eff '[Git, GitHub, ...] PRResult
filePRWorkflow issueNum = do
  -- All the best practices, encoded
  validateBranch
  autoPush
  issue <- getIssue repo issueNum False
  body <- formatPRBody issue
  pr <- createPR body
  pure $ PRResult pr.number pr.url
```

**Benefits:**
- Never forget steps
- Single source of truth
- Automatic error handling
- Consistent execution
- Testable workflows

## This Is Why ExoMonad DSL Exists

The DSL supports:
- **Arbitrary Haskell effects** - Compose Git, GitHub, LSP, TUI operations
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
- `file_pr` → Enhanced with auto-push, validation, issue links
- `exo_complete` → `complete_and_cleanup` (mark closed + sync + cleanup)

Don't create hybrid systems with both capabilities and workflows exposed. Commit to the workflow model.

## Guidance for Tool Design

### When to Create a New Tool

Create a new MCP tool when you find yourself writing the same prompt instructions repeatedly.

**Indicators:**
- "Remember to push before filing PR"
- "Don't forget to link the issue"
- "Make sure you check for X before doing Y"

These are workflows waiting to be encoded.

### When to Use Bash Instead

Use the Bash tool when:
- The workflow tool doesn't exist yet (valid escape hatch)
- One-off debugging or exploration
- Ad-hoc operations that won't be repeated

Don't create capability tools just to wrap Bash commands.