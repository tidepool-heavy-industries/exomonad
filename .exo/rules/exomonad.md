---
description: "ExoMonad v2 agent orchestration rules for projects using exo"
---

# ExoMonad Agent Rules

This project uses ExoMonad v2 Node-Mode through the `exo` binary. Treat the role charter injected
at launch as the authoritative description of your responsibilities and available orchestration
tools. Explicit instructions from the human or a parent agent take precedence over defaults.

## Operating model

ExoMonad unfolds work into scoped child contexts and folds completed branches back into their
parents. Root and TL agents are managers, but management is a perspective rather than a ban on
direct work: use judgment to plan, scaffold, coordinate, investigate, integrate, or implement when
that best advances the assigned outcome.

- Root owns the user's intent and final result.
- TL owns a subtree, its children, integration, and a complete submission upward.
- Dev owns a focused branch slice and submits it when merge-ready.
- Worker performs a bounded task in its parent's worktree and reports upward.
- Reviewer, when enabled, independently judges a submitted slice.

Discover and use the ExoMonad tools actually exposed to your role. Tool descriptions define
mechanics; they do not override the role charter or task-specific instructions. Git operations use
the `git` CLI directly.

## Scaffold, fork, converge

When delegation helps:

1. Create and commit any shared scaffold children need: interfaces, fixtures, stubs, tests, local
   guidance, or actionable `todo!()` markers. A scaffold is a decomposition artifact and may be
   temporarily incomplete or fail compilation when its state and intended next actions are clear.
2. Spawn independent siblings in the same wave. Give each child an object-level objective,
   observable done criteria, useful local context, mechanically checked scope, relevant constraints,
   and verification. Avoid duplicating generic role ritual in the task prompt.
3. Continue useful work while children run. Child events are pushed; mandatory polling and idle
   turns are unnecessary.
4. Merge completed child branches sequentially, integrate their outputs, and run proportionate
   verification.
5. Submit the complete subtree to the parent when it is merge-ready.

Fresh child contexts are the default. Inherit a parent session only when the task genuinely needs
that context. Reviewers are optional and controlled by project configuration or an explicit
mid-flight review request.

## Submission boundary

`submit_branch` means the assigned slice is complete, committed, verified as appropriate, and ready
for the parent to merge. It is not a status update. Use the role's messaging tool for progress,
questions, failures, and bounded worker results.

Before submission:

- keep changes inside the assigned boundary, or report and justify deviations;
- incorporate the current parent branch when required by the submission gate;
- run the relevant checks and preserve useful receipts;
- leave the worktree clean and commit the result.

The parent owns the fold decision and post-merge integration. An independent reviewer, when
enabled, is an additional gate rather than a substitute for parent judgment.

## Architecture boundary

These rules describe v2 Node-Mode. Classic `exomonad` workflows may still exist for compatibility,
but their PR-based tools, polling loops, and lifecycle language do not define the v2 experience.
