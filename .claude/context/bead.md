# Task: Fix: .tidepool/logs directory not created for subagent worktrees

**ID:** tidepool-soj
**Status:** open
**Priority:** 2
**Branch:** bd-soj/fix-tidepoollogs-directory-not-created-for-subagent-worktrees

## Description

Ensure log directories are created during subagent worktree bootstrap

## Context
When spawn_agents creates worktrees, it doesn't create .tidepool/logs/ directory. process-compose tries to tail .tidepool/logs/pc.log and fails. This should be created as part of worktree bootstrap in spawn_agents or the process-compose template.

## Scope Hint
Small - bootstrap script update

## Dependencies

None

## Workflow

1. Implement changes
2. Commit: [tidepool-soj] <description>
3. Push: git push -u origin bd-soj/fix-tidepoollogs-directory-not-created-for-subagent-worktrees
4. File PR: gh pr create --title "[tidepool-soj] Fix: .tidepool/logs directory not created for subagent worktrees"
