# Logging Compliance Audit

**Date:** 2026-02-15
**Policy:** Aggressive Logging (CLAUDE.md)

## Summary

This audit checks compliance with the "Aggressive Logging" policy defined in `CLAUDE.md`. The policy requires:
1.  **Before**: Log what is about to happen (command, params).
2.  **After**: Log exit code, status, result summary.
3.  **Error**: Log stderr, error messages.
4.  **Success**: Log result summary.

## Findings

### HIGH Severity (Silent Success, Weak Error Context)

*   **`rust/exomonad-core/src/services/github_poller.rs`**
    *   **Violation:** Multiple `Command::new("gh")` and `Command::new("git")` calls lack pre-execution and post-execution success logging.
    *   **Locations:**
        *   L79: `gh api` (fetch PRs) - Only warns on failure.
        *   L128: `gh repo view` - Only warns on failure.
        *   L174: `git branch` (scan worktrees) - Silent.
        *   L252, L278, L307: Helper methods.
    *   **Recommendation:** Add `tracing::info!` before execution and after success.

### MEDIUM Severity (Inconsistent or Low Visibility)

*   **`rust/exomonad-core/src/services/agent_control.rs`**
    *   **Violation:** Critical operations like `git worktree add` lack success logs. Cleanup operations are often completely silent (`let _ = ...`).
    *   **Locations:**
        *   L523, L741, L1048, L1192: `git worktree add` - Logs `info!` before, `error!` on fail, but silent on success.
        *   L507, L727, L1026, L1035, L1170, L1179: Cleanup operations - Silent.
        *   L1802, L1924: `zellij rename-pane` - Silent.
    *   **Recommendation:** Add success logs for worktree creation. Add `debug!` logs for cleanup operations.

*   **`rust/exomonad-core/src/services/copilot_review.rs`**
    *   **Violation:** `gh` calls have pre-execution `debug!` logs (good) but lack success logs.
    *   **Locations:**
        *   L57: `gh repo view` - No logs.
        *   L90: `gh api` (comments) - No success log.
        *   L173: `gh api` (reviews) - No success log.
    *   **Recommendation:** Add success logs showing item counts (e.g., "Fetched 5 comments").

*   **`rust/exomonad-core/src/handlers/jj.rs`**
    *   **Violation:** Inconsistent success logging. `error!` not always called before returning error.
    *   **Locations:**
        *   L136, L173, L195: Returns `EffectError` without logging it first.
    *   **Recommendation:** Ensure all error paths log `error!` before returning.

*   **`rust/exomonad-core/src/services/local.rs`**
    *   **Violation:** Uses `trace!` for success logging.
    *   **Locations:** L60.
    *   **Recommendation:** Consider upgrading to `debug!` or `info!` to match "aggressive" policy, or document why `trace!` is sufficient for this low-level executor.

### LOW Severity (Minor Gaps)

*   **`rust/exomonad/src/pid.rs`**: `ps` call (L133) is silent. (Acceptable for rapid polling).
*   **`rust/exomonad/src/recompile.rs`**: Uses `println!` instead of `tracing`. (Acceptable for CLI).
*   **`rust/exomonad-core/src/services/mod.rs`**: Validation checks return errors without internal logging. (Acceptable as they propagate errors).
*   **`rust/exomonad-core/src/services/merge_pr.rs`**: Mostly compliant, minor nitpick on param logging format.
*   **`rust/exomonad-core/src/services/zellij_events.rs`**: Fire-and-forget, logs `debug!` before and `warn!` on error. (Compliant).
*   **`rust/exomonad-core/src/services/popup.rs`**: Very verbose logging. (Compliant).

## Action Plan

1.  Refactor `GitHubPoller` to use a helper with proper logging.
2.  Update `AgentControlService` to log success for worktree operations.
3.  Update `CopilotReview` to log success counts.
4.  Review `LocalExecutor` log levels.
