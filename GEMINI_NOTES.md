# Gemini Code Intelligence Notes

## Audit Session: 2026-02-11

### Findings & Resolutions

#### 1. Resource Leak in `cleanup_agent` (Rust)
- **Severity:** Medium
- **Location:** `rust/exomonad-core/src/services/agent_control.rs`
- **Description:** `spawn_worker` creates a temporary configuration directory at `/tmp/exomonad-agents/{name}`. The `cleanup_agent` method cleans up the git worktree and the persistent config in `.exomonad/agents/`, but **fails to remove** the temporary directory in `/tmp`.
- **Resolution:** **Fixed.** Added cleanup logic to `cleanup_agent` to remove the temporary directory if it exists.

#### 2. Hardcoded `/tmp` Path (Rust)
- **Severity:** Low/Style
- **Location:** `rust/exomonad-core/src/services/agent_control.rs`
- **Resolution:** **Deferred.** Retained hardcoded path in cleanup logic to match the existing creation logic in `spawn_worker`. Future refactoring should address both locations simultaneously to use `std::env::temp_dir()`.

#### 3. Cleanup Path Mismatches (Rust)
- **Severity:** High
- **Location:** `cleanup_agent` in `agent_control.rs`.
- **Description:** `cleanup_agent` uses the `identifier` derived from the Zellij tab name directly for file paths. However, `spawn_*` methods append an agent suffix.
- **Resolution:** **Fixed.** `cleanup_agent` now reconstructs `internal_name` by appending the agent type suffix derived from the Zellij tab emoji.

#### 4. Subrepo Ignored in Cleanup (Rust)
- **Severity:** Medium
- **Location:** `cleanup_agent` in `agent_control.rs`.
- **Resolution:** **Fixed.** Updated `cleanup_agent` to use `effective_project_dir` for resolving the worktree path.

#### 5. Git Branch Naming Collision in `spawn_subtree` (Rust)
- **Severity:** Critical
- **Location:** `spawn_gemini_teammate` in `agent_control.rs`.
- **Description:** Branch names were constructed as `{base_branch}/{name}`, causing git file/directory collisions when spawning recursively (e.g., `base/foo` vs `base/foo/bar`).
- **Resolution:** **Fixed.** Changed the separator from `/` to `-` (e.g., `{base_branch}-{slug}`).

#### 6. UX Complexity & Inconsistency (Rust)
- **Severity:** Low/Style
- **Location:** `spawn_gemini_teammate` vs `spawn_worker` in `agent_control.rs`.
- **Description:** `spawn_subtree` (heavy task) was spawning in a Pane, indistinguishable from `spawn_worker` (light task).
- **Resolution:** **Fixed.** Switched `spawn_gemini_teammate` to use `new_zellij_tab`, creating a clean distinction: Heavy/Isolated agents get Tabs, Light/Shared agents get Panes.

#### 7. Unsafe Agent Naming (Rust)
- **Severity:** Medium
- **Location:** `spawn_gemini_teammate` and `spawn_worker`.
- **Resolution:** **Fixed.** Applied `slugify` to `options.name` for internal names, branch names, and display names to ensure filesystem and git safety.

#### 8. Slug Truncation Bug (Rust)
- **Severity:** Medium
- **Location:** `display_name` in `agent_control.rs`.
- **Description:** Slugs were truncated to 20 chars for display names, making it impossible for `cleanup_agent` to reconstruct the full internal name for cleanup.
- **Resolution:** **Fixed.** Removed truncation from `display_name`.

### Summary
The audit of the `spawn_worker` and `spawn_subtree` paths revealed several critical issues in resource management, git safety, and cleanup reliability. All identified bugs have been fixed and verified with tests. The system now enforces a consistent naming and isolation strategy:
- **Heavy/Isolated (Subtree/GitHub):** Worktree + Tab + Slugified Name (joined by `-`).
- **Light/Shared (Worker):** Shared Dir + Pane + Slugified Name.
- **Cleanup:** Robustly handles all variants by reconstructing internal names from tab metadata.
