//! **N4 — Hook mode.** `exomonad hook <event>` reads the CC hook payload on stdin, self-IDs
//! (same papers + exo-scry path as the sidecar), runs the role's `exo-policy` hook, and emits
//! the verdict on stdout. **No central server** — the hook is a short-lived process that
//! builds a `Runtime` and calls policy directly.
//!
//! - `pre_tool_use` → `HookDecision` (default-allow antipattern *nudge*, NOT a gate — C3).
//! - `stop` → `StopDecision` (the live PR-gate: open PR with unaddressed ChangesRequested).
//! - `session_start` → `SessionStartOutput`. **Must do the REAL papers-based root identity
//!   bootstrap** (currently a `default()` no-op): inject `additionalContext` describing the
//!   node's tree identity (role/path/parent) so a fresh agent knows who it is. See doc 01.
//!
//! Reads the role's hook fns from `exo_policy::role_def::<Runtime>(kind)`.
//!
//! **Status: stub (N4 leaf fills this).** Acceptance: a `pre_tool_use` payload for an
//! unrecognized tool → `{"decision":"allow"}`; a `session_start` for a non-root node →
//! `additionalContext` naming its role + parent.

use std::path::Path;

use crate::error::NodeResult;

/// The CC hook events this mode handles (mirrors the policy hook fns).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HookEvent {
    PreToolUse,
    Stop,
    SessionStart,
}

/// Handle one CC hook invocation: read stdin payload, self-ID via `papers_path`, run the
/// role's policy hook, write the verdict JSON to stdout.
pub async fn handle(event: HookEvent, papers_path: &Path, stdin_json: &str) -> NodeResult<String> {
    let _ = (event, papers_path, stdin_json);
    todo!("N4: bootstrap Runtime from papers; run role_def(kind) pre_tool_use/stop/session_start; emit verdict JSON")
}
