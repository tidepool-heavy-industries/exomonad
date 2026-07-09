//! The hook **contract**: the decision enums (`pre_tool_use` → [`HookDecision`], `session_start` →
//! [`SessionStartOutput`]) and the parsed [`HookInput`] view.
//!
//! These are the frozen types the [`RoleDef`](crate::roles::RoleDef) fn-pointers return; the
//! concrete gate bodies that produce them are domain code (they live in the `exo` usage crate),
//! written generic over the caps they need. The contract here names no caps and does no IO.
//!
//! There used to be a `stop` → `StopDecision` hook here too (Claude Code's `Stop` event). It was
//! removed: `Stop` fires on every turn-end, including a node legitimately yielding to wait on a
//! backgrounded async task, so it cannot distinguish "genuinely done" from "paused" — every
//! consequential decision built on it (a reviewer-abort-and-kill gate, a dirty-worktree exit-block)
//! was provably wrong some of the time. See `exo_framework::Exomonad::handle_tick` for the
//! wall-clock-timeout replacement and `rust/exo/CLAUDE.md` for the full account.

use exo_caps::{Reason, ToolName};
use serde::{Deserialize, Serialize};
use serde_json::Value;

/// A `PreToolUse` verdict. `Modify` rewrites the tool input in place (the PII-rewrite path).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "decision")]
pub enum HookDecision {
    Allow,
    Deny { reason: Reason },
    Modify { input: Value },
}

/// A `SessionStart` outcome — the additional context injected into the agent's conversation
/// (the root identity bootstrap; empty for non-root). Mirrors CC's `additionalContext`.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct SessionStartOutput {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub additional_context: Option<String>,
}

/// The parsed CC hook payload `pre_tool_use` inspects. A thin typed view over the fields the
/// guard/PII logic actually reads — the sidecar's `hook` mode parses the raw CC JSON into
/// this. (Wave-2 wiring firms the full field set; the gate needs `tool_name` + `tool_input`.)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HookInput {
    pub tool_name: ToolName,
    #[serde(default)]
    pub tool_input: Value,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hook_decision_serde_is_tagged() {
        let d = HookDecision::Deny {
            reason: Reason::new("blocked".into()).unwrap(),
        };
        let j = serde_json::to_value(&d).unwrap();
        assert_eq!(
            j,
            serde_json::json!({ "decision": "deny", "reason": "blocked" })
        );
        let back: HookDecision = serde_json::from_value(j).unwrap();
        assert_eq!(d, back);
    }

    #[test]
    fn session_start_empty_omits_context() {
        let j = serde_json::to_value(SessionStartOutput::default()).unwrap();
        assert_eq!(j, serde_json::json!({}));
    }
}
