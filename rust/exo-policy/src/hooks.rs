//! Hooks — `pre_tool_use` (guards / PII-rewrite), `stop` (the live PR-gate), and
//! `session_start` (root identity bootstrap). These are **functions generic over the caps
//! they need** (no `dyn Caps`); the [`RoleDef`](crate::roles::RoleDef) table stores them as
//! `fn(&R, …) -> BoxFuture<…>` monomorphized at the concrete runtime `R`, so the generic
//! bound *is* the per-hook least-privilege spec. See `docs/design/swarm/04-policy.md`.
//!
//! **Status: Wave-3 scaffold.** The decision enums + `HookInput` are the frozen contract;
//! P6 fills in `pre_tool_use` / `stop` / `session_start` (one per concern) with mock-cap
//! tests.

use serde::{Deserialize, Serialize};
use serde_json::Value;

/// A `PreToolUse` verdict. `Modify` rewrites the tool input in place (the PII-rewrite path).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "decision")]
pub enum HookDecision {
    Allow,
    Deny { reason: String },
    Modify { input: Value },
}

/// A `Stop` verdict — the live PR-gate. `Block` keeps the agent in its turn-loop (e.g. an
/// open PR has unaddressed `ChangesRequested`); `Allow` lets it exit.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "decision")]
pub enum StopDecision {
    Allow,
    Block { reason: String },
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
/// this. (Wave-2 wiring firms the full field set; P6 needs `tool_name` + `tool_input`.)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HookInput {
    pub tool_name: String,
    #[serde(default)]
    pub tool_input: Value,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hook_decision_serde_is_tagged() {
        let d = HookDecision::Deny {
            reason: "blocked".into(),
        };
        let j = serde_json::to_value(&d).unwrap();
        assert_eq!(j, serde_json::json!({ "decision": "deny", "reason": "blocked" }));
        let back: HookDecision = serde_json::from_value(j).unwrap();
        assert_eq!(d, back);
    }

    #[test]
    fn session_start_empty_omits_context() {
        let j = serde_json::to_value(SessionStartOutput::default()).unwrap();
        assert_eq!(j, serde_json::json!({}));
    }
}
