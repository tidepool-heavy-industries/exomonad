//! Event types for Claude Code stream parsing.
//!
//! These types represent the JSON events emitted by Claude Code in
//! `--output-format stream-json` mode.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ============================================================================
// Interrupt Signal Types
// ============================================================================

/// An interrupt signal sent by Claude via `mantle signal`.
///
/// Used for out-of-band communication from Claude Code to the orchestrator,
/// typically for graph state transitions or escalation requests.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct InterruptSignal {
    /// Signal type: "transition", "escalate", "request_review", etc.
    pub signal_type: String,
    /// Target state for transitions (e.g., "need_more_types").
    #[serde(skip_serializing_if = "Option::is_none")]
    pub state: Option<String>,
    /// Human-readable reason for the signal.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,
}

// ============================================================================
// Stream Event Types (for parsing Claude Code's stream-json output)
// ============================================================================

/// A single event from Claude Code's stream-json output.
/// Each line of output is one of these variants.
#[derive(Deserialize, Serialize, Debug, Clone)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
pub enum StreamEvent {
    System(SystemEvent),
    Assistant(AssistantEvent),
    User(UserEvent),
    Result(ResultEvent),
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct SystemEvent {
    pub subtype: String,
    pub session_id: String,
    #[serde(default)]
    pub tools: Vec<String>,
    pub model: String,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct AssistantEvent {
    pub message: AssistantMessage,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct AssistantMessage {
    #[serde(default)]
    pub content: Vec<ContentBlock>,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
pub enum ContentBlock {
    Text {
        text: String,
    },
    ToolUse {
        name: String,
        id: String,
        input: serde_json::Value,
    },
    ToolResult {
        tool_use_id: String,
        content: String,
        is_error: Option<bool>,
    },
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct UserEvent {
    #[serde(default)]
    pub tool_use_result: Option<String>,
    #[serde(default)]
    pub message: Option<UserMessage>,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct UserMessage {
    #[serde(default)]
    pub content: Vec<ContentBlock>,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct ResultEvent {
    pub subtype: String,
    pub is_error: bool,
    pub result: Option<String>,
    pub session_id: Option<String>,
    pub total_cost_usd: Option<f64>,
    pub num_turns: Option<i64>,
    pub structured_output: Option<serde_json::Value>,
    #[serde(default)]
    pub permission_denials: Vec<PermissionDenial>,
    #[serde(default, rename = "modelUsage")]
    pub model_usage: HashMap<String, ModelUsage>,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct PermissionDenial {
    pub tool_name: String,
    pub tool_use_id: String,
    #[serde(default)]
    pub tool_input: serde_json::Value,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ModelUsage {
    #[serde(default)]
    pub input_tokens: i64,
    #[serde(default)]
    pub output_tokens: i64,
    #[serde(default)]
    pub cache_read_input_tokens: i64,
    #[serde(default)]
    pub cache_creation_input_tokens: i64,
    #[serde(default)]
    pub cost_usd: f64,
}

// ============================================================================
// Output Types (what mantle returns to callers)
// ============================================================================

/// The final result returned by mantle to the Haskell orchestrator.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct RunResult {
    /// Exit code from claude process.
    pub exit_code: i32,
    /// Whether Claude Code reported an error.
    pub is_error: bool,
    /// Prose result from Claude Code.
    pub result: Option<String>,
    /// Structured output (when --json-schema was provided).
    pub structured_output: Option<serde_json::Value>,
    /// Session ID (available immediately from init event).
    pub session_id: String,
    /// Tag for correlating with orchestrator state (e.g., worktree name).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session_tag: Option<String>,
    /// Cost in USD.
    pub total_cost_usd: f64,
    /// Number of turns (tool use iterations).
    pub num_turns: i64,
    /// Full event stream for debugging/replay.
    pub events: Vec<StreamEvent>,
    /// Permission denials with details.
    pub permission_denials: Vec<PermissionDenial>,
    /// Per-model usage breakdown.
    pub model_usage: HashMap<String, ModelUsage>,
    /// Interrupt signals received during execution.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub interrupts: Vec<InterruptSignal>,
}

impl RunResult {
    /// Build a RunResult from collected events and process exit status.
    pub fn from_events(
        events: Vec<StreamEvent>,
        result_event: Option<ResultEvent>,
        exit_code: i32,
        session_tag: Option<String>,
        interrupts: Vec<InterruptSignal>,
    ) -> Self {
        // Extract session_id from init event
        let session_id = events
            .iter()
            .find_map(|e| {
                if let StreamEvent::System(s) = e {
                    Some(s.session_id.clone())
                } else {
                    None
                }
            })
            .or_else(|| result_event.as_ref().and_then(|r| r.session_id.clone()))
            .unwrap_or_else(|| "unknown".to_string());

        let result_event = result_event.unwrap_or_else(|| ResultEvent {
            subtype: "error".to_string(),
            is_error: true,
            result: Some("No result event received".to_string()),
            session_id: Some(session_id.clone()),
            total_cost_usd: None,
            num_turns: None,
            structured_output: None,
            permission_denials: vec![],
            model_usage: HashMap::new(),
        });

        RunResult {
            exit_code,
            is_error: result_event.is_error,
            result: result_event.result.clone(),
            structured_output: result_event.structured_output.clone(),
            session_id,
            session_tag,
            total_cost_usd: result_event.total_cost_usd.unwrap_or(0.0),
            num_turns: result_event.num_turns.unwrap_or(0),
            events,
            permission_denials: result_event.permission_denials.clone(),
            model_usage: result_event.model_usage.clone(),
            interrupts,
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE_INIT: &str = r#"{"type":"system","subtype":"init","session_id":"abc-123","tools":["Read","Glob"],"model":"claude-sonnet-4-20250514"}"#;
    const SAMPLE_TEXT: &str =
        r#"{"type":"assistant","message":{"content":[{"type":"text","text":"hello world"}]}}"#;
    const SAMPLE_TOOL: &str = r#"{"type":"assistant","message":{"content":[{"type":"tool_use","name":"Read","id":"toolu_123","input":{"file_path":"/foo/bar"}}]}}"#;
    const SAMPLE_RESULT: &str = r#"{"type":"result","subtype":"success","is_error":false,"result":"done","session_id":"abc-123","total_cost_usd":0.05,"num_turns":2,"permission_denials":[],"modelUsage":{}}"#;
    const SAMPLE_RESULT_WITH_USAGE: &str = r#"{"type":"result","subtype":"success","is_error":false,"result":"done","session_id":"abc-123","total_cost_usd":0.15,"num_turns":3,"permission_denials":[],"modelUsage":{"claude-sonnet-4-20250514":{"inputTokens":100,"outputTokens":50,"cacheReadInputTokens":1000,"cacheCreationInputTokens":500,"costUSD":0.15}}}"#;

    #[test]
    fn parse_system_event() {
        let event: StreamEvent = serde_json::from_str(SAMPLE_INIT).unwrap();
        assert!(matches!(event, StreamEvent::System(_)));
        if let StreamEvent::System(s) = event {
            assert_eq!(s.session_id, "abc-123");
            assert_eq!(s.model, "claude-sonnet-4-20250514");
            assert_eq!(s.tools, vec!["Read", "Glob"]);
        }
    }

    #[test]
    fn parse_assistant_text() {
        let event: StreamEvent = serde_json::from_str(SAMPLE_TEXT).unwrap();
        if let StreamEvent::Assistant(a) = event {
            assert_eq!(a.message.content.len(), 1);
            if let ContentBlock::Text { text } = &a.message.content[0] {
                assert_eq!(text, "hello world");
            } else {
                panic!("Expected Text block");
            }
        } else {
            panic!("Expected Assistant event");
        }
    }

    #[test]
    fn parse_assistant_tool_use() {
        let event: StreamEvent = serde_json::from_str(SAMPLE_TOOL).unwrap();
        if let StreamEvent::Assistant(a) = event {
            assert_eq!(a.message.content.len(), 1);
            if let ContentBlock::ToolUse { name, id, input } = &a.message.content[0] {
                assert_eq!(name, "Read");
                assert_eq!(id, "toolu_123");
                assert_eq!(input["file_path"], "/foo/bar");
            } else {
                panic!("Expected ToolUse block");
            }
        } else {
            panic!("Expected Assistant event");
        }
    }

    #[test]
    fn parse_result_event() {
        let event: StreamEvent = serde_json::from_str(SAMPLE_RESULT).unwrap();
        if let StreamEvent::Result(r) = event {
            assert!(!r.is_error);
            assert_eq!(r.subtype, "success");
            assert_eq!(r.result, Some("done".to_string()));
            assert_eq!(r.session_id, Some("abc-123".to_string()));
            assert_eq!(r.num_turns, Some(2));
            assert!((r.total_cost_usd.unwrap() - 0.05).abs() < 0.001);
        } else {
            panic!("Expected Result event");
        }
    }

    #[test]
    fn parse_result_with_model_usage() {
        let event: StreamEvent = serde_json::from_str(SAMPLE_RESULT_WITH_USAGE).unwrap();
        if let StreamEvent::Result(r) = event {
            assert_eq!(r.model_usage.len(), 1);
            let usage = r.model_usage.get("claude-sonnet-4-20250514").unwrap();
            assert_eq!(usage.input_tokens, 100);
            assert_eq!(usage.output_tokens, 50);
            assert_eq!(usage.cache_read_input_tokens, 1000);
        } else {
            panic!("Expected Result event");
        }
    }

    #[test]
    fn parse_full_stream() {
        let stream = format!(
            "{}\n{}\n{}\n{}",
            SAMPLE_INIT, SAMPLE_TEXT, SAMPLE_TOOL, SAMPLE_RESULT
        );
        let events: Vec<StreamEvent> = stream
            .lines()
            .map(|l| serde_json::from_str(l).unwrap())
            .collect();

        assert_eq!(events.len(), 4);
        assert!(matches!(&events[0], StreamEvent::System(_)));
        assert!(matches!(&events[1], StreamEvent::Assistant(_)));
        assert!(matches!(&events[2], StreamEvent::Assistant(_)));
        assert!(matches!(&events[3], StreamEvent::Result(_)));
    }

    #[test]
    fn run_result_serialization() {
        let result = RunResult {
            exit_code: 0,
            is_error: false,
            result: Some("test".to_string()),
            structured_output: None,
            session_id: "sess-123".to_string(),
            session_tag: Some("test-worktree".to_string()),
            total_cost_usd: 0.1,
            num_turns: 5,
            events: vec![],
            permission_denials: vec![],
            model_usage: HashMap::new(),
            interrupts: vec![],
        };

        let json = serde_json::to_string(&result).unwrap();

        // Test round-trip deserialization
        let decoded: RunResult = serde_json::from_str(&json).unwrap();
        assert_eq!(decoded.session_id, "sess-123");
        assert_eq!(decoded.session_tag, Some("test-worktree".to_string()));
        assert_eq!(decoded.num_turns, 5);

        // Empty interrupts should be omitted from JSON
        assert!(!json.contains("interrupts"));
    }

    #[test]
    fn run_result_without_tag() {
        let result = RunResult {
            exit_code: 0,
            is_error: false,
            result: Some("test".to_string()),
            structured_output: None,
            session_id: "sess-123".to_string(),
            session_tag: None,
            total_cost_usd: 0.1,
            num_turns: 5,
            events: vec![],
            permission_denials: vec![],
            model_usage: HashMap::new(),
            interrupts: vec![],
        };

        let json = serde_json::to_string(&result).unwrap();

        // Test round-trip deserialization
        let decoded: RunResult = serde_json::from_str(&json).unwrap();
        assert_eq!(decoded.session_id, "sess-123");
        assert_eq!(decoded.session_tag, None);

        // session_tag should be omitted from JSON when None
        assert!(!json.contains("session_tag"));
    }

    #[test]
    fn run_result_with_interrupt() {
        let result = RunResult {
            exit_code: 0,
            is_error: false,
            result: Some("test".to_string()),
            structured_output: None,
            session_id: "sess-123".to_string(),
            session_tag: None,
            total_cost_usd: 0.1,
            num_turns: 5,
            events: vec![],
            permission_denials: vec![],
            model_usage: HashMap::new(),
            interrupts: vec![InterruptSignal {
                signal_type: "transition".to_string(),
                state: Some("need_more_types".to_string()),
                reason: Some("Missing Foo type".to_string()),
            }],
        };

        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("\"interrupts\""));
        assert!(json.contains("\"signal_type\":\"transition\""));
        assert!(json.contains("\"state\":\"need_more_types\""));
    }

    #[test]
    fn interrupt_signal_serialization() {
        let signal = InterruptSignal {
            signal_type: "escalate".to_string(),
            state: None,
            reason: Some("Need human review".to_string()),
        };

        let json = serde_json::to_string(&signal).unwrap();
        assert!(json.contains("\"signal_type\":\"escalate\""));
        assert!(json.contains("\"reason\":\"Need human review\""));
        // state should be omitted when None
        assert!(!json.contains("\"state\""));
    }
}
