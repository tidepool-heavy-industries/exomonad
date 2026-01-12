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

/// A decision tool call from Claude Code.
///
/// When Claude calls a decision tool (e.g., `decision::approve`), we capture
/// the tool name and input for parsing back to Haskell sum types.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ToolCall {
    /// Full tool name (e.g., "decision::approve").
    pub name: String,
    /// Tool input (the branch's field values).
    pub input: serde_json::Value,
}

// ============================================================================
// Exit Code Semantics
// ============================================================================

/// Documents the semantics of process exit codes.
///
/// This enum is for documentation purposes - it defines the contract between
/// mantle and Haskell for interpreting exit codes. The Haskell orchestrator
/// should use these semantics to decide on retry behavior and error reporting.
///
/// # Exit Code Categories
///
/// - **0**: Success - task completed normally
/// - **1**: General error from Claude Code (check stderr/result for details)
/// - **2**: Auth/setup failure - human intervention required, don't retry
/// - **124**: Timeout (from `timeout` command) - may retry with longer timeout
/// - **125**: Docker error (container failed to start)
/// - **126**: Command not executable - missing dependencies or permissions
/// - **127**: Command not found - claude binary missing from container
/// - **137**: SIGKILL (128 + 9) - OOM killer or external termination
/// - **143**: SIGTERM (128 + 15) - graceful termination requested
///
/// # Retry Guidance
///
/// | Exit Code | Retry? | Reason |
/// |-----------|--------|--------|
/// | 0 | N/A | Success |
/// | 1 | Maybe | Depends on error content |
/// | 2 | No | Auth/setup needs human fix |
/// | 124 | Maybe | Timeout - try longer timeout |
/// | 125 | No | Docker misconfigured |
/// | 126 | No | Missing dependencies |
/// | 127 | No | Binary missing |
/// | 137 | Maybe | OOM - try with less context |
/// | 143 | No | Intentionally terminated |
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum ExitReason {
    /// Task completed successfully.
    Success = 0,
    /// General error from Claude Code.
    GeneralError = 1,
    /// Authentication or setup failure (e.g., expired token, invalid API key).
    /// Human intervention required - do not retry automatically.
    AuthSetupFailed = 2,
    /// Timeout from `timeout` command wrapper.
    /// May retry with increased timeout.
    Timeout = 124,
    /// Docker container failed to run (e.g., image not found, daemon error).
    DockerError = 125,
    /// Command found but not executable (permission denied, missing deps).
    CommandNotExecutable = 126,
    /// Command not found (claude binary missing from PATH).
    CommandNotFound = 127,
    /// Process killed by SIGKILL (likely OOM killer or hard timeout).
    /// May retry with reduced context size.
    Killed = 137,
    /// Process terminated by SIGTERM (graceful shutdown requested).
    Terminated = 143,
}

impl ExitReason {
    /// Interpret an exit code into a reason, if known.
    pub fn from_code(code: i32) -> Option<Self> {
        match code {
            0 => Some(Self::Success),
            1 => Some(Self::GeneralError),
            2 => Some(Self::AuthSetupFailed),
            124 => Some(Self::Timeout),
            125 => Some(Self::DockerError),
            126 => Some(Self::CommandNotExecutable),
            127 => Some(Self::CommandNotFound),
            137 => Some(Self::Killed),
            143 => Some(Self::Terminated),
            _ => None,
        }
    }

    /// Whether this exit reason suggests retrying might help.
    pub fn is_retriable(&self) -> bool {
        matches!(self, Self::Timeout | Self::Killed)
    }

    /// Whether this exit reason requires human intervention.
    pub fn needs_human(&self) -> bool {
        matches!(
            self,
            Self::AuthSetupFailed
                | Self::DockerError
                | Self::CommandNotExecutable
                | Self::CommandNotFound
        )
    }

    /// Human-readable description of the exit reason.
    pub fn description(&self) -> &'static str {
        match self {
            Self::Success => "Task completed successfully",
            Self::GeneralError => "Claude Code exited with error",
            Self::AuthSetupFailed => "Authentication or setup failure (check credentials)",
            Self::Timeout => "Process timed out",
            Self::DockerError => "Docker container failed to start",
            Self::CommandNotExecutable => "Command not executable (missing dependencies)",
            Self::CommandNotFound => "Claude binary not found",
            Self::Killed => "Process killed (OOM or external termination)",
            Self::Terminated => "Process terminated (SIGTERM)",
        }
    }
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
    /// Always serialize (Haskell expects the field to be present).
    #[serde(default)]
    pub interrupts: Vec<InterruptSignal>,
    /// Decision tool calls from Claude Code.
    /// Populated when Claude calls a `decision::*` tool.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tool_calls: Option<Vec<ToolCall>>,
    /// Stderr output from Claude Code process.
    /// Captured when process exits with error for debugging auth/setup failures.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub stderr_output: Option<String>,
}

impl RunResult {
    /// Build a RunResult from collected events and process exit status.
    ///
    /// When `stderr_output` is provided and there's no result event, it will be
    /// included in the error message to help diagnose auth/setup failures.
    pub fn from_events(
        events: Vec<StreamEvent>,
        result_event: Option<ResultEvent>,
        exit_code: i32,
        session_tag: Option<String>,
        interrupts: Vec<InterruptSignal>,
        tool_calls: Option<Vec<ToolCall>>,
        stderr_output: Option<String>,
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

        // Build error message for failures
        let (is_error, result) = if let Some(ref re) = result_event {
            (re.is_error, re.result.clone())
        } else {
            // No result event - this is an error. Build informative message.
            let error_msg = Self::build_failure_message(exit_code, stderr_output.as_deref());
            (true, Some(error_msg))
        };

        let result_event = result_event.unwrap_or_else(|| ResultEvent {
            subtype: "error".to_string(),
            is_error: true,
            result: None, // We use our own error message
            session_id: Some(session_id.clone()),
            total_cost_usd: None,
            num_turns: None,
            structured_output: None,
            permission_denials: vec![],
            model_usage: HashMap::new(),
        });

        RunResult {
            exit_code,
            is_error,
            result,
            structured_output: result_event.structured_output.clone(),
            session_id,
            session_tag,
            total_cost_usd: result_event.total_cost_usd.unwrap_or(0.0),
            num_turns: result_event.num_turns.unwrap_or(0),
            events,
            permission_denials: result_event.permission_denials.clone(),
            model_usage: result_event.model_usage.clone(),
            interrupts,
            tool_calls,
            stderr_output,
        }
    }

    /// Build an informative failure message from exit code and stderr.
    fn build_failure_message(exit_code: i32, stderr: Option<&str>) -> String {
        let exit_hint = ExitReason::from_code(exit_code)
            .map(|r| r.description())
            .unwrap_or("Claude Code exited with unexpected error");

        // Extract last N lines of stderr for the error message
        let stderr_excerpt = stderr
            .map(|s| {
                let lines: Vec<&str> = s.lines().collect();
                let last_lines: Vec<&str> = lines.iter().rev().take(10).rev().copied().collect();
                if last_lines.is_empty() {
                    String::new()
                } else {
                    format!("\n\nStderr (last {} lines):\n{}", last_lines.len(), last_lines.join("\n"))
                }
            })
            .unwrap_or_default();

        format!(
            "{} (exit code {}){}",
            exit_hint,
            exit_code,
            stderr_excerpt
        )
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
            tool_calls: None,
            stderr_output: None,
        };

        let json = serde_json::to_string(&result).unwrap();

        // Test round-trip deserialization
        let decoded: RunResult = serde_json::from_str(&json).unwrap();
        assert_eq!(decoded.session_id, "sess-123");
        assert_eq!(decoded.session_tag, Some("test-worktree".to_string()));
        assert_eq!(decoded.num_turns, 5);

        // interrupts is always serialized (Haskell expects the field to be present)
        assert!(json.contains("interrupts"));
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
            tool_calls: None,
            stderr_output: None,
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
            tool_calls: None,
            stderr_output: None,
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

    #[test]
    fn test_build_failure_message_exit_code_2() {
        let msg = RunResult::build_failure_message(2, None);
        assert!(msg.contains("exit code 2"));
        assert!(msg.contains("Authentication or setup failure"));
    }

    #[test]
    fn test_build_failure_message_with_stderr() {
        let stderr = "Error: Invalid API key\nPlease run 'claude auth login'";
        let msg = RunResult::build_failure_message(2, Some(stderr));
        assert!(msg.contains("exit code 2"));
        assert!(msg.contains("Invalid API key"));
        assert!(msg.contains("Stderr"));
    }

    #[test]
    fn test_from_events_no_result_includes_stderr() {
        let events = vec![];
        let stderr = "Auth failed: token expired".to_string();
        let result = RunResult::from_events(
            events,
            None, // No result event - simulates crash
            2,    // Exit code 2 = auth failure
            None,
            vec![],
            None,
            Some(stderr),
        );

        assert!(result.is_error);
        assert_eq!(result.exit_code, 2);
        assert!(result.result.as_ref().unwrap().contains("Authentication or setup failure"));
        assert!(result.result.as_ref().unwrap().contains("token expired"));
        assert_eq!(result.stderr_output, Some("Auth failed: token expired".to_string()));
    }

    #[test]
    fn test_from_events_with_result_event_doesnt_override() {
        // When there IS a result event, we should use its is_error and result
        let result_event = ResultEvent {
            subtype: "success".to_string(),
            is_error: false,
            result: Some("Task completed".to_string()),
            session_id: Some("abc".to_string()),
            total_cost_usd: Some(0.1),
            num_turns: Some(5),
            structured_output: None,
            permission_denials: vec![],
            model_usage: HashMap::new(),
        };
        let result = RunResult::from_events(
            vec![],
            Some(result_event),
            0,
            None,
            vec![],
            None,
            None,
        );

        assert!(!result.is_error);
        assert_eq!(result.result, Some("Task completed".to_string()));
    }
}
