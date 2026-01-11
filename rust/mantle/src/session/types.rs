//! Session types for mantle orchestration.
//!
//! These types are used for:
//! - Storing session metadata in `.mantle/sessions.json`
//! - Returning results to Haskell callers via JSON stdout
//! - Tracking session lifecycle state

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

use mantle_shared::events::{InterruptSignal, ModelUsage};

/// Strip control characters from a string for clean JSON output.
///
/// Removes control chars (0x00-0x08, 0x0B, 0x0C, 0x0E-0x1F, 0x7F) except:
/// - Tab (0x09) - valid whitespace
/// - Newline (0x0A) - valid whitespace
/// - Carriage return (0x0D) - handled by serde_json
fn sanitize_string(s: &str) -> String {
    s.chars()
        .filter(|c| !matches!(c, '\x00'..='\x08' | '\x0b' | '\x0c' | '\x0e'..='\x1f' | '\x7f'))
        .collect()
}

/// Recursively sanitize all string values in a JSON Value.
fn sanitize_json_value(value: &mut serde_json::Value) {
    match value {
        serde_json::Value::String(s) => {
            *s = sanitize_string(s);
        }
        serde_json::Value::Array(arr) => {
            for item in arr {
                sanitize_json_value(item);
            }
        }
        serde_json::Value::Object(obj) => {
            for (_, v) in obj.iter_mut() {
                sanitize_json_value(v);
            }
        }
        _ => {}
    }
}

/// Session lifecycle state.
///
/// Tracks where a session is in its lifecycle. Used for filtering
/// and for determining valid operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, clap::ValueEnum)]
#[serde(rename_all = "snake_case")]
pub enum SessionState {
    /// Session created, worktree ready, not yet run
    Pending,
    /// Session is currently running in a container
    Running,
    /// Session completed successfully (exit code 0)
    Completed,
    /// Session failed (non-zero exit code or error)
    Failed,
    /// Session was manually cancelled
    Cancelled,
}

impl std::fmt::Display for SessionState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SessionState::Pending => write!(f, "pending"),
            SessionState::Running => write!(f, "running"),
            SessionState::Completed => write!(f, "completed"),
            SessionState::Failed => write!(f, "failed"),
            SessionState::Cancelled => write!(f, "cancelled"),
        }
    }
}

/// Metadata stored for each session.
///
/// This is persisted in `.mantle/sessions.json` and used for:
/// - Resuming sessions (need branch, worktree, cc_session_id)
/// - Forking sessions (need parent_id)
/// - Listing/querying sessions (need state, timestamps)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionMetadata {
    /// Unique session ID (UUID v4)
    pub id: String,

    /// Semantic slug provided by caller (e.g., "implement/user-auth")
    pub slug: String,

    /// Generated branch name: `{slug}-{hex}`
    pub branch: String,

    /// Path to the git worktree
    pub worktree: PathBuf,

    /// Model used (haiku, sonnet, opus)
    pub model: String,

    /// Current lifecycle state
    pub state: SessionState,

    /// Creation timestamp
    pub created_at: DateTime<Utc>,

    /// Last update timestamp
    pub updated_at: DateTime<Utc>,

    /// Parent session ID (for forked sessions)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent_id: Option<String>,

    /// Child session IDs (sessions forked from this one)
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub child_ids: Vec<String>,

    /// Claude Code session ID (from stream init event)
    /// This is needed for --resume and --fork-session flags
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cc_session_id: Option<String>,

    /// Last container ID (when running in Docker)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub container_id: Option<String>,

    /// Accumulated cost across all runs
    #[serde(default)]
    pub total_cost_usd: f64,

    /// Total turns across all runs
    #[serde(default)]
    pub total_turns: i64,

    /// Last exit code
    #[serde(skip_serializing_if = "Option::is_none")]
    pub last_exit_code: Option<i32>,
}

impl SessionMetadata {
    /// Create a new session metadata entry.
    pub fn new(id: String, slug: String, branch: String, worktree: PathBuf, model: String) -> Self {
        let now = Utc::now();
        Self {
            id,
            slug,
            branch,
            worktree,
            model,
            state: SessionState::Pending,
            created_at: now,
            updated_at: now,
            parent_id: None,
            child_ids: Vec::new(),
            cc_session_id: None,
            container_id: None,
            total_cost_usd: 0.0,
            total_turns: 0,
            last_exit_code: None,
        }
    }

    /// Create a forked session from a parent.
    pub fn fork_from(
        parent: &SessionMetadata,
        id: String,
        slug: String,
        branch: String,
        worktree: PathBuf,
    ) -> Self {
        let now = Utc::now();
        Self {
            id,
            slug,
            branch,
            worktree,
            model: parent.model.clone(),
            state: SessionState::Pending,
            created_at: now,
            updated_at: now,
            parent_id: Some(parent.id.clone()),
            child_ids: Vec::new(),
            // Inherit CC session ID for --fork-session
            cc_session_id: parent.cc_session_id.clone(),
            container_id: None,
            total_cost_usd: 0.0,
            total_turns: 0,
            last_exit_code: None,
        }
    }

    /// Mark session as running.
    pub fn mark_running(&mut self, container_id: Option<String>) {
        self.state = SessionState::Running;
        self.container_id = container_id;
        self.updated_at = Utc::now();
    }

    /// Mark session as completed with results.
    pub fn mark_completed(&mut self, exit_code: i32, cost: f64, turns: i64, cc_session_id: Option<String>) {
        self.state = if exit_code == 0 {
            SessionState::Completed
        } else {
            SessionState::Failed
        };
        self.last_exit_code = Some(exit_code);
        self.total_cost_usd += cost;
        self.total_turns += turns;
        if let Some(sid) = cc_session_id {
            self.cc_session_id = Some(sid);
        }
        self.container_id = None;
        self.updated_at = Utc::now();
    }

    /// Mark session as cancelled.
    pub fn mark_cancelled(&mut self) {
        self.state = SessionState::Cancelled;
        self.container_id = None;
        self.updated_at = Utc::now();
    }
}

/// Output returned to Haskell callers.
///
/// This is printed as JSON to stdout after each session command.
/// The field names use snake_case to match Haskell's deriving-aeson expectations.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionOutput {
    /// Session ID (mantle's UUID)
    pub session_id: String,

    /// Git branch name
    pub branch: String,

    /// Worktree path
    pub worktree: PathBuf,

    /// Container/process exit code (0 = success)
    pub exit_code: i32,

    /// Whether Claude reported an error
    pub is_error: bool,

    /// Final result text from Claude (if any)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result_text: Option<String>,

    /// Structured output (if --json-schema was used)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub structured_output: Option<serde_json::Value>,

    /// Total API cost for this run (USD)
    pub total_cost_usd: f64,

    /// Number of turns in this run
    pub num_turns: i64,

    /// Interrupt signals received during the run
    #[serde(default)]
    pub interrupts: Vec<InterruptSignal>,

    /// Wall-clock duration in seconds
    pub duration_secs: f64,

    /// Error message (when is_error = true or exit_code != 0)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,

    /// Per-model token usage
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub model_usage: HashMap<String, ModelUsage>,

    /// Claude Code session ID (for resume/fork)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cc_session_id: Option<String>,

    /// Decision tool calls from Claude Code.
    /// Populated when Claude calls a `decision::*` tool.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tool_calls: Option<Vec<mantle_shared::events::ToolCall>>,
}

impl SessionOutput {
    /// Create a successful output from run results.
    pub fn success(
        session_id: String,
        branch: String,
        worktree: PathBuf,
        result: &mantle_shared::RunResult,
        duration_secs: f64,
    ) -> Self {
        Self {
            session_id,
            branch,
            worktree,
            exit_code: result.exit_code,
            is_error: result.is_error,
            result_text: result.result.clone(),
            structured_output: result.structured_output.clone(),
            total_cost_usd: result.total_cost_usd,
            num_turns: result.num_turns,
            interrupts: result.interrupts.clone(),
            duration_secs,
            error: None,
            model_usage: result.model_usage.clone(),
            cc_session_id: Some(result.session_id.clone()),
            tool_calls: result.tool_calls.clone(),
        }
    }

    /// Create an error output.
    pub fn error(
        session_id: String,
        branch: String,
        worktree: PathBuf,
        error_msg: String,
        duration_secs: f64,
    ) -> Self {
        Self {
            session_id,
            branch,
            worktree,
            exit_code: 1,
            is_error: true,
            result_text: None,
            structured_output: None,
            total_cost_usd: 0.0,
            num_turns: 0,
            interrupts: Vec::new(),
            duration_secs,
            error: Some(error_msg),
            model_usage: HashMap::new(),
            cc_session_id: None,
            tool_calls: None,
        }
    }

    /// Sanitize string fields to remove control characters that would break JSON parsing.
    ///
    /// This should be called before serializing to JSON for Haskell consumption.
    pub fn sanitize(&mut self) {
        // Sanitize result_text
        if let Some(ref mut text) = self.result_text {
            *text = sanitize_string(text);
        }

        // Sanitize error message
        if let Some(ref mut err) = self.error {
            *err = sanitize_string(err);
        }

        // Recursively sanitize structured_output
        if let Some(ref mut output) = self.structured_output {
            sanitize_json_value(output);
        }
    }
}

/// Generate a unique branch name from a slug.
///
/// Appends a 6-character hex suffix from a UUID to ensure uniqueness.
///
/// # Examples
///
/// ```
/// use mantle::session::types::generate_branch_name;
///
/// let branch = generate_branch_name("implement/user-auth");
/// assert!(branch.starts_with("implement/user-auth-"));
/// assert_eq!(branch.len(), "implement/user-auth-".len() + 6);
/// ```
pub fn generate_branch_name(slug: &str) -> String {
    let uuid = uuid::Uuid::new_v4();
    let hex = &uuid.to_string()[..6];
    format!("{}-{}", slug, hex)
}

/// Generate a unique session ID.
pub fn generate_session_id() -> String {
    uuid::Uuid::new_v4().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_branch_name() {
        let branch = generate_branch_name("implement/user-auth");
        assert!(branch.starts_with("implement/user-auth-"));
        assert_eq!(branch.len(), "implement/user-auth-".len() + 6);

        // Different calls should produce different branches
        let branch2 = generate_branch_name("implement/user-auth");
        assert_ne!(branch, branch2);
    }

    #[test]
    fn test_session_state_display() {
        assert_eq!(SessionState::Pending.to_string(), "pending");
        assert_eq!(SessionState::Running.to_string(), "running");
        assert_eq!(SessionState::Completed.to_string(), "completed");
        assert_eq!(SessionState::Failed.to_string(), "failed");
        assert_eq!(SessionState::Cancelled.to_string(), "cancelled");
    }

    #[test]
    fn test_session_metadata_new() {
        let meta = SessionMetadata::new(
            "test-id".to_string(),
            "implement/auth".to_string(),
            "implement/auth-abc123".to_string(),
            PathBuf::from("/tmp/worktree"),
            "sonnet".to_string(),
        );

        assert_eq!(meta.id, "test-id");
        assert_eq!(meta.slug, "implement/auth");
        assert_eq!(meta.branch, "implement/auth-abc123");
        assert_eq!(meta.state, SessionState::Pending);
        assert!(meta.parent_id.is_none());
        assert!(meta.cc_session_id.is_none());
    }

    #[test]
    fn test_session_metadata_lifecycle() {
        let mut meta = SessionMetadata::new(
            "test-id".to_string(),
            "test".to_string(),
            "test-abc123".to_string(),
            PathBuf::from("/tmp"),
            "sonnet".to_string(),
        );

        // Mark running
        meta.mark_running(Some("container-123".to_string()));
        assert_eq!(meta.state, SessionState::Running);
        assert_eq!(meta.container_id, Some("container-123".to_string()));

        // Mark completed
        meta.mark_completed(0, 0.05, 3, Some("cc-session-456".to_string()));
        assert_eq!(meta.state, SessionState::Completed);
        assert_eq!(meta.last_exit_code, Some(0));
        assert!((meta.total_cost_usd - 0.05).abs() < 0.001);
        assert_eq!(meta.total_turns, 3);
        assert_eq!(meta.cc_session_id, Some("cc-session-456".to_string()));
        assert!(meta.container_id.is_none());
    }

    #[test]
    fn test_session_metadata_serialization() {
        let meta = SessionMetadata::new(
            "test-id".to_string(),
            "test".to_string(),
            "test-abc123".to_string(),
            PathBuf::from("/tmp"),
            "sonnet".to_string(),
        );

        let json = serde_json::to_string(&meta).unwrap();
        let decoded: SessionMetadata = serde_json::from_str(&json).unwrap();

        assert_eq!(decoded.id, meta.id);
        assert_eq!(decoded.slug, meta.slug);
        assert_eq!(decoded.branch, meta.branch);
    }

    #[test]
    fn test_session_output_serialization() {
        let output = SessionOutput::error(
            "test-id".to_string(),
            "test-abc123".to_string(),
            PathBuf::from("/tmp"),
            "Something went wrong".to_string(),
            1.5,
        );

        let json = serde_json::to_string(&output).unwrap();
        assert!(json.contains("\"session_id\":\"test-id\""));
        assert!(json.contains("\"exit_code\":1"));
        assert!(json.contains("\"is_error\":true"));
        assert!(json.contains("\"error\":\"Something went wrong\""));
    }
}
