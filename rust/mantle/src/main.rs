//! mantle: Spawn and manage Claude Code sessions.
//!
//! CLI tool for running Claude Code sessions with process supervision,
//! timeout handling, and inter-process communication.
//!
//! ## Commands
//!
//! - `session` - Manage sessions (start, continue, fork)
//!
//! ## Stateless Design
//!
//! Mantle is stateless - all session data (cc_session_id, worktree, branch, model)
//! is passed as CLI arguments. The hub tracks session metadata for observability.

use clap::{Parser, Subcommand};
use tracing::error;
use tracing_subscriber::EnvFilter;

use mantle::session::{
    continue_session, fork_session, start_session,
    ContinueConfig, ForkConfig, StartConfig,
};

// ============================================================================
// CLI Types
// ============================================================================

#[derive(Parser)]
#[command(name = "mantle")]
#[command(about = "Spawn and manage Claude Code sessions")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Manage Claude Code sessions
    Session {
        #[command(subcommand)]
        command: SessionCommands,
    },
}

#[derive(Subcommand)]
enum SessionCommands {
    /// Start a new session
    Start {
        /// Semantic slug for the session (e.g., "implement/user-auth")
        #[arg(long)]
        slug: String,

        /// Prompt text for Claude Code
        #[arg(long)]
        prompt: String,

        /// Model to use (haiku, sonnet, opus)
        #[arg(long, default_value = "sonnet")]
        model: String,

        /// Timeout in seconds (0 = no timeout)
        #[arg(long, default_value = "0")]
        timeout: u64,

        /// JSON schema for structured output (passed to Claude Code --json-schema)
        #[arg(long)]
        json_schema: Option<String>,

        /// Path to file containing JSON array of MCP decision tools for sum type outputs
        #[arg(long)]
        decision_tools_file: Option<String>,

        // === Graph Execution Tracking ===

        /// Hub session ID (for registering nodes in existing hub session)
        /// When provided, registers this node into an existing hub session
        /// instead of creating a new one
        #[arg(long)]
        hub_session_id: Option<String>,

        /// Execution ID (e.g., "run-1") - human-readable run identifier
        #[arg(long)]
        execution_id: Option<String>,

        /// Node path in graph (e.g., "n0" for root, "n2.n0" for nested)
        #[arg(long)]
        node_path: Option<String>,

        /// Node type/handler name (e.g., "hTypes", "hImpl")
        #[arg(long)]
        node_type: Option<String>,

        /// Parent hub node ID (for tree structure in hub)
        #[arg(long)]
        parent_hub_node_id: Option<String>,
    },

    /// Continue an existing session with a new prompt
    Continue {
        /// Claude Code session ID (for --resume)
        #[arg(long)]
        cc_session_id: String,

        /// Worktree path where code lives
        #[arg(long)]
        worktree: String,

        /// Git branch name
        #[arg(long)]
        branch: String,

        /// Model to use (haiku, sonnet, opus)
        #[arg(long)]
        model: String,

        /// New prompt text
        #[arg(long)]
        prompt: String,

        /// Timeout in seconds (0 = no timeout)
        #[arg(long, default_value = "0")]
        timeout: u64,

        /// Path to file containing JSON array of MCP decision tools for sum type outputs
        #[arg(long)]
        decision_tools_file: Option<String>,
    },

    /// Fork a session (create child session from parent's context)
    Fork {
        /// Parent's Claude Code session ID (for --fork-session)
        #[arg(long)]
        parent_cc_session_id: String,

        /// Parent's worktree path
        #[arg(long)]
        parent_worktree: String,

        /// Parent's git branch
        #[arg(long)]
        parent_branch: String,

        /// Model to use (haiku, sonnet, opus)
        #[arg(long)]
        model: String,

        /// Slug for the child session
        #[arg(long)]
        child_slug: String,

        /// Prompt for the child session
        #[arg(long)]
        child_prompt: String,

        /// Timeout in seconds (0 = no timeout)
        #[arg(long, default_value = "0")]
        timeout: u64,

        /// Path to file containing JSON array of MCP decision tools for sum type outputs
        #[arg(long)]
        decision_tools_file: Option<String>,
    },
}

// ============================================================================
// Main
// ============================================================================

fn main() {
    // Initialize tracing with env filter (RUST_LOG)
    // IMPORTANT: Output to stderr to avoid corrupting JSON on stdout
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_target(false)
        .with_writer(std::io::stderr)
        .init();

    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Session { command } => handle_session_command(command),
    };

    if let Err(e) = result {
        error!(error = %e, "Command failed");
        std::process::exit(1);
    }
}

// ============================================================================
// Session Commands
// ============================================================================

fn handle_session_command(cmd: SessionCommands) -> std::result::Result<(), Box<dyn std::error::Error>> {
    // Get repository root (current directory for now)
    let repo_root = std::env::current_dir()?;

    match cmd {
        SessionCommands::Start { slug, prompt, model, timeout, json_schema, decision_tools_file,
                               hub_session_id, execution_id, node_path, node_type, parent_hub_node_id } => {
            let config = StartConfig {
                slug,
                prompt,
                model,
                timeout_secs: timeout,
                base_branch: None,
                json_schema,
                decision_tools_file,
                hub_session_id,
                execution_id,
                node_path,
                node_type,
                parent_hub_node_id,
            };

            match start_session(&repo_root, &config) {
                Ok(output) => {
                    println!("{}", serde_json::to_string_pretty(&output)?);
                    if output.exit_code != 0 {
                        std::process::exit(output.exit_code);
                    }
                    Ok(())
                }
                Err(e) => {
                    eprintln!("Error starting session: {}", e);
                    std::process::exit(1);
                }
            }
        }

        SessionCommands::Continue { cc_session_id, worktree, branch, model, prompt, timeout, decision_tools_file } => {
            let config = ContinueConfig {
                cc_session_id,
                worktree: worktree.into(),
                branch,
                model,
                prompt,
                timeout_secs: timeout,
                decision_tools_file,
            };

            match continue_session(&repo_root, &config) {
                Ok(output) => {
                    println!("{}", serde_json::to_string_pretty(&output)?);
                    if output.exit_code != 0 {
                        std::process::exit(output.exit_code);
                    }
                    Ok(())
                }
                Err(e) => {
                    eprintln!("Error continuing session: {}", e);
                    std::process::exit(1);
                }
            }
        }

        SessionCommands::Fork { parent_cc_session_id, parent_worktree, parent_branch, model, child_slug, child_prompt, timeout, decision_tools_file } => {
            let config = ForkConfig {
                parent_cc_session_id,
                parent_worktree: parent_worktree.into(),
                parent_branch,
                model,
                child_slug,
                child_prompt,
                timeout_secs: timeout,
                decision_tools_file,
            };

            match fork_session(&repo_root, &config) {
                Ok(output) => {
                    println!("{}", serde_json::to_string_pretty(&output)?);
                    if output.exit_code != 0 {
                        std::process::exit(output.exit_code);
                    }
                    Ok(())
                }
                Err(e) => {
                    eprintln!("Error forking session: {}", e);
                    std::process::exit(1);
                }
            }
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use mantle::events::{ContentBlock, StreamEvent};
    use mantle::{InterruptSignal, RunResult};
    use std::collections::HashMap;

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

        // Empty interrupts should still be present in JSON (Haskell expects it)
        assert!(json.contains("\"interrupts\":[]"));
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
}
