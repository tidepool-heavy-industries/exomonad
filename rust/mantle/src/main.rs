//! mantle: Spawn and manage Claude Code sessions.
//!
//! CLI tool for running Claude Code sessions with process supervision,
//! timeout handling, and inter-process communication.
//!
//! ## Commands
//!
//! - `session` - Manage sessions (start, continue, fork, list, info, cleanup)
//! - `run` - Legacy: spawn in zellij pane (requires --features zellij)

use clap::{Parser, Subcommand};
use tracing::error;
use tracing_subscriber::EnvFilter;

use mantle::session::{
    cleanup_sessions, continue_session, fork_session, list_sessions, session_info, start_session,
    CleanupConfig, ContinueConfig, ForkConfig, ListConfig, SessionState, StartConfig, StateManager,
    WorktreeManager,
};

// Zellij-specific imports
#[cfg(feature = "zellij")]
use std::path::PathBuf;
#[cfg(feature = "zellij")]
use std::time::Duration;
#[cfg(feature = "zellij")]
use tracing::{debug, info};
#[cfg(feature = "zellij")]
use zellij_client::{cli_client::start_cli_client, os_input_output::get_cli_client_os_input};
#[cfg(feature = "zellij")]
use zellij_utils::input::{actions::Action, command::RunCommandAction};
#[cfg(feature = "zellij")]
use mantle::{build_prompt, shell_quote, MantleError, Result, ResultFifo};

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

    /// Run a Claude Code session in a new zellij pane (legacy, requires --features zellij)
    #[cfg(feature = "zellij")]
    Run {
        /// Zellij session name to attach to
        #[arg(long)]
        session: String,

        /// Name for the new pane
        #[arg(long)]
        name: String,

        /// Model to use (haiku, sonnet, opus)
        #[arg(long, default_value = "haiku")]
        model: String,

        /// Prompt text for Claude Code
        #[arg(long)]
        prompt: String,

        /// Context to inject before the prompt (for session resume with fresh context)
        #[arg(long)]
        inject_context: Option<String>,

        /// Working directory for Claude Code
        #[arg(long)]
        cwd: Option<PathBuf>,

        /// JSON schema for structured output validation
        #[arg(long)]
        json_schema: Option<String>,

        /// Tools to allow (e.g., "Glob,Read" or "" for none)
        #[arg(long)]
        tools: Option<String>,

        /// Timeout in seconds (0 = no timeout)
        #[arg(long, default_value = "300")]
        timeout: u64,

        /// Resume an existing Claude Code session by ID
        #[arg(long)]
        resume: Option<String>,

        /// Fork the session (read-only resume, doesn't modify original)
        #[arg(long)]
        fork_session: bool,

        /// Tag for correlating this session with orchestrator state (e.g., worktree name)
        #[arg(long)]
        session_tag: Option<String>,

        /// Control socket path (enables hook interception via Haskell)
        #[arg(long)]
        control_socket: Option<PathBuf>,
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
        #[arg(long, default_value = "300")]
        timeout: u64,

        /// Use Docker container instead of local execution
        #[arg(long)]
        docker: bool,
    },

    /// Continue an existing session with a new prompt
    Continue {
        /// Session ID to continue
        session_id: String,

        /// New prompt text
        #[arg(long)]
        prompt: String,
    },

    /// Fork a session (create child session from parent's context)
    Fork {
        /// Parent session ID to fork from
        parent_id: String,

        /// Slug for the child session
        #[arg(long)]
        child_slug: String,

        /// Prompt for the child session
        #[arg(long)]
        child_prompt: String,
    },

    /// Show detailed information about a session
    Info {
        /// Session ID
        session_id: String,
    },

    /// List all sessions
    List {
        /// Filter by state
        #[arg(long, value_enum)]
        state: Option<SessionState>,
    },

    /// Clean up old sessions
    Cleanup {
        /// Only clean up completed sessions
        #[arg(long)]
        completed: bool,

        /// Show what would be cleaned up without doing it
        #[arg(long)]
        dry_run: bool,
    },
}

// ============================================================================
// Main
// ============================================================================

fn main() {
    // Initialize tracing with env filter (RUST_LOG)
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_target(false)
        .init();

    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Session { command } => handle_session_command(command),

        #[cfg(feature = "zellij")]
        Commands::Run {
            session,
            name,
            model,
            prompt,
            inject_context,
            cwd,
            json_schema,
            tools,
            timeout,
            resume,
            fork_session,
            session_tag,
            control_socket,
        } => run_cc_session(
            &session,
            &name,
            &model,
            &prompt,
            inject_context.as_deref(),
            cwd.as_ref(),
            json_schema.as_deref(),
            tools.as_deref(),
            timeout,
            resume.as_deref(),
            fork_session,
            session_tag.as_deref(),
            control_socket.as_ref(),
        ),
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
    let state_manager = StateManager::new(&repo_root)?;

    match cmd {
        SessionCommands::Start { slug, prompt, model, timeout, docker } => {
            let config = StartConfig {
                slug,
                prompt,
                model,
                timeout_secs: timeout,
                docker,
                base_branch: None,
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

        SessionCommands::Continue { session_id, prompt } => {
            let config = ContinueConfig {
                session_id,
                prompt,
                timeout_secs: 300, // Default timeout
                docker: false,
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

        SessionCommands::Fork { parent_id, child_slug, child_prompt } => {
            let config = ForkConfig {
                parent_id,
                child_slug,
                child_prompt,
                timeout_secs: 300, // Default timeout
                docker: false,
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

        SessionCommands::Info { session_id } => {
            match session_info(&state_manager, &session_id) {
                Ok(info) => {
                    // Output JSON for Haskell consumption
                    println!("{}", serde_json::to_string(&info)?);
                    Ok(())
                }
                Err(e) => {
                    eprintln!("Error: {}", e);
                    std::process::exit(1);
                }
            }
        }

        SessionCommands::List { state } => {
            let config = ListConfig {
                state,
                parent_id: None,
                limit: None,
            };

            match list_sessions(&state_manager, &config) {
                Ok(output) => {
                    // Output JSON for Haskell consumption
                    println!("{}", serde_json::to_string(&output)?);
                    Ok(())
                }
                Err(e) => {
                    eprintln!("Error: {}", e);
                    std::process::exit(1);
                }
            }
        }

        SessionCommands::Cleanup { completed, dry_run } => {
            let worktree_manager = WorktreeManager::new(&repo_root, &state_manager.worktrees_dir())?;
            let config = CleanupConfig {
                completed_only: completed,
                failed_only: false,
                all_terminal: !completed, // If not just completed, clean all terminal states
                dry_run,
                session_ids: vec![],
            };

            match cleanup_sessions(&state_manager, &worktree_manager, &config) {
                Ok(output) => {
                    // Output JSON for Haskell consumption
                    println!("{}", serde_json::to_string(&output)?);
                    Ok(())
                }
                Err(e) => {
                    eprintln!("Error: {}", e);
                    std::process::exit(1);
                }
            }
        }
    }
}

// ============================================================================
// Legacy Run Command (zellij)
// ============================================================================

#[cfg(feature = "zellij")]
fn run_cc_session(
    session: &str,
    name: &str,
    model: &str,
    prompt: &str,
    inject_context: Option<&str>,
    cwd: Option<&PathBuf>,
    json_schema: Option<&str>,
    tools: Option<&str>,
    timeout_secs: u64,
    resume: Option<&str>,
    fork_session: bool,
    session_tag: Option<&str>,
    control_socket: Option<&PathBuf>,
) -> std::result::Result<(), Box<dyn std::error::Error>> {
    // Create FIFO for result communication
    let result_fifo = ResultFifo::new()?;

    // Build claude args
    let mut claude_args = vec![
        "--dangerously-skip-permissions".to_string(),
        "--output-format".to_string(),
        "stream-json".to_string(),
        "--verbose".to_string(),
        "--model".to_string(),
        model.to_string(),
    ];

    // Add optional flags
    if let Some(schema) = json_schema {
        claude_args.push("--json-schema".to_string());
        claude_args.push(schema.to_string());
    }

    if let Some(t) = tools {
        claude_args.push("--tools".to_string());
        claude_args.push(t.to_string());
    }

    // Add session resumption flags
    if let Some(session_id) = resume {
        claude_args.push("--resume".to_string());
        claude_args.push(session_id.to_string());
    }

    if fork_session {
        claude_args.push("--fork-session".to_string());
    }

    // Add prompt (with optional injected context prefix)
    claude_args.push("-p".to_string());
    claude_args.push(build_prompt(prompt, inject_context));

    // Build wrap command - escape args properly
    let escaped_args: Vec<String> = claude_args
        .iter()
        .map(|a| shell_quote(a).into_owned())
        .collect();

    // Use mantle-agent instead of mantle for the wrapper
    let mut wrap_cmd_parts = vec![
        "mantle-agent".to_string(),
        "wrap".to_string(),
        "--result-fifo".to_string(),
        shell_quote(&result_fifo.path().display().to_string()).into_owned(),
    ];

    // Add cwd if specified
    if let Some(dir) = cwd {
        wrap_cmd_parts.push("--cwd".to_string());
        wrap_cmd_parts.push(shell_quote(&dir.display().to_string()).into_owned());
    }

    // Add session tag if specified
    if let Some(tag) = session_tag {
        wrap_cmd_parts.push("--session-tag".to_string());
        wrap_cmd_parts.push(shell_quote(tag).into_owned());
    }

    // Pass timeout to wrap command so it can enforce it on the subprocess
    if timeout_secs > 0 {
        wrap_cmd_parts.push("--timeout".to_string());
        wrap_cmd_parts.push(timeout_secs.to_string());
    }

    // Pass control socket to wrap command for hook interception
    if let Some(socket) = control_socket {
        wrap_cmd_parts.push("--control-socket".to_string());
        wrap_cmd_parts.push(shell_quote(&socket.display().to_string()).into_owned());
    }

    wrap_cmd_parts.push("--".to_string());
    wrap_cmd_parts.extend(escaped_args);

    let wrap_cmd = wrap_cmd_parts.join(" ");

    debug!(wrap_cmd = %wrap_cmd, "Building wrap command");

    let os_input = get_cli_client_os_input()
        .map_err(|e| MantleError::Zellij(format!("Failed to get zellij OS input: {}", e)))?;

    // Spawn pane running wrap command
    let actions = vec![Action::NewTiledPane(
        None,
        Some(RunCommandAction {
            command: PathBuf::from("bash"),
            args: vec!["-c".into(), wrap_cmd],
            cwd: cwd.cloned(),
            direction: None,
            hold_on_close: true,
            hold_on_start: false,
            originating_plugin: None,
            use_terminal_title: false,
        }),
        Some(name.to_string()),
    )];

    info!(
        session = %session,
        pane_name = %name,
        model = %model,
        "Spawning zellij pane"
    );

    start_cli_client(Box::new(os_input), session, actions);

    // Block reading from FIFO
    let timeout = if timeout_secs > 0 {
        Duration::from_secs(timeout_secs)
    } else {
        Duration::ZERO // Sentinel for "no timeout"
    };

    let result = result_fifo.read_with_timeout(timeout)?;

    // Print final JSON to stdout (for Haskell caller)
    println!(
        "{}",
        serde_json::to_string_pretty(&result).map_err(MantleError::JsonSerialize)?
    );

    Ok(())
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
