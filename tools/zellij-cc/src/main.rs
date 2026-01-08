//! zellij-cc: Spawn and manage Claude Code sessions in zellij panes.
//!
//! CLI tool for running Claude Code in isolated zellij panes with
//! supervision, timeout handling, and inter-process communication.

use clap::{Parser, Subcommand};
use shell_escape::escape;
use std::borrow::Cow;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;
use std::sync::mpsc;
use std::thread;
use std::time::Duration;
use tracing::{debug, error, info, warn};
use tracing_subscriber::EnvFilter;

use zellij_client::{cli_client::start_cli_client, os_input_output::get_cli_client_os_input};
use zellij_utils::input::{actions::Action, command::RunCommandAction};

use zellij_cc::error::{Result, ZellijCcError};
use zellij_cc::events::{InterruptSignal, ResultEvent, RunResult, StreamEvent};
use zellij_cc::fifo::{write_result, write_signal, ResultFifo, SignalFifo};
use zellij_cc::humanize::{print_event_humanized, print_interrupt};
use zellij_cc::supervisor::{build_claude_command, install_signal_handlers, Supervisor};
use zellij_cc::tui::{run_tui, TuiEvent};

// ============================================================================
// CLI Types
// ============================================================================

#[derive(Parser)]
#[command(name = "zellij-cc")]
#[command(about = "Spawn and manage Claude Code sessions in zellij panes")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run a Claude Code session in a new pane, block until complete
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
    },

    /// Internal: Wrap claude as subprocess, humanize output
    #[command(hide = true)]
    Wrap {
        /// FIFO to write final JSON result to
        #[arg(long)]
        result_fifo: PathBuf,

        /// Working directory for Claude Code
        #[arg(long)]
        cwd: Option<PathBuf>,

        /// Tag for correlating this session with orchestrator state
        #[arg(long)]
        session_tag: Option<String>,

        /// Timeout in seconds (0 = no timeout)
        #[arg(long, default_value = "0")]
        timeout: u64,

        /// Disable TUI, use simple println output (for CI/headless)
        #[arg(long)]
        no_tui: bool,

        /// All remaining args passed to claude
        #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
        claude_args: Vec<String>,
    },

    /// Signal an interrupt (called by Claude via Bash)
    Signal {
        /// Signal type: "transition", "escalate", "request_review", etc.
        signal_type: String,

        /// Target state for transitions
        #[arg(long)]
        state: Option<String>,

        /// Reason/payload for the signal
        #[arg(long)]
        reason: Option<String>,

        /// FIFO to write to (set via TIDEPOOL_SIGNAL_FIFO env var by wrap)
        #[arg(long, env = "TIDEPOOL_SIGNAL_FIFO")]
        fifo: PathBuf,
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
        ),
        Commands::Wrap {
            result_fifo,
            cwd,
            session_tag,
            timeout,
            no_tui,
            claude_args,
        } => wrap_claude(&result_fifo, cwd.as_ref(), session_tag.as_deref(), timeout, no_tui, &claude_args),
        Commands::Signal {
            signal_type,
            state,
            reason,
            fifo,
        } => send_signal(&fifo, &signal_type, state.as_deref(), reason.as_deref()),
    };

    if let Err(e) = result {
        error!(error = %e, "Command failed");
        std::process::exit(1);
    }
}

// ============================================================================
// Signal Command
// ============================================================================

fn send_signal(
    fifo: &PathBuf,
    signal_type: &str,
    state: Option<&str>,
    reason: Option<&str>,
) -> Result<()> {
    let signal = InterruptSignal {
        signal_type: signal_type.to_string(),
        state: state.map(|s| s.to_string()),
        reason: reason.map(|s| s.to_string()),
    };

    write_signal(fifo, &signal)?;

    println!("Signal sent: {} (state: {:?})", signal_type, state);
    Ok(())
}

// ============================================================================
// Run Command (orchestrator)
// ============================================================================

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
) -> Result<()> {
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

    let mut wrap_cmd_parts = vec![
        "zellij-cc".to_string(),
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

    wrap_cmd_parts.push("--".to_string());
    wrap_cmd_parts.extend(escaped_args);

    let wrap_cmd = wrap_cmd_parts.join(" ");

    debug!(wrap_cmd = %wrap_cmd, "Building wrap command");

    let os_input = get_cli_client_os_input()
        .map_err(|e| ZellijCcError::Zellij(format!("Failed to get zellij OS input: {}", e)))?;

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
        serde_json::to_string_pretty(&result).map_err(ZellijCcError::JsonSerialize)?
    );

    Ok(())
}

// ============================================================================
// Wrap Command (runs claude as subprocess, humanizes output)
// ============================================================================

fn wrap_claude(
    result_fifo: &PathBuf,
    cwd: Option<&PathBuf>,
    session_tag: Option<&str>,
    timeout_secs: u64,
    no_tui: bool,
    claude_args: &[String],
) -> Result<()> {
    // Install signal handlers for SIGINT/SIGTERM forwarding
    install_signal_handlers();

    // Create signal FIFO for interrupt communication
    let signal_fifo = SignalFifo::new()?;

    // Build and spawn claude with supervisor
    let cmd = build_claude_command(claude_args, cwd.map(|p| p.as_path()), signal_fifo.path());

    // Spawn with timeout if specified (0 = no timeout)
    let timeout = if timeout_secs > 0 {
        Some(Duration::from_secs(timeout_secs))
    } else {
        None
    };
    let mut supervisor = Supervisor::spawn(cmd, timeout)?;

    // Take stdout for reading
    let stdout = supervisor.take_stdout();

    if no_tui {
        // Simple println mode for CI/headless
        wrap_claude_simple(stdout, &signal_fifo, &mut supervisor, result_fifo, session_tag)
    } else {
        // Full TUI mode
        wrap_claude_tui(stdout, &signal_fifo, &mut supervisor, result_fifo, session_tag)
    }
}

/// Simple println mode (original behavior, for CI/headless)
fn wrap_claude_simple(
    stdout: std::process::ChildStdout,
    signal_fifo: &SignalFifo,
    supervisor: &mut Supervisor,
    result_fifo: &PathBuf,
    session_tag: Option<&str>,
) -> Result<()> {
    let reader = BufReader::new(stdout);

    let mut events: Vec<StreamEvent> = Vec::new();
    let mut result_event: Option<ResultEvent> = None;
    let mut interrupts: Vec<InterruptSignal> = Vec::new();

    // Process each JSONL line from claude stdout
    for line in reader.lines() {
        // Check for signals (non-blocking)
        while let Some(signal) = signal_fifo.try_recv() {
            print_interrupt(&signal);
            interrupts.push(signal);
        }

        let line = match line {
            Ok(l) => l,
            Err(e) => {
                warn!(error = %e, "Error reading line from claude stdout");
                continue;
            }
        };

        if line.trim().is_empty() {
            continue;
        }

        match serde_json::from_str::<StreamEvent>(&line) {
            Ok(event) => {
                print_event_humanized(&event); // Human-readable to stdout (pane)

                if let StreamEvent::Result(ref r) = event {
                    result_event = Some(r.clone());
                }
                events.push(event);
            }
            Err(e) => {
                let truncated: String = line.chars().take(50).collect();
                warn!(
                    error = %e,
                    line = %truncated,
                    "JSON parse error"
                );
            }
        }
    }

    // Drain any remaining signals
    interrupts.extend(signal_fifo.drain());

    // Wait for claude to exit
    let status = supervisor.wait_with_timeout()?;
    let exit_code = status.code().unwrap_or(-1);

    // Build final result
    let result = RunResult::from_events(
        events,
        result_event,
        exit_code,
        session_tag.map(|s| s.to_string()),
        interrupts,
    );

    // Write to result FIFO (unblocks the waiting `run` process)
    write_result(result_fifo, &result)?;

    Ok(())
}

/// TUI mode with interactive display
fn wrap_claude_tui(
    stdout: std::process::ChildStdout,
    signal_fifo: &SignalFifo,
    supervisor: &mut Supervisor,
    result_fifo: &PathBuf,
    session_tag: Option<&str>,
) -> Result<()> {
    // Create channel for TUI events
    let (tx, rx) = mpsc::channel::<TuiEvent>();
    let tx_signal = tx.clone();

    // Spawn stdout reader thread
    let stdout_handle = thread::spawn(move || {
        let reader = BufReader::new(stdout);
        for line in reader.lines() {
            let line = match line {
                Ok(l) => l,
                Err(e) => {
                    warn!(error = %e, "Error reading line from claude stdout");
                    continue;
                }
            };

            if line.trim().is_empty() {
                continue;
            }

            match serde_json::from_str::<StreamEvent>(&line) {
                Ok(event) => {
                    if tx.send(TuiEvent::Claude(event)).is_err() {
                        break; // TUI closed
                    }
                }
                Err(e) => {
                    let truncated: String = line.chars().take(50).collect();
                    warn!(
                        error = %e,
                        line = %truncated,
                        "JSON parse error"
                    );
                }
            }
        }
        // Signal that stdout is done
        let _ = tx.send(TuiEvent::ProcessExit);
    });

    // Clone signal_fifo path for the thread
    let signal_path = signal_fifo.path().to_path_buf();

    // Spawn signal reader thread
    let signal_handle = thread::spawn(move || {
        // Create a new SignalFifo for this thread using the same path
        // We poll periodically to check for signals
        loop {
            // Try to read signal from the FIFO
            if let Ok(contents) = std::fs::read_to_string(&signal_path) {
                for line in contents.lines() {
                    if let Ok(signal) = serde_json::from_str::<InterruptSignal>(line) {
                        if tx_signal.send(TuiEvent::Interrupt(signal)).is_err() {
                            return; // TUI closed
                        }
                    }
                }
                // Clear the file after reading
                let _ = std::fs::write(&signal_path, "");
            }
            thread::sleep(Duration::from_millis(100));
        }
    });

    // Run TUI (blocks until user quits)
    let tui_result = run_tui(rx)?;

    // Wait for stdout thread to finish
    let _ = stdout_handle.join();

    // Signal thread runs forever, so we don't join it - it'll be terminated when we exit
    drop(signal_handle);

    // Wait for the child process (non-blocking check in loop)
    let exit_code = loop {
        match supervisor.try_wait()? {
            Some(status) => {
                break status.code().unwrap_or(-1);
            }
            None => {
                // Process still running, wait a bit
                thread::sleep(Duration::from_millis(100));
            }
        }
    };

    // Build final result from TUI state
    let result = RunResult::from_events(
        tui_result.events,
        tui_result.result_event,
        exit_code,
        session_tag.map(|s| s.to_string()),
        tui_result.interrupts,
    );

    // Write to result FIFO (unblocks the waiting `run` process)
    write_result(result_fifo, &result)?;

    Ok(())
}

// ============================================================================
// Utilities
// ============================================================================

/// Shell-escape a string for safe use in shell commands
fn shell_quote(s: &str) -> Cow<'_, str> {
    escape(Cow::Borrowed(s))
}

/// Build the final prompt, optionally prepending injected context.
///
/// When context is provided, it's prepended with a double newline separator
/// to clearly delineate it from the actual prompt.
fn build_prompt(prompt: &str, inject_context: Option<&str>) -> String {
    match inject_context {
        Some(ctx) => format!("{}\n\n{}", ctx, prompt),
        None => prompt.to_string(),
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use zellij_cc::events::ContentBlock;

    const SAMPLE_INIT: &str = r#"{"type":"system","subtype":"init","session_id":"abc-123","tools":["Read","Glob"],"model":"claude-sonnet-4-20250514"}"#;
    const SAMPLE_TEXT: &str = r#"{"type":"assistant","message":{"content":[{"type":"text","text":"hello world"}]}}"#;
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

    #[test]
    fn test_build_prompt_without_context() {
        let result = build_prompt("do the thing", None);
        assert_eq!(result, "do the thing");
    }

    #[test]
    fn test_build_prompt_with_context() {
        let result = build_prompt("continue working", Some("CONTEXT: file.rs was modified"));
        assert_eq!(result, "CONTEXT: file.rs was modified\n\ncontinue working");
    }

    #[test]
    fn test_build_prompt_with_multiline_context() {
        let ctx = "CONTEXT:\n- file1.rs modified\n- file2.rs added";
        let result = build_prompt("proceed", Some(ctx));
        assert_eq!(result, "CONTEXT:\n- file1.rs modified\n- file2.rs added\n\nproceed");
    }

    #[test]
    fn test_build_prompt_with_special_chars() {
        // Verify special chars pass through (shell escaping happens later)
        let ctx = "CONTEXT: user said \"hello\" & 'goodbye'";
        let result = build_prompt("continue", Some(ctx));
        assert!(result.contains("\"hello\""));
        assert!(result.contains("&"));
        assert!(result.contains("'goodbye'"));
    }
}
