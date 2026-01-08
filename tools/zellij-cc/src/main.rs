//! zellij-cc: Spawn and manage Claude Code sessions in zellij panes.
//!
//! CLI tool for running Claude Code in isolated zellij panes with
//! supervision, timeout handling, and inter-process communication.

use clap::{Parser, Subcommand};
use shell_escape::escape;
use std::borrow::Cow;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;
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
            claude_args,
        } => wrap_claude(&result_fifo, cwd.as_ref(), session_tag.as_deref(), timeout, &claude_args),
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

    // Add prompt
    claude_args.push("-p".to_string());
    claude_args.push(prompt.to_string());

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

// ============================================================================
// Utilities
// ============================================================================

/// Shell-escape a string for safe use in shell commands
fn shell_quote(s: &str) -> Cow<'_, str> {
    escape(Cow::Borrowed(s))
}
