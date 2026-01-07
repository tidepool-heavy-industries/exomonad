use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use nix::sys::stat::Mode;
use nix::unistd::mkfifo;
use serde::{Deserialize, Serialize};
use shell_escape::escape;
use std::borrow::Cow;
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

use zellij_client::{cli_client::start_cli_client, os_input_output::get_cli_client_os_input};
use zellij_utils::input::{actions::Action, command::RunCommandAction};

// ============================================================================
// FIFO Cleanup Guard
// ============================================================================

/// Guard that removes a FIFO on drop, ensuring cleanup even on error paths.
struct FifoGuard {
    path: PathBuf,
}

impl FifoGuard {
    fn new(path: PathBuf) -> Result<Self> {
        // Remove any stale FIFO from previous runs (e.g., after crash)
        if path.exists() {
            std::fs::remove_file(&path).ok();
        }
        mkfifo(&path, Mode::S_IRUSR | Mode::S_IWUSR)
            .with_context(|| format!("Failed to create FIFO at {}", path.display()))?;
        Ok(Self { path })
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for FifoGuard {
    fn drop(&mut self) {
        if let Err(err) = std::fs::remove_file(&self.path) {
            eprintln!("warning: failed to remove FIFO {}: {err}", self.path.display());
        }
    }
}

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
// Interrupt Signal Types
// ============================================================================

/// An interrupt signal sent by Claude via `zellij-cc signal`
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct InterruptSignal {
    /// Signal type: "transition", "escalate", "request_review", etc.
    pub signal_type: String,
    /// Target state for transitions (e.g., "need_more_types")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub state: Option<String>,
    /// Human-readable reason for the signal
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
    Text { text: String },
    ToolUse { name: String, id: String, input: serde_json::Value },
    ToolResult { tool_use_id: String, content: String, is_error: Option<bool> },
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
// Output Types (what zellij-cc returns to callers)
// ============================================================================

#[derive(Serialize, Deserialize)]
struct RunResult {
    /// Exit code from claude process
    exit_code: i32,
    /// Whether Claude Code reported an error
    is_error: bool,
    /// Prose result from Claude Code
    result: Option<String>,
    /// Structured output (when --json-schema was provided)
    structured_output: Option<serde_json::Value>,
    /// Session ID (available immediately from init event)
    session_id: String,
    /// Tag for correlating with orchestrator state (e.g., worktree name)
    #[serde(skip_serializing_if = "Option::is_none")]
    session_tag: Option<String>,
    /// Cost in USD
    total_cost_usd: f64,
    /// Number of turns (tool use iterations)
    num_turns: i64,
    /// Full event stream for debugging/replay
    events: Vec<StreamEvent>,
    /// Permission denials with details
    permission_denials: Vec<PermissionDenial>,
    /// Per-model usage breakdown
    model_usage: HashMap<String, ModelUsage>,
    /// Interrupt signals received during execution
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    interrupts: Vec<InterruptSignal>,
}

// ============================================================================
// Main
// ============================================================================

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
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
        } => {
            run_cc_session(
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
            )?;
        }
        Commands::Wrap {
            result_fifo,
            cwd,
            session_tag,
            claude_args,
        } => {
            wrap_claude(&result_fifo, cwd.as_ref(), session_tag.as_deref(), &claude_args)?;
        }
        Commands::Signal {
            signal_type,
            state,
            reason,
            fifo,
        } => {
            send_signal(&fifo, &signal_type, state.as_deref(), reason.as_deref())?;
        }
    }

    Ok(())
}

// ============================================================================
// Signal Command
// ============================================================================

fn send_signal(fifo: &Path, signal_type: &str, state: Option<&str>, reason: Option<&str>) -> Result<()> {
    let signal = InterruptSignal {
        signal_type: signal_type.to_string(),
        state: state.map(|s| s.to_string()),
        reason: reason.map(|s| s.to_string()),
    };

    let json = serde_json::to_string(&signal)?;

    // Write to signal FIFO (non-blocking, fire-and-forget)
    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .open(fifo)
        .context("Failed to open signal FIFO - is wrap running?")?;

    file.write_all(json.as_bytes())?;
    file.write_all(b"\n")?;

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
    // Create FIFO for result communication (guard ensures cleanup on all paths)
    let fifo_path = PathBuf::from(format!("/tmp/zellij-cc-{}.fifo", std::process::id()));
    let fifo_guard = FifoGuard::new(fifo_path)?;

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
        shell_quote(&fifo_guard.path().display().to_string()).into_owned(),
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

    wrap_cmd_parts.push("--".to_string());
    wrap_cmd_parts.extend(escaped_args);

    let wrap_cmd = wrap_cmd_parts.join(" ");

    let os_input = get_cli_client_os_input()
        .context("Failed to get zellij OS input - is zellij running?")?;

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

    start_cli_client(Box::new(os_input), session, actions);

    // Block reading from FIFO (no polling!)
    // Use Duration::ZERO as sentinel for "no timeout" - read_result_from_fifo handles this
    let timeout = if timeout_secs > 0 {
        Duration::from_secs(timeout_secs)
    } else {
        Duration::ZERO // Sentinel for "no timeout"
    };

    let result = read_result_from_fifo(fifo_guard.path(), timeout)?;

    // fifo_guard handles cleanup on drop (including error paths)

    // Print final JSON to stdout (for Haskell caller)
    println!("{}", serde_json::to_string_pretty(&result)?);

    Ok(())
}

// ============================================================================
// Wrap Command (runs claude as subprocess, humanizes output)
// ============================================================================

fn wrap_claude(result_fifo: &Path, cwd: Option<&PathBuf>, session_tag: Option<&str>, claude_args: &[String]) -> Result<()> {
    use std::sync::mpsc;
    use std::thread;

    // Create signal FIFO for interrupt communication (guard ensures cleanup on all paths)
    let signal_fifo_path = PathBuf::from(format!("/tmp/zellij-cc-{}.signal", std::process::id()));
    let signal_fifo_guard = FifoGuard::new(signal_fifo_path)?;

    // Spawn claude with captured stdout and signal FIFO env var
    let mut cmd = Command::new("claude");
    cmd.args(claude_args)
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit()) // Pass stderr through
        .env("TIDEPOOL_SIGNAL_FIFO", signal_fifo_guard.path());

    if let Some(dir) = cwd {
        cmd.current_dir(dir);
    }

    let mut child = cmd.spawn().context("Failed to spawn claude")?;

    let stdout = child.stdout.take().unwrap();
    let reader = BufReader::new(stdout);

    // Channel to receive signals from the signal FIFO reader thread
    let (signal_tx, signal_rx) = mpsc::channel::<InterruptSignal>();

    // Termination flag for the signal reader thread
    let should_stop = Arc::new(AtomicBool::new(false));
    let should_stop_clone = Arc::clone(&should_stop);

    // Spawn a thread to read from signal FIFO
    let signal_fifo_clone = signal_fifo_guard.path().to_path_buf();
    let signal_thread = thread::spawn(move || {
        // Open FIFO in read mode - this blocks until writer opens it
        // Using O_RDONLY | O_NONBLOCK would be ideal but for simplicity
        // we just try to open and read in a loop
        while !should_stop_clone.load(Ordering::Relaxed) {
            match std::fs::File::open(&signal_fifo_clone) {
                Ok(file) => {
                    let reader = BufReader::new(file);
                    for line in reader.lines() {
                        if should_stop_clone.load(Ordering::Relaxed) { break; }
                        if let Ok(line) = line {
                            if line.trim().is_empty() { continue; }
                            if let Ok(signal) = serde_json::from_str::<InterruptSignal>(&line) {
                                let _ = signal_tx.send(signal);
                            }
                        }
                    }
                }
                Err(_) => {
                    // FIFO not ready yet, short sleep and retry
                    thread::sleep(Duration::from_millis(100));
                }
            }
        }
    });

    let mut events: Vec<StreamEvent> = Vec::new();
    let mut result_event: Option<ResultEvent> = None;
    let mut interrupts: Vec<InterruptSignal> = Vec::new();

    // Process each JSONL line from claude stdout
    for line in reader.lines() {
        // Check for signals (non-blocking)
        while let Ok(signal) = signal_rx.try_recv() {
            println!("\n⚡ Interrupt: {} (state: {:?})", signal.signal_type, signal.state);
            interrupts.push(signal);
        }

        let line = line?;
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
                eprintln!("[parse error] {} (line: {}...)", e, truncated);
            }
        }
    }

    // Drain any remaining signals
    while let Ok(signal) = signal_rx.try_recv() {
        interrupts.push(signal);
    }

    // Wait for claude to exit
    let status = child.wait()?;

    // Signal the reader thread to stop and wait for it
    should_stop.store(true, Ordering::Relaxed);
    // The thread may be blocked on FIFO read, so we give it a moment then move on
    // (the thread will exit on next poll cycle or when FIFO is removed)
    let _ = signal_thread.join();

    // signal_fifo_guard handles cleanup on drop (including error paths)

    // Build final result
    let result = build_run_result(events, result_event, status, session_tag, interrupts)?;
    let json = serde_json::to_string(&result)?;

    // Write to FIFO (unblocks the waiting `run` process)
    let mut fifo = std::fs::File::create(result_fifo)
        .context("Failed to open result FIFO for writing")?;
    fifo.write_all(json.as_bytes())?;

    Ok(())
}

// ============================================================================
// Human-Readable Output
// ============================================================================

fn print_event_humanized(event: &StreamEvent) {
    match event {
        StreamEvent::System(s) => {
            let short_id = if s.session_id.len() >= 8 {
                &s.session_id[..8]
            } else {
                &s.session_id
            };
            println!("━━━ Session {} ━━━", short_id);
            println!("Model: {}", s.model);
            println!();
        }
        StreamEvent::Assistant(a) => {
            for block in &a.message.content {
                match block {
                    ContentBlock::Text { text } => {
                        println!("{}", text);
                    }
                    ContentBlock::ToolUse { name, input, .. } => {
                        println!("\n┌─ {} ─────────────────", name);
                        // Show key input params, truncated
                        if let Some(obj) = input.as_object() {
                            for (k, v) in obj.iter().take(3) {
                                let v_str = v.to_string();
                                let char_count = v_str.chars().count();
                                let preview: String = v_str.chars().take(60).collect();
                                if char_count > 60 {
                                    println!("│ {}: {}...", k, preview);
                                } else {
                                    println!("│ {}: {}", k, preview);
                                }
                            }
                            if obj.len() > 3 {
                                println!("│ ... ({} more fields)", obj.len() - 3);
                            }
                        }
                        println!("└─────────────────────────");
                    }
                    ContentBlock::ToolResult { content, is_error, .. } => {
                        let status = if is_error.unwrap_or(false) { "✗" } else { "✓" };
                        let char_count = content.chars().count();
                        let preview: String = content.chars().take(100).collect();
                        if char_count > 100 {
                            println!("  {} {}...", status, preview);
                        } else {
                            println!("  {} {}", status, preview);
                        }
                    }
                }
            }
        }
        StreamEvent::User(_) => {
            // Usually just tool results, already handled above
        }
        StreamEvent::Result(r) => {
            println!();
            println!("━━━ Complete ━━━");
            println!("Status: {}", if r.is_error { "error" } else { "success" });
            println!("Turns: {}", r.num_turns.unwrap_or(0));
            println!("Cost: ${:.4}", r.total_cost_usd.unwrap_or(0.0));
        }
    }
}

// ============================================================================
// Result Building
// ============================================================================

fn build_run_result(
    events: Vec<StreamEvent>,
    result_event: Option<ResultEvent>,
    status: ExitStatus,
    session_tag: Option<&str>,
    interrupts: Vec<InterruptSignal>,
) -> Result<RunResult> {
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

    Ok(RunResult {
        exit_code: status.code().unwrap_or(-1),
        is_error: result_event.is_error,
        result: result_event.result.clone(),
        structured_output: result_event.structured_output.clone(),
        session_id,
        session_tag: session_tag.map(|s| s.to_string()),
        total_cost_usd: result_event.total_cost_usd.unwrap_or(0.0),
        num_turns: result_event.num_turns.unwrap_or(0),
        events,
        permission_denials: result_event.permission_denials.clone(),
        model_usage: result_event.model_usage.clone(),
        interrupts,
    })
}

// ============================================================================
// FIFO Reading
// ============================================================================

fn read_result_from_fifo(fifo_path: &Path, timeout: Duration) -> Result<RunResult> {
    use nix::poll::{poll, PollFd, PollFlags, PollTimeout};
    use std::os::fd::AsFd;

    // Open FIFO for reading (this blocks until writer opens it)
    let file = std::fs::File::open(fifo_path)
        .context("Failed to open FIFO for reading")?;

    // Use poll with timeout
    // Note: PollTimeout::from takes u16, so we clamp to avoid overflow.
    // For very long timeouts (>65s), we use NONE (infinite) since the
    // outer process will handle overall timeout.
    let mut poll_fds = [PollFd::new(file.as_fd(), PollFlags::POLLIN)];
    let timeout_ms = timeout.as_millis();
    let poll_timeout = if timeout_ms == 0 {
        PollTimeout::NONE
    } else if timeout_ms > u16::MAX as u128 {
        // For very long timeouts, just wait indefinitely at the poll level
        // (the overall timeout is handled at the process level)
        PollTimeout::NONE
    } else {
        PollTimeout::from(timeout_ms as u16)
    };

    match poll(&mut poll_fds, poll_timeout)? {
        0 => anyhow::bail!("Timeout waiting for result from FIFO"),
        _ => {
            let content = std::io::read_to_string(file)
                .context("Failed to read from FIFO")?;
            let result: RunResult = serde_json::from_str(&content)
                .context("Failed to parse result JSON from FIFO")?;
            Ok(result)
        }
    }
}

/// Shell-escape a string for safe use in shell commands
fn shell_quote(s: &str) -> Cow<'_, str> {
    escape(Cow::Borrowed(s))
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

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
}
