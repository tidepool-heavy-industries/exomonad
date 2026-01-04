use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use serde::Serialize;
use shell_escape::escape;
use std::borrow::Cow;
use std::path::PathBuf;
use std::thread;
use std::time::{Duration, Instant};

use zellij_client::{cli_client::start_cli_client, os_input_output::get_cli_client_os_input};
use zellij_utils::input::{actions::Action, command::RunCommandAction};

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

        /// Output file for JSON result (stderr goes to <file>.stderr)
        #[arg(long)]
        output_file: PathBuf,

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
    },
}

#[derive(Serialize)]
struct RunResult {
    /// Always 0 - we cannot get actual exit code from zellij pane.
    /// Check is_error field for actual success/failure.
    exit_code: i32,
    output_file: PathBuf,
    stderr_file: PathBuf,
    /// Whether Claude Code reported an error
    is_error: bool,
    /// Prose result from Claude Code
    result: Option<String>,
    /// Structured output (when --json-schema was provided)
    structured_output: Option<serde_json::Value>,
    /// Cost in USD
    cost_usd: Option<f64>,
    /// Number of turns (tool use iterations)
    num_turns: Option<i64>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run {
            session,
            name,
            model,
            prompt,
            output_file,
            cwd,
            json_schema,
            tools,
            timeout,
        } => {
            run_cc_session(
                &session,
                &name,
                &model,
                &prompt,
                &output_file,
                cwd.as_ref(),
                json_schema.as_deref(),
                tools.as_deref(),
                timeout,
            )?;
        }
    }

    Ok(())
}

fn run_cc_session(
    session: &str,
    name: &str,
    model: &str,
    prompt: &str,
    output_file: &PathBuf,
    cwd: Option<&PathBuf>,
    json_schema: Option<&str>,
    tools: Option<&str>,
    timeout_secs: u64,
) -> Result<()> {
    // Validate output file's parent directory exists
    if let Some(parent) = output_file.parent() {
        if !parent.as_os_str().is_empty() && !parent.exists() {
            anyhow::bail!(
                "Output file parent directory does not exist: '{}'",
                parent.display()
            );
        }
    }

    let output_path = output_file.display().to_string();

    // Stderr goes to a separate file
    let stderr_file = output_file.with_extension("stderr");
    let stderr_path = stderr_file.display().to_string();

    // Build shell command with proper escaping (using shell-escape crate)
    // Captures stdout to output_file, stderr to stderr_file
    // Both streams shown in pane via process substitution
    let mut cmd_parts = vec![
        "claude".to_string(),
        "--dangerously-skip-permissions".to_string(),
        "--output-format".to_string(),
        "json".to_string(),
        "--model".to_string(),
        shell_quote(model).into_owned(),
    ];

    // Add optional flags
    if let Some(schema) = json_schema {
        cmd_parts.push("--json-schema".to_string());
        cmd_parts.push(shell_quote(schema).into_owned());
    }

    if let Some(t) = tools {
        cmd_parts.push("--tools".to_string());
        cmd_parts.push(shell_quote(t).into_owned());
    }

    // Add prompt
    cmd_parts.push("-p".to_string());
    cmd_parts.push(shell_quote(prompt).into_owned());

    let shell_cmd = format!(
        "{} > >(tee {}) 2> >(tee {} >&2)",
        cmd_parts.join(" "),
        shell_quote(&output_path),
        shell_quote(&stderr_path),
    );

    let os_input = get_cli_client_os_input()
        .context("Failed to get zellij OS input - is zellij running?")?;

    // Spawn pane with the command
    // Use bash for process substitution support
    let actions = vec![Action::NewTiledPane(
        None, // direction
        Some(RunCommandAction {
            command: PathBuf::from("bash"),
            args: vec!["-c".into(), shell_cmd],
            cwd: cwd.cloned(),
            direction: None,
            hold_on_close: true, // Keep pane open so user can see result
            hold_on_start: false,
            originating_plugin: None,
            use_terminal_title: false,
        }),
        Some(name.to_string()), // pane_name
    )];

    start_cli_client(Box::new(os_input), session, actions);

    // Block until CC exits by watching for valid JSON in output file
    let start = Instant::now();
    let timeout = if timeout_secs > 0 {
        Some(Duration::from_secs(timeout_secs))
    } else {
        None
    };

    eprintln!(
        "Waiting for Claude Code to complete (output: '{}')...",
        output_path
    );

    loop {
        // Check timeout
        if let Some(t) = timeout {
            if start.elapsed() > t {
                // Include context in timeout error
                let partial = std::fs::read_to_string(output_file)
                    .ok()
                    .filter(|s| !s.trim().is_empty());
                let stderr_content = std::fs::read_to_string(&stderr_file)
                    .ok()
                    .filter(|s| !s.trim().is_empty());

                let mut msg = format!(
                    "Timeout after {:?} waiting for Claude Code to complete.\n\
                     Session: {}, Pane: {}\n\
                     Output file: '{}'\n\
                     Stderr file: '{}'",
                    t, session, name, output_path, stderr_path
                );

                if let Some(out) = partial {
                    let lines: Vec<&str> = out.lines().collect();
                    let tail: Vec<&str> = lines.iter().rev().take(5).rev().cloned().collect();
                    msg.push_str(&format!("\n\nPartial stdout:\n{}", tail.join("\n")));
                }
                if let Some(err) = stderr_content {
                    let lines: Vec<&str> = err.lines().collect();
                    let tail: Vec<&str> = lines.iter().rev().take(5).rev().cloned().collect();
                    msg.push_str(&format!("\n\nStderr:\n{}", tail.join("\n")));
                }

                anyhow::bail!(msg);
            }
        }

        // Try to read and parse the output file
        if let Ok(contents) = std::fs::read_to_string(output_file) {
            if !contents.trim().is_empty() {
                // Check for Claude Code's expected JSON structure to avoid false positives
                // Claude outputs: {"type": "result", ...} or {"type": "error", ...}
                if let Ok(json) = serde_json::from_str::<serde_json::Value>(&contents) {
                    if json.get("type").is_some() {
                        eprintln!("Claude Code completed after {:?}", start.elapsed());
                        break;
                    }
                }
            }
        }

        // Wait before polling again
        thread::sleep(Duration::from_millis(500));
    }

    // Parse the Claude Code JSON output to extract fields
    let cc_output = std::fs::read_to_string(output_file)
        .context("Failed to read output file")?;
    let cc_json: serde_json::Value = serde_json::from_str(&cc_output)
        .context("Failed to parse Claude Code JSON output")?;

    let result = RunResult {
        exit_code: 0,
        output_file: output_file.clone(),
        stderr_file,
        is_error: cc_json.get("is_error").and_then(|v| v.as_bool()).unwrap_or(false),
        result: cc_json.get("result").and_then(|v| v.as_str()).map(String::from),
        structured_output: cc_json.get("structured_output").cloned(),
        cost_usd: cc_json.get("total_cost_usd").and_then(|v| v.as_f64()),
        num_turns: cc_json.get("num_turns").and_then(|v| v.as_i64()),
    };

    println!("{}", serde_json::to_string_pretty(&result)?);

    Ok(())
}

/// Shell-escape a string for safe use in shell commands
fn shell_quote(s: &str) -> Cow<'_, str> {
    escape(Cow::Borrowed(s))
}
