use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use serde::Serialize;
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

        /// Output file for JSON result
        #[arg(long)]
        output_file: PathBuf,

        /// Working directory for Claude Code
        #[arg(long)]
        cwd: Option<PathBuf>,

        /// Timeout in seconds (0 = no timeout)
        #[arg(long, default_value = "300")]
        timeout: u64,
    },
}

#[derive(Serialize)]
struct RunResult {
    exit_code: i32,
    output_file: PathBuf,
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
            timeout,
        } => {
            run_cc_session(&session, &name, &model, &prompt, &output_file, cwd.as_ref(), timeout)?;
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
    timeout_secs: u64,
) -> Result<()> {
    // Build the command to run in the pane
    // Uses tee to both show output in pane AND capture to file
    let output_path = output_file.display().to_string();

    // Build shell command with proper quoting
    // -p enables print mode (non-interactive, exits after response)
    // --dangerously-skip-permissions bypasses tool permission prompts
    // --output-format json returns structured JSON
    let shell_cmd = format!(
        "claude --dangerously-skip-permissions --output-format json --model {} -p {} | tee {}",
        shell_quote(model),
        shell_quote(prompt),
        shell_quote(&output_path),
    );

    let os_input = get_cli_client_os_input()
        .context("Failed to get zellij OS input - is zellij running?")?;

    // Spawn pane with the command
    // NewTiledPane(direction, command, pane_name)
    let actions = vec![Action::NewTiledPane(
        None, // direction
        Some(RunCommandAction {
            command: PathBuf::from("sh"),
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

    eprintln!("Waiting for Claude Code to complete (output: {})...", output_path);

    loop {
        // Check timeout
        if let Some(t) = timeout {
            if start.elapsed() > t {
                anyhow::bail!("Timeout waiting for Claude Code to complete");
            }
        }

        // Try to read and parse the output file
        if let Ok(contents) = std::fs::read_to_string(output_file) {
            // Claude headless JSON output is complete when we can parse it
            if !contents.trim().is_empty() {
                if let Ok(_json) = serde_json::from_str::<serde_json::Value>(&contents) {
                    eprintln!("Claude Code completed after {:?}", start.elapsed());
                    break;
                }
            }
        }

        // Wait before polling again
        thread::sleep(Duration::from_millis(500));
    }

    let result = RunResult {
        exit_code: 0, // We can't easily get the actual exit code from zellij
        output_file: output_file.clone(),
    };

    println!("{}", serde_json::to_string_pretty(&result)?);

    Ok(())
}

/// Simple shell quoting (wrap in single quotes, escape internal quotes)
fn shell_quote(s: &str) -> String {
    format!("'{}'", s.replace('\'', "'\\''"))
}
