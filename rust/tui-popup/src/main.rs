//! TUI popup binary for Zellij pane popups.
//!
//! FIFO mode only: Reads PopupDefinition from file, writes PopupResult to file/FIFO,
//! renders to /dev/tty (works even with stdout redirected).

use anyhow::{Context, Result};
use clap::Parser;
use std::io::{self, Read};
use std::path::PathBuf;

// Use local modules instead of tui_sidebar
use crate::popup::run_popup_with_tty;
use crate::protocol::PopupDefinition;

mod popup;
mod protocol;
mod realm;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Read PopupDefinition from file (required)
    #[arg(long)]
    input: Option<PathBuf>,

    /// Write PopupResult to file/FIFO (required)
    #[arg(long)]
    output: PathBuf,
}

fn main() -> Result<()> {
    // Setup minimal logging to stderr
    tracing_subscriber::fmt()
        .with_writer(io::stderr)
        .with_max_level(tracing::Level::WARN)
        .init();

    let args = Args::parse();

    // Read definition from input file or stdin
    let json = if let Some(input_path) = args.input {
        std::fs::read_to_string(&input_path)
            .with_context(|| format!("Failed to read input file: {}", input_path.display()))?
    } else {
        let mut buffer = String::new();
        io::stdin()
            .read_to_string(&mut buffer)
            .context("Failed to read from stdin")?;
        buffer
    };

    let definition: PopupDefinition =
        serde_json::from_str(&json).context("Failed to parse PopupDefinition JSON")?;

    // Run popup with /dev/tty backend (blocking)
    let result = run_popup_with_tty(definition)
        .context("Popup failed")?;

    // Write result to output file/FIFO
    let result_json = serde_json::to_string(&result)?;
    std::fs::write(&args.output, &result_json)
        .with_context(|| format!("Failed to write output: {}", args.output.display()))?;

    Ok(())
}