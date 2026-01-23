use anyhow::{Context, Result};
use clap::Parser;
use std::io::{self, Read};
use tui_sidebar::popup::run_popup;
use tui_sidebar::protocol::PopupDefinition;

/// TUI popup binary for Zellij pane popups.
///
/// Accepts PopupDefinition via CLI arg or stdin, renders it, captures user input,
/// and outputs PopupResult JSON to stdout. Exits when the user submits or cancels.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// PopupDefinition JSON string
    #[arg(long)]
    spec: Option<String>,

    /// Read PopupDefinition from stdin instead of --spec
    #[arg(long)]
    stdin: bool,
}

fn main() -> Result<()> {
    // Setup minimal logging to stderr (avoid polluting stdout)
    tracing_subscriber::fmt()
        .with_writer(io::stderr)
        .with_max_level(tracing::Level::WARN)
        .init();

    let args = Args::parse();

    // Get PopupDefinition from --spec or stdin
    let json = if args.stdin {
        let mut buffer = String::new();
        io::stdin()
            .read_to_string(&mut buffer)
            .context("Failed to read from stdin")?;
        buffer
    } else if let Some(spec) = args.spec {
        spec
    } else {
        anyhow::bail!("Either --spec or --stdin must be provided");
    };

    // Parse PopupDefinition
    let definition: PopupDefinition =
        serde_json::from_str(&json).context("Failed to parse PopupDefinition JSON")?;

    // Run popup (blocking)
    let result = run_popup(definition).context("Popup failed")?;

    // Output PopupResult to stdout
    println!("{}", serde_json::to_string(&result)?);

    Ok(())
}
