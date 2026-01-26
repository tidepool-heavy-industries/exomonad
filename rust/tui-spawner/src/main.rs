//! FIFO-based popup spawner for cross-container TUI communication.
//!
//! This binary handles the coordination between control-server (in one container)
//! and tui-popup (in the Zellij container) using named pipes (FIFOs).
//!
//! Flow:
//! 1. Write PopupDefinition JSON to input file
//! 2. Create FIFO for result
//! 3. Spawn Zellij floating pane with tui-popup
//! 4. Block waiting for tui-popup to write result to FIFO
//! 5. Read result and output to stdout (for Haskell to capture)
//! 6. Cleanup input file and FIFO

use anyhow::{Context, Result};
use clap::Parser;
use nix::libc;
use nix::sys::stat::Mode;
use nix::unistd::mkfifo;
use std::fs::{self, File};
use std::io::{BufRead, BufReader};
use std::os::unix::fs::OpenOptionsExt;
use std::path::PathBuf;
use std::process::Command;
use std::time::{Duration, Instant};

/// FIFO-based popup spawner for Docker cross-container communication.
///
/// Spawns a tui-popup in a Zellij floating pane and waits for the result.
/// Input is PopupDefinition JSON (via argument or stdin).
/// Output is PopupResult JSON (to stdout).
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// PopupDefinition JSON (if not provided, reads from stdin)
    #[arg(long)]
    definition: Option<String>,

    /// Read PopupDefinition from stdin
    #[arg(long)]
    stdin: bool,

    /// Directory for FIFO and input files (default: /sockets)
    #[arg(long, default_value = "/sockets")]
    sockets_dir: PathBuf,

    /// Timeout in seconds for waiting for popup response
    #[arg(long, default_value = "300")]
    timeout: u64,

    /// Zellij container name (for docker exec)
    #[arg(long, default_value = "tidepool-zellij")]
    zellij_container: String,

    /// Zellij session name (required for targeting the correct session)
    #[arg(long, default_value = "tidepool", env = "ZELLIJ_SESSION_NAME")]
    zellij_session: String,

    /// Dry run: print commands instead of executing
    #[arg(long)]
    dry_run: bool,
}

fn main() -> Result<()> {
    // Setup logging to stderr
    tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .with_max_level(tracing::Level::INFO)
        .init();

    let args = Args::parse();

    // Get PopupDefinition JSON
    let definition_json = if let Some(ref json) = args.definition {
        json.clone()
    } else if args.stdin {
        let mut buffer = String::new();
        std::io::stdin()
            .read_line(&mut buffer)
            .context("Failed to read from stdin")?;
        buffer
    } else {
        anyhow::bail!("Either --definition or --stdin must be provided");
    };

    // Generate unique ID for this popup
    let popup_id = uuid::Uuid::new_v4();
    let input_path = args.sockets_dir.join(format!("popup-{}-in.json", popup_id));
    let fifo_path = args.sockets_dir.join(format!("popup-{}.fifo", popup_id));

    tracing::info!("Popup ID: {}", popup_id);
    tracing::info!("Input file: {}", input_path.display());
    tracing::info!("FIFO: {}", fifo_path.display());

    // Ensure cleanup on exit (even on panic)
    let input_path_cleanup = input_path.clone();
    let fifo_path_cleanup = fifo_path.clone();

    // Catch panics and cleanup
    let result = std::panic::catch_unwind(|| {
        run_popup(
            &args,
            &definition_json,
            &input_path,
            &fifo_path,
        )
    });

    // Cleanup regardless of success/failure
    cleanup(&input_path_cleanup, &fifo_path_cleanup);

    // Re-raise panic if one occurred
    match result {
        Ok(Ok(output)) => {
            // Output result to stdout (Haskell reads this)
            println!("{}", output);
            Ok(())
        }
        Ok(Err(e)) => Err(e),
        Err(panic) => std::panic::resume_unwind(panic),
    }
}

fn run_popup(
    args: &Args,
    definition_json: &str,
    input_path: &PathBuf,
    fifo_path: &PathBuf,
) -> Result<String> {
    // 1. Write input file
    fs::write(input_path, definition_json)
        .with_context(|| format!("Failed to write input file: {}", input_path.display()))?;
    tracing::info!("Wrote input file");

    // 2. Create FIFO
    mkfifo(fifo_path, Mode::S_IRUSR | Mode::S_IWUSR | Mode::S_IRGRP | Mode::S_IWGRP)
        .with_context(|| format!("Failed to create FIFO: {}", fifo_path.display()))?;
    tracing::info!("Created FIFO");

    // 3. Spawn Zellij floating pane
    // Must use --session flag to target the correct Zellij session
    // CRITICAL: Run as UID 1000 to avoid root-owned files in /sockets
    // (tui-spawner runs as UID 1000, so files it creates must be writable)
    let zellij_args = [
        "exec", "-u", "1000", &args.zellij_container,
        "zellij", "--session", &args.zellij_session,
        "action", "new-pane", "--floating", "--close-on-exit",
        "--", "tui-popup",
        "--input", input_path.to_str().unwrap(),
        "--output", fifo_path.to_str().unwrap(),
    ];

    if args.dry_run {
        tracing::info!("DRY RUN: docker {}", zellij_args.join(" "));
        return Ok(r#"{"button":"submit","values":{}}"#.to_string());
    }

    let status = Command::new("docker")
        .args(&zellij_args)
        .status()
        .context("Failed to execute docker exec")?;

    if !status.success() {
        anyhow::bail!("docker exec failed with status: {}", status);
    }
    tracing::info!("Spawned Zellij pane");

    // 4. Open FIFO with timeout (handles crash-before-open)
    let timeout = Duration::from_secs(args.timeout);
    let file = open_fifo_with_timeout(fifo_path, timeout)?;
    tracing::info!("FIFO opened");

    // 5. Read result (blocks until tui-popup writes + exits)
    let mut reader = BufReader::new(file);
    let mut content = String::new();
    reader.read_line(&mut content)
        .context("Failed to read from FIFO")?;

    // 6. Handle crash-after-open (empty read)
    if content.is_empty() {
        anyhow::bail!("TUI exited without returning result (empty FIFO)");
    }

    tracing::info!("Got result: {} bytes", content.len());
    Ok(content.trim().to_string())
}

/// Open FIFO with timeout.
///
/// This handles the case where tui-popup crashes before opening the FIFO.
/// We use non-blocking open with a poll loop, then reopen in blocking mode.
fn open_fifo_with_timeout(fifo_path: &PathBuf, timeout: Duration) -> Result<File> {
    let start = Instant::now();

    loop {
        // Try non-blocking open
        match fs::OpenOptions::new()
            .read(true)
            .custom_flags(libc::O_NONBLOCK)
            .open(fifo_path)
        {
            Ok(_) => {
                // Writer has connected, reopen in blocking mode for actual read
                let file = File::open(fifo_path)
                    .with_context(|| format!("Failed to reopen FIFO: {}", fifo_path.display()))?;
                return Ok(file);
            }
            Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                // FIFO not ready yet (no writer)
            }
            Err(e) if e.raw_os_error() == Some(libc::ENXIO) => {
                // No writer yet on FIFO (Linux-specific)
            }
            Err(e) => {
                return Err(e).with_context(|| {
                    format!("Failed to open FIFO: {}", fifo_path.display())
                });
            }
        }

        // Check timeout
        if start.elapsed() > timeout {
            anyhow::bail!(
                "Timeout waiting for TUI to start after {} seconds",
                timeout.as_secs()
            );
        }

        // Wait a bit before retrying
        std::thread::sleep(Duration::from_millis(100));
    }
}

/// Cleanup input file and FIFO.
fn cleanup(input_path: &PathBuf, fifo_path: &PathBuf) {
    if input_path.exists() {
        if let Err(e) = fs::remove_file(input_path) {
            tracing::warn!("Failed to remove input file: {}", e);
        } else {
            tracing::info!("Removed input file");
        }
    }
    if fifo_path.exists() {
        if let Err(e) = fs::remove_file(fifo_path) {
            tracing::warn!("Failed to remove FIFO: {}", e);
        } else {
            tracing::info!("Removed FIFO");
        }
    }
}
