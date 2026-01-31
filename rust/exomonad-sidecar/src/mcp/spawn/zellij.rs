//! Zellij tab operations for agent spawning.

use anyhow::{anyhow, Context, Result};
use std::path::Path;
use tokio::process::Command;
use tracing::{debug, info, warn};

/// Check if running inside a Zellij session.
///
/// Returns the session name if in Zellij, error otherwise.
pub fn check_zellij_env() -> Result<String> {
    // ZELLIJ is set when running inside Zellij
    // ZELLIJ_SESSION_NAME contains the session name
    if std::env::var("ZELLIJ").is_err() {
        return Err(anyhow!(
            "Not running inside Zellij session (ZELLIJ env var not set)"
        ));
    }

    let session = std::env::var("ZELLIJ_SESSION_NAME").unwrap_or_else(|_| "default".to_string());

    Ok(session)
}

/// Create a new Zellij tab.
///
/// # Arguments
/// * `name` - Tab name (will be visible in Zellij tab bar)
/// * `cwd` - Working directory for the tab
/// * `command` - Optional command to run (e.g., "claude")
///
/// # Returns
/// The tab name on success.
pub async fn new_tab(name: &str, cwd: &Path, command: Option<&str>) -> Result<String> {
    info!(name, cwd = %cwd.display(), command, "Creating Zellij tab");

    let mut args = vec![
        "action".to_string(),
        "new-tab".to_string(),
        "--name".to_string(),
        name.to_string(),
        "--cwd".to_string(),
        cwd.to_string_lossy().to_string(),
    ];

    // Add command if provided
    if let Some(cmd) = command {
        args.push("--".to_string());
        // Split command into parts
        for part in cmd.split_whitespace() {
            args.push(part.to_string());
        }
    }

    debug!(args = ?args, "Executing zellij command");

    let output = Command::new("zellij")
        .args(&args)
        .output()
        .await
        .context("Failed to execute zellij action new-tab")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Log both for debugging
        if !stderr.is_empty() {
            warn!(stderr = %stderr, "zellij stderr");
        }
        if !stdout.is_empty() {
            debug!(stdout = %stdout, "zellij stdout");
        }

        return Err(anyhow!("zellij new-tab failed: {}", stderr));
    }

    Ok(name.to_string())
}

/// Close a Zellij tab by switching to it and closing.
///
/// Note: Zellij doesn't have a direct "close tab by name" command,
/// so we need to use a workaround.
pub async fn close_tab(name: &str) -> Result<()> {
    info!(name, "Closing Zellij tab");

    // First, try to go to the tab
    let output = Command::new("zellij")
        .args(["action", "go-to-tab-name", name])
        .output()
        .await
        .context("Failed to execute zellij go-to-tab-name")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        // Tab might not exist, which is fine
        if stderr.contains("not found") || stderr.contains("No tab") {
            debug!(name, "Tab not found, nothing to close");
            return Ok(());
        }
        warn!(stderr = %stderr, "Failed to switch to tab");
    }

    // Now close the current tab
    let output = Command::new("zellij")
        .args(["action", "close-tab"])
        .output()
        .await
        .context("Failed to execute zellij close-tab")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        warn!(stderr = %stderr, "Failed to close tab (may already be closed)");
    }

    Ok(())
}
