//! tmux event emission and input injection service.
//!
//! Replaces the original event system. Events are log-only (no sidebar).
//! Input injection uses tmux buffer pattern for multiline safety.

use crate::ui_protocol::AgentEvent;
use anyhow::{Context, Result};
use std::path::Path;
use tracing::{info, warn};

use super::tmux_ipc::{PaneId, TmuxIpc};

/// Emit an agent event. Log-only — no tmux interaction.
/// Keeps function signature for callers.
pub fn emit_event(_session: &str, event: &AgentEvent) -> Result<()> {
    let json = serde_json::to_string(event).context("Failed to serialize event")?;
    info!("[TmuxEvents] Event: {}", json);
    Ok(())
}

/// Extract a short label from the first line of text.
/// Strips leading markdown headers (`# `, `## `, `### `) and sanitizes
/// shell metacharacters so the pointer itself is safe for tmux injection.
/// Allows alphanumeric characters, spaces, hyphens, underscores, and dots.
fn extract_label(text: &str) -> String {
    let first_line = text.lines().next().unwrap_or("feedback");
    let stripped = first_line
        .strip_prefix("### ")
        .or_else(|| first_line.strip_prefix("## "))
        .or_else(|| first_line.strip_prefix("# "))
        .unwrap_or(first_line);
    let label: String = stripped
        .trim()
        .chars()
        .filter(|c| c.is_alphanumeric() || *c == ' ' || *c == '-' || *c == '_' || *c == '.')
        .take(80)
        .collect();
    if label.is_empty() {
        "feedback".to_string()
    } else {
        label
    }
}

/// Delete tmp files older than 1 hour. Best-effort, errors are logged and ignored.
async fn gc_tmp_dir(tmp_dir: &Path) {
    let mut entries = match tokio::fs::read_dir(tmp_dir).await {
        Ok(e) => e,
        Err(_) => return,
    };
    let cutoff = std::time::SystemTime::now() - std::time::Duration::from_secs(3600);
    while let Ok(Some(entry)) = entries.next_entry().await {
        let Ok(meta) = entry.metadata().await else {
            continue;
        };
        let is_old = meta.modified().map(|m| m < cutoff).unwrap_or(false);
        if is_old {
            if let Err(e) = tokio::fs::remove_file(entry.path()).await {
                warn!(path = %entry.path().display(), error = %e, "Failed to GC tmp file");
            }
        }
    }
}

/// Inject text into a target pane/window via tmux buffer pattern.
///
/// `target` is a tmux target specifier (pane_id like %N, or window name).
///
/// For multiline text, writes content to `.exo/tmp/{uuid}.txt` and injects
/// a short `[label] @.exo/tmp/{uuid}.txt` pointer instead. This avoids shell
/// metacharacter expansion (`!`, `$`, backticks) when pasting into tmux.
/// Uses a relative path so directory names containing metacharacters are safe.
/// Both Claude and Gemini auto-read `@filepath` references.
///
/// Tmp files older than 1 hour are garbage-collected on each write.
pub async fn inject_input(target: &str, text: &str, project_dir: &Path) -> Result<()> {
    let session =
        std::env::var("EXOMONAD_TMUX_SESSION").context("EXOMONAD_TMUX_SESSION not set")?;

    let ipc = TmuxIpc::new(&session);

    if text.contains('\n') {
        let tmp_dir = project_dir.join(".exo/tmp");
        tokio::fs::create_dir_all(&tmp_dir)
            .await
            .context("Failed to create .exo/tmp/")?;

        // GC old files before writing new one
        gc_tmp_dir(&tmp_dir).await;

        let filename = format!("{}.txt", uuid::Uuid::new_v4());
        let relative_path = format!(".exo/tmp/{}", filename);
        let file_path = tmp_dir.join(&filename);
        tokio::fs::write(&file_path, text)
            .await
            .context("Failed to write feedback file")?;

        let label = extract_label(text);
        let pointer = format!("[{}] @{}", label, relative_path);

        info!(
            "[TmuxEvents] Injecting file-indirect to target '{}': {} chars -> {}",
            target,
            text.len(),
            relative_path
        );

        let target = target.to_string();
        ipc.inject_input(&target, &pointer).await
    } else {
        info!(
            "[TmuxEvents] Injecting input to target '{}': {} chars",
            target,
            text.len()
        );

        let target = target.to_string();
        let text = text.to_string();
        ipc.inject_input(&target, &text).await
    }
}

/// Close a worker pane by pane_id.
pub async fn close_worker_pane(pane_id: &str) -> Result<()> {
    let session =
        std::env::var("EXOMONAD_TMUX_SESSION").context("EXOMONAD_TMUX_SESSION not set")?;

    info!("[TmuxEvents] Closing worker pane '{}'", pane_id);

    let pid = PaneId::parse(pane_id).context("Invalid pane_id")?;
    let ipc = TmuxIpc::new(&session);

    ipc.kill_pane(&pid).await
}

/// Helper to get current timestamp in ISO 8601 format.
pub fn now_iso8601() -> String {
    chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_label_markdown_h1() {
        assert_eq!(
            extract_label("# Copilot Review\nSome details"),
            "Copilot Review"
        );
    }

    #[test]
    fn test_extract_label_markdown_h2() {
        assert_eq!(
            extract_label("## Changes Requested\nFix this"),
            "Changes Requested"
        );
    }

    #[test]
    fn test_extract_label_markdown_h3() {
        assert_eq!(extract_label("### Minor Note\nDetails"), "Minor Note");
    }

    #[test]
    fn test_extract_label_plain_text() {
        assert_eq!(
            extract_label("Some feedback here\nMore lines"),
            "Some feedback here"
        );
    }

    #[test]
    fn test_extract_label_empty_first_line() {
        assert_eq!(extract_label("\nContent below"), "feedback");
    }

    #[test]
    fn test_extract_label_empty_string() {
        assert_eq!(extract_label(""), "feedback");
    }

    #[test]
    fn test_extract_label_sanitizes_metacharacters() {
        assert_eq!(
            extract_label("# Fix the `bug` with $HOME!\nDetails"),
            "Fix the bug with HOME"
        );
    }

    #[test]
    fn test_extract_label_truncates_long_labels() {
        let long = "A".repeat(200);
        let label = extract_label(&format!("# {}\nBody", long));
        assert_eq!(label.len(), 80);
    }
}
