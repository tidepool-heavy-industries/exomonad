use exo_caps::{PaneId, TmuxError};
use std::path::Path;

async fn tmux(op: &'static str, args: &[&str]) -> Result<std::process::Output, TmuxError> {
    tracing::info!("Executing: tmux {}", args.join(" "));
    let output = tokio::process::Command::new("tmux")
        .args(args)
        .output()
        .await
        .map_err(TmuxError::Io)?;

    tracing::info!("tmux {} returned: {:?}", args[0], output.status);
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        tracing::error!(
            "tmux {} failed with status: {} - {}",
            args[0],
            output.status,
            stderr
        );
        return Err(TmuxError::Failed {
            op,
            detail: stderr.to_string(),
        });
    }
    Ok(output)
}

fn parse_pane_id(stdout: &str) -> Result<PaneId, TmuxError> {
    let s = stdout.trim().to_string();
    PaneId::new(s).map_err(|e| TmuxError::Failed {
        op: "boot_root_session",
        detail: format!("parse pane id: {}", e),
    })
}

pub async fn boot_root_session(
    session: &str,
    cwd: &Path,
    recreate: bool,
) -> Result<PaneId, TmuxError> {
    if recreate {
        let args = ["kill-session", "-t", session];
        tracing::info!("Executing: tmux {}", args.join(" "));
        let status = tokio::process::Command::new("tmux")
            .args(args)
            .status()
            .await;
        match status {
            Ok(s) => tracing::info!("tmux kill-session returned: {:?}", s),
            Err(e) => tracing::error!("tmux kill-session failed to execute: {}", e),
        }
    }

    let cwd_str = cwd.to_string_lossy();
    let args = [
        "new-session",
        "-d",
        "-s",
        session,
        "-c",
        &cwd_str,
        "-P",
        "-F",
        "#{pane_id}",
    ];
    tracing::info!("Executing: tmux {}", args.join(" "));
    let output = tokio::process::Command::new("tmux")
        .args(args)
        .output()
        .await
        .map_err(TmuxError::Io)?;

    tracing::info!("tmux new-session returned: {:?}", output.status);
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        tracing::error!("tmux new-session FAILED: {}", stderr);
        return Err(TmuxError::Failed {
            op: "boot_root_session",
            detail: stderr.into_owned(),
        });
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    parse_pane_id(&stdout)
}

pub async fn paste_to_pane(session: &str, pane: &PaneId, text: &str) -> Result<(), TmuxError> {
    let _ = session; // Unused but kept for signature consistency and possible future use
    let target = pane.as_str();

    // 1. Exit copy/scroll mode if active — copy mode intercepts input (matches TmuxIpc)
    let mode_output = tokio::process::Command::new("tmux")
        .args(["display-message", "-p", "-t", target, "#{pane_in_mode}"])
        .output()
        .await;

    if let Ok(output) = mode_output {
        if output.status.success() && String::from_utf8_lossy(&output.stdout).trim() == "1" {
            let _ = tokio::process::Command::new("tmux")
                .args(["send-keys", "-t", target, "-X", "cancel"])
                .output()
                .await;
            tokio::time::sleep(std::time::Duration::from_millis(50)).await;
        }
    }

    // 2. Buffer paste pattern — load-buffer temp + paste-buffer + send-keys Enter
    let payload = text.trim_end_matches('\n').trim_end_matches('\r');
    // NamedTempFile::new() does sync filesystem work; keep it off the async executor.
    let tmp = tokio::task::spawn_blocking(tempfile::NamedTempFile::new)
        .await
        .map_err(|e| TmuxError::Failed {
            op: "paste",
            detail: format!("temp-file task join: {e}"),
        })?
        .map_err(TmuxError::Io)?;
    let tmp_path = tmp.path().to_path_buf();
    tokio::fs::write(&tmp_path, payload)
        .await
        .map_err(TmuxError::Io)?;

    // Use temp file name as a unique buffer name
    let buf_name = format!(
        "exo_{}",
        tmp_path.file_name().unwrap_or_default().to_string_lossy()
    );
    let tmp_path_str = tmp_path.to_string_lossy().into_owned();

    tmux("paste", &["load-buffer", "-b", &buf_name, &tmp_path_str]).await?;

    tmux(
        "paste",
        &["paste-buffer", "-t", target, "-b", &buf_name, "-d"],
    )
    .await?;

    // Debounce: allow TUI (Claude Code Ink, Gemini CLI readline) to process pasted text
    tokio::time::sleep(std::time::Duration::from_millis(150)).await;

    tmux("paste", &["send-keys", "-t", target, "Enter"]).await?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_pane_id() {
        assert_eq!(
            parse_pane_id("%123\n").unwrap(),
            PaneId::new("%123".into()).unwrap()
        );
        assert!(parse_pane_id("123").is_err());
    }
}
