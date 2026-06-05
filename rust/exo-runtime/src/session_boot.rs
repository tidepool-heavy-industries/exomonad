//! Root-session bootstrap: create the detached `{session}-exp` window and capture its pane.
//! Pasting (delivery) is NOT here — that goes through the hardened `TmuxIpc::inject_input`
//! (see `impl Tmux for Runtime`).

use exo_caps::{PaneId, TmuxError};
use std::path::Path;

/// Size of the controlling terminal as `(cols, rows)`, read from `/dev/tty` via `stty size`.
/// `None` when there's no tty (e.g. a headless/cron run) — caller falls back to tmux's default.
fn terminal_size() -> Option<(u16, u16)> {
    let tty = std::fs::File::open("/dev/tty").ok()?;
    let out = std::process::Command::new("stty")
        .arg("size")
        .stdin(tty)
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    let s = String::from_utf8_lossy(&out.stdout);
    let mut it = s.split_whitespace();
    let rows: u16 = it.next()?.parse().ok()?;
    let cols: u16 = it.next()?.parse().ok()?;
    Some((cols, rows))
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

    let cwd_str = cwd.to_string_lossy().into_owned();
    let mut args: Vec<String> = vec![
        "new-session".into(),
        "-d".into(),
        "-s".into(),
        session.into(),
        "-c".into(),
        cwd_str,
        "-n".into(),
        "🤖 root".into(),
    ];
    // Size the detached session to the controlling terminal so the root window (and the `claude`
    // TUI launched into it) starts full-size. Without this, a detached `new-session` defaults to
    // 80x24 and the TUI doesn't repaint on attach — child windows, created while a client is
    // attached, already inherit the client size, so only the root needs this.
    if let Some((cols, rows)) = terminal_size() {
        args.extend(["-x".into(), cols.to_string(), "-y".into(), rows.to_string()]);
    }
    args.extend(["-P".into(), "-F".into(), "#{pane_id}".into()]);
    tracing::info!("Executing: tmux {}", args.join(" "));
    let output = tokio::process::Command::new("tmux")
        .args(&args)
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
