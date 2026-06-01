//! Root-session bootstrap: create the detached `{session}-exp` window and capture its pane.
//! Pasting (delivery) is NOT here — that goes through the hardened `TmuxIpc::inject_input`
//! (see `impl Tmux for Runtime`).

use exo_caps::{PaneId, TmuxError};
use std::path::Path;

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
        "-n",
        "🤖 root",
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
