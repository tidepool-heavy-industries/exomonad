use crate::{InboxPath, PaneId};
use std::path::{Path, PathBuf};

/// Child's OWN ingestion inbox: `{home}/.claude/exo/inboxes/{run_id}/pane-{n}.jsonl`.
pub fn inbox_path(home: &Path, run_id: &str, pane: &PaneId) -> InboxPath {
    let n = pane.as_str().trim_start_matches('%');
    InboxPath::new(
        home.join(".claude/exo/inboxes")
            .join(run_id)
            .join(format!("pane-{n}.jsonl")),
    )
}

/// Child's NodePapers path: `{home}/.claude/exo/papers/{run_id}/pane-{n}.json`.
pub fn papers_path(home: &Path, run_id: &str, pane: &PaneId) -> PathBuf {
    let n = pane.as_str().trim_start_matches('%');
    home.join(".claude/exo/papers")
        .join(run_id)
        .join(format!("pane-{n}.json"))
}

/// Child's NodeStatus path: `{home}/.claude/exo/status/{run_id}/pane-{n}.json`.
pub fn status_path(home: &Path, run_id: &str, pane: &PaneId) -> PathBuf {
    let n = pane.as_str().trim_start_matches('%');
    home.join(".claude/exo/status")
        .join(run_id)
        .join(format!("pane-{n}.json"))
}

/// A Gemini child's `settings.json`: `{home}/.claude/exo/agents/{run_id}/pane-{n}/settings.json`.
/// Per-pane (NOT under the child's worktree) because **inline** children share their parent's
/// worktree as cwd — writing `settings.json` there would have siblings clobber each other's
/// config (and thus their identity). Gemini reads it via the absolute `GEMINI_CLI_SYSTEM_SETTINGS_PATH`
/// env var, so the location is free to be per-pane.
pub fn gemini_settings_path(home: &Path, run_id: &str, pane: &PaneId) -> PathBuf {
    let n = pane.as_str().trim_start_matches('%');
    home.join(".claude/exo/agents")
        .join(run_id)
        .join(format!("pane-{n}"))
        .join("settings.json")
}

/// Sidecar-owned cursor for the outbound Teams watcher: a JSON map `{member → processed-count}`
/// at `{home}/.claude/exo/teamcursor/{run_id}/pane-{n}.json`. We track our OWN high-water-mark
/// here rather than marking CC's inbox `read` — CC is the concurrent writer of those inboxes, so
/// we never write them. Survives a sidecar restart (no re-forwarding of already-bridged messages).
pub fn team_cursor_path(home: &Path, run_id: &str, pane: &PaneId) -> PathBuf {
    let n = pane.as_str().trim_start_matches('%');
    home.join(".claude/exo/teamcursor")
        .join(run_id)
        .join(format!("pane-{n}.json"))
}

/// Node's hook-RPC socket: `{home}/.claude/exo/sockets/{run_id}/pane-{n}.sock`. The sidecar
/// binds it; the `exomonad experimental hook` client connects to it. Home-based (like inboxes),
/// NOT under the worktree — so a live socket file can never dirty a worktree and trip the
/// `stop` clean-gate. Both ends derive it identically from papers (run_id + own pane).
pub fn hook_sock(home: &Path, run_id: &str, pane: &PaneId) -> PathBuf {
    let n = pane.as_str().trim_start_matches('%');
    home.join(".claude/exo/sockets")
        .join(run_id)
        .join(format!("pane-{n}.sock"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::PaneId;

    #[test]
    fn test_paths_strip_percent() {
        let home = Path::new("/home/user");
        let run_id = "run-42";
        let pane = PaneId::new("%317".into()).unwrap();

        let inbox = inbox_path(home, run_id, &pane);
        assert_eq!(
            inbox.as_path(),
            Path::new("/home/user/.claude/exo/inboxes/run-42/pane-317.jsonl")
        );

        let papers = papers_path(home, run_id, &pane);
        assert_eq!(
            papers,
            Path::new("/home/user/.claude/exo/papers/run-42/pane-317.json")
        );

        let settings = gemini_settings_path(home, run_id, &pane);
        assert_eq!(
            settings,
            Path::new("/home/user/.claude/exo/agents/run-42/pane-317/settings.json")
        );
    }

    #[test]
    fn gemini_settings_path_is_per_pane() {
        // The whole point of the fix: two inline siblings (sharing a worktree) get DISTINCT
        // settings files, so neither clobbers the other's papers pointer / identity.
        let home = Path::new("/home/user");
        let a = gemini_settings_path(home, "run", &PaneId::new("%31".into()).unwrap());
        let b = gemini_settings_path(home, "run", &PaneId::new("%32".into()).unwrap());
        assert_ne!(a, b);
    }

    #[test]
    fn test_paths_no_percent_defensive() {
        // PaneId::new would actually reject this, but the helper should be robust
        // if we ever bypassed it or changed PaneId.
        // Actually PaneId::new is the only way to get a PaneId.
        // Let's just test that it works as expected.
        let home = Path::new(".");
        let run_id = "run";
        let pane = PaneId::new("%1".into()).unwrap();
        let inbox = inbox_path(home, run_id, &pane);
        assert!(inbox.as_path().to_string_lossy().ends_with("pane-1.jsonl"));
    }
}
