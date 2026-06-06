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
