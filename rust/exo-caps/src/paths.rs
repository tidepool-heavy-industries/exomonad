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

/// A node's private CC config files, derived as **siblings** of its papers file:
/// `(settings, mcp)` = `<papers>.settings.json`, `<papers>.mcp.json`. These are passed to
/// `claude` via `--settings`/`--mcp-config` so the node NEVER writes the shared cwd's
/// `.claude/settings.local.json` / `.mcp.json` (which an inline worker would clobber, and which
/// made the root `.mcp.json` a git-tracked footgun). Living beside the papers keeps them per-node:
/// the root's under `.exo/node/{run}/`, a worktree child's under its `.exo/`, an inline worker's
/// under `~/.claude/exo/papers/{run}/` — the last is OUTSIDE the shared cwd, so no clobber.
pub fn node_config_paths(papers: &Path) -> (PathBuf, PathBuf) {
    (
        papers.with_extension("settings.json"),
        papers.with_extension("mcp.json"),
    )
}

/// Child's NodeStatus path: `{home}/.claude/exo/status/{run_id}/pane-{n}.json`.
pub fn status_path(home: &Path, run_id: &str, pane: &PaneId) -> PathBuf {
    let n = pane.as_str().trim_start_matches('%');
    home.join(".claude/exo/status")
        .join(run_id)
        .join(format!("pane-{n}.json"))
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
/// binds it; the `exo hook` client connects to it. Home-based (like inboxes),
/// NOT under the worktree — so a live socket file can never dirty a worktree and trip the
/// `stop` clean-gate. Both ends derive it identically from papers (run_id + own pane).
pub fn hook_sock(home: &Path, run_id: &str, pane: &PaneId) -> PathBuf {
    let n = pane.as_str().trim_start_matches('%');
    home.join(".claude/exo/sockets")
        .join(run_id)
        .join(format!("pane-{n}.sock"))
}

/// Node's listen-channel socket: `{home}/.claude/exo/sockets/{run_id}/pane-{n}.listen.sock`.
/// Streaming, newline-framed — NOT the one-shot EOF-framed hooksock protocol, hence a second
/// socket beside [`hook_sock`]. The sidecar binds it; the `exo listen` Monitor client connects
/// and receives every dispatched message as a frame, acking each after flushing it to stdout.
pub fn listen_sock(home: &Path, run_id: &str, pane: &PaneId) -> PathBuf {
    let n = pane.as_str().trim_start_matches('%');
    home.join(".claude/exo/sockets")
        .join(run_id)
        .join(format!("pane-{n}.listen.sock"))
}

/// Advisory lock held by the one MCP connection that owns this node's background loops.
/// Codex may open multiple stdio connections to the same configured MCP server; all of them
/// serve tools, but only the lock holder may consume the inbox or bind the node's sockets.
pub fn sidecar_owner_lock(home: &Path, run_id: &str, pane: &PaneId) -> PathBuf {
    let n = pane.as_str().trim_start_matches('%');
    home.join(".claude/exo/sockets")
        .join(run_id)
        .join(format!("pane-{n}.owner.lock"))
}

/// A sibling of the inbox used solely to generate a cross-process filesystem wake event after
/// a Codex connection learns the thread binding.
pub fn binding_wake_path(inbox: &InboxPath) -> PathBuf {
    inbox.as_path().with_extension("binding-wake")
}

/// Recover the pane id encoded in an inbox path (`…/pane-{n}.jsonl` → `%{n}`).
///
/// The bus keys every per-node file by pane, so an inbox path is enough to reach the sibling
/// status/socket files of the node it belongs to — this is how a *sender* holding only a
/// recipient's `InboxPath` (ledger row or `parent_inbox`) checks that recipient's
/// [`status_path`] for listener liveness. `None` for a path that doesn't match the scheme.
pub fn pane_from_inbox(inbox: &InboxPath) -> Option<PaneId> {
    let name = inbox.as_path().file_name()?.to_str()?;
    let n = name.strip_prefix("pane-")?.strip_suffix(".jsonl")?;
    PaneId::new(format!("%{n}")).ok()
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
    fn node_config_paths_are_siblings_of_papers() {
        let (s, m) = node_config_paths(Path::new("/x/.exo/node/run-1/root.json"));
        assert_eq!(s, Path::new("/x/.exo/node/run-1/root.settings.json"));
        assert_eq!(m, Path::new("/x/.exo/node/run-1/root.mcp.json"));

        // Inline worker papers live under $HOME — config siblings land there too (outside the cwd).
        let (s, m) = node_config_paths(Path::new("/home/u/.claude/exo/papers/run-1/pane-3.json"));
        assert_eq!(
            s,
            Path::new("/home/u/.claude/exo/papers/run-1/pane-3.settings.json")
        );
        assert_eq!(
            m,
            Path::new("/home/u/.claude/exo/papers/run-1/pane-3.mcp.json")
        );
    }

    #[test]
    fn listen_sock_shape() {
        let pane = PaneId::new("%317".into()).unwrap();
        assert_eq!(
            listen_sock(Path::new("/home/user"), "run-42", &pane),
            Path::new("/home/user/.claude/exo/sockets/run-42/pane-317.listen.sock")
        );
    }

    #[test]
    fn codex_coordination_paths_have_stable_shapes() {
        let pane = PaneId::new("%317".into()).unwrap();
        assert_eq!(
            sidecar_owner_lock(Path::new("/home/user"), "run-42", &pane),
            Path::new("/home/user/.claude/exo/sockets/run-42/pane-317.owner.lock")
        );
        let inbox = inbox_path(Path::new("/home/user"), "run-42", &pane);
        assert_eq!(
            binding_wake_path(&inbox),
            Path::new("/home/user/.claude/exo/inboxes/run-42/pane-317.binding-wake")
        );
    }

    #[test]
    fn pane_from_inbox_roundtrips() {
        let pane = PaneId::new("%317".into()).unwrap();
        let inbox = inbox_path(Path::new("/home/user"), "run-42", &pane);
        assert_eq!(pane_from_inbox(&inbox), Some(pane));

        let bogus = InboxPath::new("/tmp/not-an-inbox.txt".into());
        assert_eq!(pane_from_inbox(&bogus), None);
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
