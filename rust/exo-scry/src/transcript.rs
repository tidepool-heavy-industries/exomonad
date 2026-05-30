//! Locate a Claude Code session's transcript from its working directory.
//!
//! Claude Code stores each session's transcript at
//! `~/.claude/projects/{escaped-cwd}/{session-uuid}.jsonl`, where the cwd is
//! escaped by replacing `/` and `.` with `-`. This module is the fully portable
//! core of the cwd-based signal: given a project dir, the *active* session is
//! the most recently written transcript. Disambiguating which live process owns
//! that transcript (when several share a cwd) is the caller's job — this layer
//! only reports the newest transcript, not which pid it belongs to.

use std::os::unix::fs::MetadataExt;
use std::path::{Path, PathBuf};

/// Claude Code's project-dir name for a working directory: every `/` and `.`
/// becomes `-`. Verified against live project dirs (worktree paths included).
pub fn escape_cwd(cwd: &Path) -> String {
    cwd.to_string_lossy()
        .chars()
        .map(|c| if c == '/' || c == '.' { '-' } else { c })
        .collect()
}

/// The transcript directory for a cwd: `{projects_root}/{escaped-cwd}`.
pub fn project_dir(projects_root: &Path, cwd: &Path) -> PathBuf {
    projects_root.join(escape_cwd(cwd))
}

/// The session UUID of the most recently written transcript in `project_dir`
/// (its `.jsonl` basename), or `None` if the dir is absent or holds none.
///
/// With a single live session per cwd this is unambiguously that session's
/// current transcript — older `.jsonl` files are its past sessions and are
/// correctly ignored.
pub fn newest_session(project_dir: &Path) -> std::io::Result<Option<String>> {
    let entries = match std::fs::read_dir(project_dir) {
        Ok(e) => e,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(e) => return Err(e),
    };
    let mut newest: Option<(i64, String)> = None;
    for entry in entries {
        let Ok(entry) = entry else { continue };
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("jsonl") {
            continue;
        }
        let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else {
            continue;
        };
        let Ok(meta) = entry.metadata() else { continue };
        let mtime = meta.mtime();
        if newest.as_ref().is_none_or(|(m, _)| mtime > *m) {
            newest = Some((mtime, stem.to_string()));
        }
    }
    Ok(newest.map(|(_, uuid)| uuid))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn escapes_slashes_and_dots() {
        assert_eq!(
            escape_cwd(Path::new("/home/inanna/dev/exomonad")),
            "-home-inanna-dev-exomonad"
        );
        assert_eq!(
            escape_cwd(Path::new("/home/inanna/dev/exomonad/.exo/worktrees/address-type")),
            "-home-inanna-dev-exomonad--exo-worktrees-address-type"
        );
    }

    #[test]
    fn newest_session_picks_latest_mtime() {
        use std::time::{Duration, SystemTime};
        let root = std::env::temp_dir().join(format!("exo-scry-tx-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        std::fs::create_dir_all(&root).unwrap();
        // Deterministic mtimes (no sleep): beta newer than alpha; ignore non-jsonl.
        for (name, secs) in [("alpha.jsonl", 1000), ("beta.jsonl", 2000), ("notes.txt", 9000)] {
            std::fs::write(root.join(name), b"{}").unwrap();
            let f = std::fs::File::options().write(true).open(root.join(name)).unwrap();
            f.set_modified(SystemTime::UNIX_EPOCH + Duration::from_secs(secs)).unwrap();
        }
        assert_eq!(newest_session(&root).unwrap().as_deref(), Some("beta"));
        std::fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn newest_session_missing_dir_is_none() {
        let root = std::env::temp_dir().join("exo-scry-tx-nonexistent-xyzzy");
        let _ = std::fs::remove_dir_all(&root);
        assert_eq!(newest_session(&root).unwrap(), None);
    }
}
