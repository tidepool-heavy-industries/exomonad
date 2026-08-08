//! Fold-time file boundary — the machine-checkable counterpart to a spawn spec's `boundary`
//! prose. A spawner can optionally persist the list of file paths / directory prefixes a child is
//! allowed to touch; `merge` (`tools/merge.rs`) reads it back and refuses to fold a diff that
//! steps outside it, before the merge commits. Today this check is root-operator discipline (`git
//! diff --name-only $(merge-base)..branch` read by eye against the spec prose) — a reviewer
//! structurally cannot do it, since it never sees the spawn-spec's boundary list. This makes it a
//! mechanism instead.

use exo_caps::{AgentName, CapError, CapResult, Fs};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// The set of file paths / directory prefixes a child is allowed to touch, persisted parent-side
/// at spawn time and checked at fold time. Absence (no file on disk) means "no boundary was
/// recorded" — NOT "empty boundary" — so a boundaryless branch merges exactly as it always did.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct FileBoundary {
    pub allowed: Vec<String>,
}

/// True if `file` is inside the boundary: it equals an allowed entry exactly, or an allowed entry
/// names a directory that contains it (the entry is treated as a directory when `file` starts with
/// `entry` + `/`). Dep-free by design — no glob crate. This is why `"src/lib"` does NOT match
/// `"src/librs"`: the match requires the separator, not just a string prefix.
pub fn matches(allowed: &[String], file: &str) -> bool {
    allowed
        .iter()
        .any(|entry| file == entry || file.starts_with(&format!("{entry}/")))
}

/// Where a child's file boundary is persisted — relative to the SPAWNING node's own worktree (the
/// sidecar's cwd), not the child's. The same node later reads it back from the same cwd at merge
/// time, so this is parent-local bookkeeping, never materialized into the child's own worktree.
pub fn boundary_path(child: &str) -> PathBuf {
    PathBuf::from(format!(".exo/boundaries/{child}.json"))
}

/// Persist a child's file boundary, best-effort — mirrors [`crate::spawn::write_acceptance`]. A
/// write failure only costs the fold-time check (it degrades to "no boundary recorded", i.e. an
/// unrestricted merge), never blocks the spawn itself.
pub async fn write_boundary<C: Fs>(ctx: &C, child: &AgentName, boundary: &FileBoundary) {
    let path = boundary_path(child.as_str());
    let bytes = match serde_json::to_vec(boundary) {
        Ok(b) => b,
        Err(e) => {
            tracing::warn!(
                "failed to serialize file boundary for {}: {e}",
                child.as_str()
            );
            return;
        }
    };
    if let Err(e) = ctx.write_atomic(&path, &bytes).await {
        tracing::warn!(
            "failed to persist file boundary for {}: {e}",
            child.as_str()
        );
    }
}

/// Read back a child's persisted file boundary. `Ok(None)` means no boundary file exists — the
/// ordinary case for a child spawned without `file_boundary`, or with an older ledger predating
/// this mechanism — and the fold-time check must treat that as "unrestricted", not as an error.
/// Any other failure (unreadable file, corrupt JSON) is loud: a boundary that exists but can't be
/// read must never be silently treated as "no boundary".
pub async fn read_boundary<C: Fs>(ctx: &C, child: &AgentName) -> CapResult<Option<FileBoundary>> {
    let path = boundary_path(child.as_str());
    match ctx.read(&path).await {
        Ok(bytes) => {
            let boundary = serde_json::from_slice(&bytes).map_err(|e| CapError::Json {
                context: format!("{}", path.display()),
                source: e,
            })?;
            Ok(Some(boundary))
        }
        Err(exo_caps::FsError::At { source, .. } | exo_caps::FsError::Io(source))
            if source.kind() == std::io::ErrorKind::NotFound =>
        {
            Ok(None)
        }
        Err(e) => Err(CapError::from(e)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exact_match() {
        assert!(matches(
            &["rust/exo/src/boundary.rs".to_string()],
            "rust/exo/src/boundary.rs"
        ));
    }

    #[test]
    fn dir_prefix_match() {
        assert!(matches(
            &["rust/exo/src".to_string()],
            "rust/exo/src/tools/merge.rs"
        ));
    }

    #[test]
    fn non_match() {
        assert!(!matches(
            &["rust/exo/src".to_string()],
            "rust/exo-caps/src/git.rs"
        ));
        assert!(!matches(&["README.md".to_string()], "README.md.bak"));
    }

    #[test]
    fn dir_prefix_false_positive_guard() {
        // "src/lib" must NOT match "src/librs" — a naive string-prefix check would wrongly
        // allow it; the '/' separator requirement is what closes this hole.
        assert!(!matches(&["src/lib".to_string()], "src/librs"));
        assert!(matches(&["src/lib".to_string()], "src/lib/mod.rs"));
        assert!(matches(&["src/lib".to_string()], "src/lib"));
    }

    #[test]
    fn boundary_path_is_scoped_to_boundaries_dir() {
        assert_eq!(
            boundary_path("some-child"),
            PathBuf::from(".exo/boundaries/some-child.json")
        );
    }
}
