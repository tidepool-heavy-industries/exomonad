//! **Standing directives** — a node's own persistent instructions to everything it spawns.
//!
//! A directive is a plain `.md` file in the node's **local, untracked** `.exo/directives/`
//! directory. `.exo/*` is covered by the repo's `.git/info/exclude`, so directives are never
//! git-tracked and never ride a merge: they are per-node local state. A human (or a parent)
//! propagates one by ordinary message; adopting it is a file write.
//!
//! Every spawn path in this crate does three things with the loaded bundle:
//!
//! 1. **Injects** it as text into the child's spec ([`Directives::apply`]) — this is what actually
//!    makes the child obey, and it works for inline children with no worktree of their own.
//! 2. **Copies** it into a worktree child's own `.exo/directives/` ([`copy_directives`]) — untracked
//!    files do not materialize through `git worktree add`, so without the copy a mid-tree TL would
//!    inherit its parent's directives in its *prompt* but have nothing to pass further down.
//! 3. **Records** its content hash ([`Directives::hash`]) onto the child's `Spawned` ledger row via
//!    `ExoSpawn::directives_hash`, so "which directives was this node born under" is answerable
//!    after the fact.

use exo_caps::{AgentName, CapError, CapResult, Fs, FsError};

/// The directory a node's standing directives live in, relative to its worktree root.
pub const DIRECTIVES_DIR: &str = ".exo/directives";

/// The header [`Directives::apply`] opens its appended section with.
pub const DIRECTIVES_HEADER: &str = "INHERITED DIRECTIVES (apply throughout this task):";

/// A node's loaded standing directives: `(filename, content)` pairs, **sorted by filename**.
///
/// Sorted at construction so both the injected text and the hash are deterministic regardless of
/// the order the filesystem happened to hand back.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Directives {
    /// `(filename, content)`, sorted by filename.
    pub files: Vec<(String, String)>,
}

impl Directives {
    /// True when this node has no standing directives — the common case. Every consumer is a
    /// no-op in that case: no text appended, no hash recorded, no files copied.
    pub fn is_empty(&self) -> bool {
        self.files.is_empty()
    }

    /// Content hash of the bundle: lowercase sha256 hex over the sorted `(name, content)` pairs,
    /// each field NUL-terminated — `update(name); update(b"\0"); update(content); update(b"\0")`
    /// per file, in `files` order. The NUL terminators keep the digest unambiguous (no pair of
    /// distinct bundles can concatenate to the same byte stream).
    ///
    /// `None` when [`is_empty`](Directives::is_empty) — "no directives" is the absence of a hash,
    /// not the hash of nothing, so a ledger row reads honestly.
    pub fn hash(&self) -> Option<String> {
        if self.is_empty() {
            return None;
        }
        use sha2::{Digest, Sha256};
        let mut h = Sha256::new();
        for (name, content) in &self.files {
            h.update(name.as_bytes());
            h.update(b"\0");
            h.update(content.as_bytes());
            h.update(b"\0");
        }
        let digest = h.finalize();
        Some(digest.iter().fold(String::new(), |mut acc, b| {
            acc.push_str(&format!("{:02x}", b));
            acc
        }))
    }

    /// Append the directives to a child's rendered spec as a trailing section:
    /// `\n\n{DIRECTIVES_HEADER}\n` then each filename as a plain label and its content. Directive
    /// files often carry their own headings, so the renderer adds no redundant heading layer.
    ///
    /// No-op (returns `task` unchanged) when [`is_empty`](Directives::is_empty).
    pub fn apply(&self, task: String) -> String {
        if self.is_empty() {
            return task;
        }
        let mut out = task;
        out.push_str("\n\n");
        out.push_str(DIRECTIVES_HEADER);
        for (name, content) in &self.files {
            out.push_str(&format!("\n\n{name}:\n{content}"));
        }
        out
    }
}

/// Load this node's standing directives from [`DIRECTIVES_DIR`].
///
/// Only regular `.md` entries are read; anything else in the directory is ignored. The result is
/// sorted by filename.
///
/// A **missing** directory is `Ok(empty)` — most nodes have no directives and that is not an error.
/// Any **other** failure (unreadable directory, unreadable file, non-UTF-8 content) is a hard
/// `Err`: a node that HAS directives must never silently spawn children without them, which is
/// exactly what a swallowed error would do.
pub async fn load_directives<C: Fs>(ctx: &C) -> CapResult<Directives> {
    let entries = match ctx.read_dir(std::path::Path::new(DIRECTIVES_DIR)).await {
        Ok(entries) => entries,
        Err(e) if is_not_found(&e) => return Ok(Directives::default()),
        Err(e) => return Err(e.into()),
    };

    let mut files = Vec::new();
    for p in entries
        .into_iter()
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("md"))
    {
        let name = p
            .file_name()
            .and_then(|n| n.to_str())
            .map(|n| n.to_string())
            .ok_or_else(|| {
                CapError::invalid(
                    "directives",
                    format!("{} has no usable file name", p.display()),
                )
            })?;
        let bytes = ctx.read(&p).await?;
        let content = String::from_utf8(bytes)
            .map_err(|_| CapError::invalid("directives", format!("{name} is not valid UTF-8")))?;
        files.push((name, content));
    }
    files.sort_by(|a, b| a.0.cmp(&b.0));

    Ok(Directives { files })
}

fn is_not_found(e: &FsError) -> bool {
    match e {
        FsError::At { source, .. } => source.kind() == std::io::ErrorKind::NotFound,
        FsError::Io(source) => source.kind() == std::io::ErrorKind::NotFound,
    }
}

/// Copy the bundle into a spawned **worktree** child's own `.exo/directives/`, so it can pass the
/// directives further down its own subtree (`git worktree add` does not carry untracked files).
///
/// Best-effort by design: a failure is logged at `warn` and swallowed. The text injection has
/// already happened by the time this runs, so the child obeys its directives either way — only its
/// ability to re-propagate them downward is lost. No-op when the bundle is empty.
pub async fn copy_directives<C: Fs>(ctx: &C, child: &AgentName, d: &Directives) {
    if d.is_empty() {
        return;
    }
    for (name, content) in &d.files {
        let path = std::path::PathBuf::from(format!(
            ".exo/worktrees/{}/.exo/directives/{}",
            child.as_str(),
            name
        ));
        if let Err(e) = ctx.write_atomic(&path, content.as_bytes()).await {
            tracing::warn!(
                "failed to copy directive {} to {}: {e}",
                name,
                child.as_str()
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{Call, MockRuntime};
    use exo_caps::AgentName;
    use std::path::PathBuf;

    #[tokio::test]
    async fn load_sorts_and_ignores_non_md() {
        let mock = MockRuntime::default();
        mock.dirs.lock().unwrap().insert(
            DIRECTIVES_DIR.to_string(),
            vec![
                PathBuf::from(".exo/directives/b.md"),
                PathBuf::from(".exo/directives/a.md"),
                PathBuf::from(".exo/directives/notes.txt"),
            ],
        );
        mock.files
            .lock()
            .unwrap()
            .insert(".exo/directives/a.md".to_string(), b"alpha".to_vec());
        mock.files
            .lock()
            .unwrap()
            .insert(".exo/directives/b.md".to_string(), b"beta".to_vec());

        let d = load_directives(&mock).await.unwrap();
        assert_eq!(
            d.files,
            vec![
                ("a.md".to_string(), "alpha".to_string()),
                ("b.md".to_string(), "beta".to_string()),
            ]
        );
    }

    #[tokio::test]
    async fn hash_is_order_independent_and_content_sensitive() {
        let mock1 = MockRuntime::default();
        mock1.dirs.lock().unwrap().insert(
            DIRECTIVES_DIR.to_string(),
            vec![
                PathBuf::from(".exo/directives/b.md"),
                PathBuf::from(".exo/directives/a.md"),
            ],
        );
        mock1
            .files
            .lock()
            .unwrap()
            .insert(".exo/directives/a.md".to_string(), b"alpha".to_vec());
        mock1
            .files
            .lock()
            .unwrap()
            .insert(".exo/directives/b.md".to_string(), b"beta".to_vec());

        let mock2 = MockRuntime::default();
        mock2.dirs.lock().unwrap().insert(
            DIRECTIVES_DIR.to_string(),
            vec![
                PathBuf::from(".exo/directives/a.md"),
                PathBuf::from(".exo/directives/b.md"),
            ],
        );
        mock2
            .files
            .lock()
            .unwrap()
            .insert(".exo/directives/a.md".to_string(), b"alpha".to_vec());
        mock2
            .files
            .lock()
            .unwrap()
            .insert(".exo/directives/b.md".to_string(), b"beta".to_vec());

        let d1 = load_directives(&mock1).await.unwrap();
        let d2 = load_directives(&mock2).await.unwrap();
        assert_eq!(d1.hash(), d2.hash());

        let mock3 = MockRuntime::default();
        mock3.dirs.lock().unwrap().insert(
            DIRECTIVES_DIR.to_string(),
            vec![PathBuf::from(".exo/directives/a.md")],
        );
        mock3
            .files
            .lock()
            .unwrap()
            .insert(".exo/directives/a.md".to_string(), b"different".to_vec());
        let d3 = load_directives(&mock3).await.unwrap();
        assert_ne!(d1.hash(), d3.hash());
    }

    #[tokio::test]
    async fn missing_directory_is_empty_no_hash() {
        let mock = MockRuntime::default();
        let d = load_directives(&mock).await.unwrap();
        assert!(d.is_empty());
        assert_eq!(d.hash(), None);
    }

    #[tokio::test]
    async fn missing_file_is_loud_error() {
        let mock = MockRuntime::default();
        mock.dirs.lock().unwrap().insert(
            DIRECTIVES_DIR.to_string(),
            vec![PathBuf::from(".exo/directives/a.md")],
        );
        // Deliberately no entry in mock.files for a.md.
        assert!(load_directives(&mock).await.is_err());
    }

    #[tokio::test]
    async fn invalid_utf8_is_loud_error() {
        let mock = MockRuntime::default();
        mock.dirs.lock().unwrap().insert(
            DIRECTIVES_DIR.to_string(),
            vec![PathBuf::from(".exo/directives/a.md")],
        );
        mock.files
            .lock()
            .unwrap()
            .insert(".exo/directives/a.md".to_string(), vec![0xff, 0xfe]);
        assert!(load_directives(&mock).await.is_err());
    }

    #[test]
    fn apply_empty_is_noop() {
        let d = Directives::default();
        let task = "do the thing".to_string();
        assert_eq!(d.apply(task.clone()), task);
    }

    #[test]
    fn apply_nonempty_appends_section() {
        let d = Directives {
            files: vec![("a.md".to_string(), "alpha content".to_string())],
        };
        let task = "do the thing".to_string();
        let out = d.apply(task.clone());
        assert!(out.starts_with(&task));
        assert!(out.contains(DIRECTIVES_HEADER));
        assert!(out.contains("\na.md:\n"));
        assert!(!out.contains("## a.md"));
        assert!(out.contains("alpha content"));
    }

    #[tokio::test]
    async fn copy_writes_one_file_per_directive() {
        let mock = MockRuntime::default();
        let child = AgentName::new("kid".into()).unwrap();
        let d = Directives {
            files: vec![("a.md".to_string(), "alpha".to_string())],
        };
        copy_directives(&mock, &child, &d).await;
        assert_eq!(
            mock.calls_made(),
            vec![Call::FsWrite {
                path: ".exo/worktrees/kid/.exo/directives/a.md".to_string(),
            }]
        );
    }

    #[tokio::test]
    async fn copy_empty_writes_nothing() {
        let mock = MockRuntime::default();
        let child = AgentName::new("kid".into()).unwrap();
        let d = Directives::default();
        copy_directives(&mock, &child, &d).await;
        assert!(mock.calls_made().is_empty());
    }
}
