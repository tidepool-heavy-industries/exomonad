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

use exo_caps::{AgentName, CapResult, Fs};

/// The directory a node's standing directives live in, relative to its worktree root.
pub const DIRECTIVES_DIR: &str = ".exo/directives";

/// The header [`Directives::apply`] opens its appended section with.
pub const DIRECTIVES_HEADER: &str =
    "STANDING DIRECTIVES (inherited from your spawner — follow these):";

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
        todo!("sha256 hex over NUL-terminated sorted (name, content) pairs; None when empty")
    }

    /// Append the directives to a child's rendered spec as a trailing section:
    /// `\n\n{DIRECTIVES_HEADER}\n` then, per file, a `## {name}` header and the file's content.
    ///
    /// No-op (returns `task` unchanged) when [`is_empty`](Directives::is_empty).
    pub fn apply(&self, task: String) -> String {
        todo!("append the STANDING DIRECTIVES section per file; no-op when empty")
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
    todo!("read_dir(DIRECTIVES_DIR); NotFound => Ok(empty); filter .md; read each; sort by name")
}

/// Copy the bundle into a spawned **worktree** child's own `.exo/directives/`, so it can pass the
/// directives further down its own subtree (`git worktree add` does not carry untracked files).
///
/// Best-effort by design: a failure is logged at `warn` and swallowed. The text injection has
/// already happened by the time this runs, so the child obeys its directives either way — only its
/// ability to re-propagate them downward is lost. No-op when the bundle is empty.
pub async fn copy_directives<C: Fs>(ctx: &C, child: &AgentName, d: &Directives) {
    todo!("write_atomic each file under .exo/worktrees/<child>/.exo/directives/; warn on error")
}
