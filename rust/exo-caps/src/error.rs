//! The capability error type — held to the `exo-scry::ScryError` bar: distinct
//! inspectable variants, source-preserving (`#[from]` the per-cap errors, never
//! flattening to a `String`), no stringly-typed soup.

use thiserror::Error;

#[derive(Debug, Error)]
pub enum CapError {
    /// A domain newtype's constructor rejected bad input.
    #[error("invalid {what}: {detail}")]
    Invalid { what: &'static str, detail: String },

    // Per-cap errors flow in transparently, preserving `source()` chaining. A tool
    // generic over several caps unifies them with `?` via these `#[from]` impls.
    #[error(transparent)]
    Git(#[from] crate::git::GitError),
    #[error(transparent)]
    GitHub(#[from] crate::github::GitHubError),
    #[error(transparent)]
    Tmux(#[from] crate::tmux::TmuxError),
    #[error(transparent)]
    Bus(#[from] crate::bus::BusError),
    #[error(transparent)]
    Spawn(#[from] crate::spawner::SpawnError),
    #[error(transparent)]
    Fs(#[from] crate::fs::FsError),
    #[error(transparent)]
    Process(#[from] crate::process::ProcessError),
    #[error(transparent)]
    Kv(#[from] crate::kv::KvError),

    #[error("io: {0}")]
    Io(#[from] std::io::Error),
    #[error("json ({context}): {source}")]
    Json {
        context: String,
        #[source]
        source: serde_json::Error,
    },
}

impl CapError {
    /// Construct an `Invalid` — a domain newtype's constructor rejecting bad input.
    pub fn invalid(what: &'static str, detail: impl Into<String>) -> Self {
        CapError::Invalid {
            what,
            detail: detail.into(),
        }
    }
}

pub type CapResult<T> = Result<T, CapError>;
