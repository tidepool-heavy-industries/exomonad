//! Node sidecar errors — boot/self-ID failures + the assembly placeholder.

use thiserror::Error;

pub type NodeResult<T> = Result<T, NodeError>;

#[derive(Debug, Error)]
pub enum NodeError {
    /// `--papers <path>` was given but the file couldn't be read or parsed.
    #[error("failed to load papers from {path}: {detail}")]
    Papers { path: String, detail: String },

    /// A required ambient value (e.g. `$TMUX_PANE`, run-id) was absent at boot.
    #[error("missing required boot context: {0}")]
    MissingContext(&'static str),

    /// `exo-scry` self-ID failed.
    #[error("identity resolution failed: {0}")]
    Scry(String),

    /// The converge wiring isn't in place yet (Wave-2 scaffold placeholder).
    #[error("node loops not yet assembled")]
    NotAssembled,

    #[error(transparent)]
    Io(#[from] std::io::Error),
}
