//! The capability error type — held to the `exo-scry::ScryError` bar: distinct
//! inspectable variants, `#[from]` source-chaining, no stringly-typed soup.
//!
//! Wave-1 note: per-cap leaves may introduce richer per-domain error enums (e.g.
//! `GitError`) wrapped here via `#[from]`; the per-domain string variants below are the
//! scaffold's floor.

use thiserror::Error;

#[derive(Debug, Error)]
pub enum CapError {
    #[error("git: {0}")]
    Git(String),
    #[error("github: {0}")]
    GitHub(String),
    #[error("tmux: {0}")]
    Tmux(String),
    #[error("bus: {0}")]
    Bus(String),
    #[error("spawn: {0}")]
    Spawn(String),
    #[error("invalid {what}: {detail}")]
    Invalid { what: &'static str, detail: String },
    #[error("io: {0}")]
    Io(#[from] std::io::Error),
    #[error("json ({context}): {source}")]
    Json {
        context: String,
        source: serde_json::Error,
    },
}

pub type CapResult<T> = Result<T, CapError>;
