//! Effect system types.
//!
//! Provides `EffectContext` for identity threading and `EffectError` for
//! structured error reporting across the service layer.

pub mod error;

pub use error::{spawn_blocking_effect, EffectError, ResultExt};

use crate::domain::{AgentName, BirthBranch};

/// Identity context for effect handlers, baked into the backend at construction.
///
/// Always present — the backend can't exist without it. No Option, no Mutex, no panic path.
#[derive(Debug, Clone)]
pub struct EffectContext {
    pub agent_name: AgentName,
    pub birth_branch: BirthBranch,
}

/// Result type for effect operations.
pub type EffectResult<T> = Result<T, EffectError>;
