//! Test instrumentation for effect dispatch.
//!
//! Provides `WithTestHooks<D>` — a wrapper around any `EffectDispatch` that emits
//! `DebugEvent`s via `tokio::sync::broadcast` and supports barriers for synchronous
//! probing at effect boundaries.

use chrono::{DateTime, Utc};
use serde::Serialize;

/// Phase of an effect dispatch (before or after execution).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Phase {
    Before,
    After,
}

/// Event emitted by test hooks around effect dispatch.
#[derive(Debug, Clone, Serialize)]
pub struct DebugEvent {
    /// Timestamp of the event.
    pub ts: DateTime<Utc>,
    /// Effect type (e.g., "git.get_branch").
    pub effect_type: String,
    /// Agent name from the effect context.
    pub agent_name: String,
    /// Whether this is a before or after event.
    pub phase: Phase,
    /// Whether the effect succeeded (None for Before, Some for After).
    pub success: Option<bool>,
    /// Duration in milliseconds (None for Before, Some for After).
    pub duration_ms: Option<u64>,
}
