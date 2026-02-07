//! Protocol buffer generated types for ExoMonad FFI boundary.
//!
//! This crate provides the single source of truth for types that cross
//! the Rust/Haskell WASM boundary.
//!
//! ## Proto files
//!
//! ### Core types (exomonad/)
//! Core FFI types use JSON wire format (serde derives).
//! - `ffi.proto` - Core FFI types (ErrorCode, FfiError, FfiResult)
//! - `common.proto` - Shared primitives (SessionId, Role, etc.)
//! - `hook.proto` - Claude Code hook types
//! - `agent.proto` - Agent management types
//! - `popup.proto` - UI popup types
//!
//! ### Effect types (effects/)
//! Effect types use protobuf binary encoding (prost Message).
//! - `envelope.proto` - EffectEnvelope + EffectResponse wire types
//! - `effect_error.proto` - Unified effect error type
//! - `git.proto` - Git operations
//! - `github.proto` - GitHub API
//! - `fs.proto` - Filesystem operations
//! - `agent.proto` - Agent control
//! - `log.proto` - Logging and events

pub mod ffi {
    include!(concat!(env!("OUT_DIR"), "/exomonad.ffi.rs"));
}

// Conditionally include modules as they become available
#[cfg(feature = "full")]
pub mod common {
    include!(concat!(env!("OUT_DIR"), "/exomonad.common.rs"));
}

#[cfg(feature = "full")]
pub mod hook {
    include!(concat!(env!("OUT_DIR"), "/exomonad.hook.rs"));
}

#[cfg(feature = "full")]
pub mod agent {
    include!(concat!(env!("OUT_DIR"), "/exomonad.agent.rs"));
}

#[cfg(feature = "full")]
pub mod popup {
    include!(concat!(env!("OUT_DIR"), "/exomonad.popup.rs"));
}

// ============================================================================
// Effect types (feature = "effects")
// ============================================================================

/// Effect types module containing all generated effect message types.
///
/// Effect types use protobuf binary encoding (not JSON).
/// Encode/decode via `prost::Message` trait.
#[cfg(feature = "effects")]
pub mod effects {
    /// Effect error and envelope types.
    ///
    /// Contains both error types (EffectError, NotFound, etc.) and
    /// envelope types (EffectEnvelope, EffectResponse) since they
    /// share the `exomonad.effects` package.
    pub mod error {
        include!(concat!(env!("OUT_DIR"), "/exomonad.effects.rs"));
    }

    /// Git effect types.
    pub mod git {
        include!(concat!(env!("OUT_DIR"), "/exomonad.effects.git.rs"));
    }

    /// GitHub effect types.
    pub mod github {
        include!(concat!(env!("OUT_DIR"), "/exomonad.effects.github.rs"));
    }

    /// Filesystem effect types.
    pub mod fs {
        include!(concat!(env!("OUT_DIR"), "/exomonad.effects.fs.rs"));
    }

    /// Agent control effect types.
    pub mod agent {
        include!(concat!(env!("OUT_DIR"), "/exomonad.effects.agent.rs"));
    }

    /// Logging effect types.
    pub mod log {
        include!(concat!(env!("OUT_DIR"), "/exomonad.effects.log.rs"));
    }

    // Re-export common effect types
    pub use error::{Custom, EffectError, EffectEnvelope, EffectResponse, InvalidInput, NetworkError, NotFound, PermissionDenied, Timeout};
}

// Re-export common types at crate root for convenience
pub use ffi::{ErrorCode, ErrorContext, FfiError, FfiResult};
