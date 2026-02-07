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
//! Modules are auto-generated from `proto/effects/*.proto` at build time.
//! Each `.proto` file with a `service` definition gets its own module.

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
///
/// Sub-modules are generated from `proto/effects/*.proto` — see
/// `build.rs` `generate_effect_module_declarations()`.
#[cfg(feature = "effects")]
pub mod effects {
    include!(concat!(env!("OUT_DIR"), "/effect_modules.rs"));

    // Re-export common effect types (stable framework API)
    pub use error::{Custom, EffectError, EffectEnvelope, EffectResponse, InvalidInput, NetworkError, NotFound, PermissionDenied, Timeout};
}

// Re-export common types at crate root for convenience
pub use ffi::{ErrorCode, ErrorContext, FfiError, FfiResult};
