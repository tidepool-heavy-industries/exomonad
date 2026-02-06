//! Protocol buffer generated types for ExoMonad FFI boundary.
//!
//! This crate provides the single source of truth for types that cross
//! the Rust/Haskell WASM boundary. Types are generated from `.proto` files
//! and include serde derives for JSON wire compatibility.
//!
//! ## Proto files
//!
//! - `ffi.proto` - Core FFI types (ErrorCode, FfiError, FfiResult)
//! - `common.proto` - Shared primitives (SessionId, Role, etc.)
//! - `hook.proto` - Claude Code hook types
//! - `agent.proto` - Agent management types
//! - `popup.proto` - UI popup types

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

// Re-export common types at crate root for convenience
pub use ffi::{ErrorCode, ErrorContext, FfiError, FfiResult};
