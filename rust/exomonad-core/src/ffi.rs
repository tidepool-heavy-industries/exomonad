//! Marker trait for types that cross service boundaries.

use serde::{de::DeserializeOwned, Serialize};

/// Marker trait for types used across service boundaries.
///
/// Ensures consistent serialization capabilities.
pub trait FFIBoundary: Serialize + DeserializeOwned + Send + Sync + 'static {}
