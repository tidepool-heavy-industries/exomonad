//! Logging utilities for mantle.
//!
//! Provides a standardized way to initialize tracing across all mantle binaries.

use tracing_subscriber::EnvFilter;

/// Initialize tracing with standard defaults for mantle CLI tools.
///
/// - Uses `RUST_LOG` environment variable for filtering.
/// - Outputs to `stderr` to avoid corrupting `stdout` (useful for JSON output).
/// - Disables target display for cleaner CLI output.
pub fn init_logging() {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_target(false)
        .with_writer(std::io::stderr)
        .init();
}

/// Initialize tracing with a default filter if `RUST_LOG` is not set.
///
/// Same as [`init_logging`], but allows specifying a default filter string.
pub fn init_logging_with_default(default_filter: &str) {
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| EnvFilter::new(default_filter)),
        )
        .with_target(false)
        .with_writer(std::io::stderr)
        .init();
}
