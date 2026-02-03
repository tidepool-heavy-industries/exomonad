use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::fmt::Debug;

/// Trait that all FFI boundary types must implement.
///
/// This ensures consistent serialization, validation, and error handling
/// across the WASM boundary.
pub trait FFIBoundary: Serialize + DeserializeOwned + Send + Sync + 'static {
    /// Validate the data after deserialization.
    ///
    /// This allows for "Parse, don't validate" at the boundary layer, but
    /// still provides a standard way to enforce domain invariants.
    fn validate(&self) -> Result<(), FFIError> {
        Ok(())
    }

    /// Schema version for compatibility checking.
    fn schema_version() -> u32 {
        1
    }
}

/// Standardized error code for programmatic handling across the FFI boundary.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum ErrorCode {
    /// Resource (file, issue, branch, etc.) does not exist.
    NotFound,
    /// Missing or invalid credentials (e.g., GitHub token).
    NotAuthenticated,
    /// Git command failed (e.g., merge conflict, dirty working directory).
    GitError,
    /// File system operation failed (e.g., permission denied).
    IoError,
    /// Network request failed (e.g., API unreachable).
    NetworkError,
    /// Invalid input parameters provided to the host function.
    InvalidInput,
    /// Unexpected internal error (bug in the host function or runtime).
    InternalError,
    /// Operation timed out.
    Timeout,
    /// Resource already exists (e.g., worktree path).
    AlreadyExists,
}

impl Default for ErrorCode {
    fn default() -> Self {
        Self::InternalError
    }
}

/// Rich context for debugging errors.
#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq)]
pub struct ErrorContext {
    /// The command that failed (e.g., "git worktree add ...").
    #[serde(skip_serializing_if = "Option::is_none")]
    pub command: Option<String>,

    /// Process exit code, if applicable.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub exit_code: Option<i32>,

    /// Standard error output from the command (truncated if necessary).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stderr: Option<String>,

    /// Standard output from the command (truncated if necessary).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stdout: Option<String>,

    /// Relevant file path.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub file_path: Option<String>,

    /// Working directory where the operation was attempted.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub working_dir: Option<String>,
}

/// Structured error returned to the WASM guest.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FFIError {
    /// Human-readable summary of the error.
    pub message: String,

    /// Programmatic error code.
    pub code: ErrorCode,

    /// Rich context for debugging.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context: Option<ErrorContext>,

    /// Actionable suggestion for recovery.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub suggestion: Option<String>,
}

impl std::fmt::Display for FFIError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{:?}] {}", self.code, self.message)
    }
}

impl std::error::Error for FFIError {}

/// Standardized result envelope for all host functions.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(tag = "kind", content = "payload")]
pub enum FFIResult<T> {
    Success(T),
    Error(FFIError),
}

impl<T> FFIResult<T> {
    /// Create a success result.
    pub fn success(value: T) -> Self {
        Self::Success(value)
    }

    /// Create an error result with full details.
    pub fn error(
        message: impl Into<String>,
        code: ErrorCode,
        context: Option<ErrorContext>,
        suggestion: Option<String>,
    ) -> Self {
        Self::Error(FFIError {
            message: message.into(),
            code,
            context,
            suggestion,
        })
    }

    /// Create a simple error result.
    pub fn simple_error(message: impl Into<String>, code: ErrorCode) -> Self {
        Self::Error(FFIError {
            message: message.into(),
            code,
            context: None,
            suggestion: None,
        })
    }
}

impl<T: FFIBoundary> FFIBoundary for FFIResult<T> {}
impl FFIBoundary for FFIError {}
impl FFIBoundary for String {}
impl FFIBoundary for bool {}
impl<T: FFIBoundary> FFIBoundary for Vec<T> {}
impl<T: FFIBoundary> FFIBoundary for Option<T> {}

// Primitive FFIBoundary impls to match Haskell-side instances and
// allow simple types like () / Int / Word64 across the FFI boundary.
impl FFIBoundary for () {}

// Signed integers (Haskell Int typically maps to a machine-sized int;
// we provide common widths explicitly).
impl FFIBoundary for i32 {}
impl FFIBoundary for i64 {}
impl FFIBoundary for isize {}
impl FFIBoundary for i8 {}
impl FFIBoundary for i16 {}

// Unsigned integers (Haskell Word64).
impl FFIBoundary for u64 {}
impl FFIBoundary for u32 {}
impl FFIBoundary for usize {}
impl FFIBoundary for u8 {}
impl FFIBoundary for u16 {}
