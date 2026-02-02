//! Domain types with parse-at-edge validation.
//!
//! This module provides newtype wrappers for string domain concepts
//! with validation at construction time. All parsing happens at the
//! boundary (deserialization, construction) to ensure invalid values
//! never enter the system.

use serde::{Deserialize, Serialize};
use std::fmt;

// ============================================================================
// Error Types
// ============================================================================

/// Domain validation errors.
#[derive(Debug, thiserror::Error)]
pub enum DomainError {
    /// Empty field value.
    #[error("empty {field}")]
    Empty { field: &'static str },

    /// Invalid field value.
    #[error("invalid {field}: {value}")]
    Invalid {
        field: &'static str,
        value: String,
    },

    /// Parse error for numeric types.
    #[error("parse error for {field}: {value}")]
    ParseError {
        field: &'static str,
        value: String,
    },
}

// ============================================================================
// Session Identifiers
// ============================================================================

/// Session identifier (non-empty string).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct SessionId(String);

impl TryFrom<String> for SessionId {
    type Error = DomainError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        if s.is_empty() {
            return Err(DomainError::Empty {
                field: "session_id",
            });
        }
        Ok(Self(s))
    }
}

impl From<SessionId> for String {
    fn from(id: SessionId) -> String {
        id.0
    }
}

impl SessionId {
    /// Get the session ID as a string slice.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Create from &str (unchecked, for tests).
    #[cfg(test)]
    pub fn from_str_unchecked(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl From<&str> for SessionId {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl fmt::Display for SessionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// ============================================================================
// Tool Names
// ============================================================================

/// Tool name identifier (non-empty string).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct ToolName(String);

impl TryFrom<String> for ToolName {
    type Error = DomainError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        if s.is_empty() {
            return Err(DomainError::Empty { field: "tool_name" });
        }
        Ok(Self(s))
    }
}

impl From<ToolName> for String {
    fn from(name: ToolName) -> String {
        name.0
    }
}

impl ToolName {
    /// Get the tool name as a string slice.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<&str> for ToolName {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl fmt::Display for ToolName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// ============================================================================
// GitHub Identifiers
// ============================================================================

/// GitHub repository owner (non-empty string).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct GithubOwner(String);

impl TryFrom<String> for GithubOwner {
    type Error = DomainError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        if s.is_empty() {
            return Err(DomainError::Empty {
                field: "github_owner",
            });
        }
        Ok(Self(s))
    }
}

impl From<GithubOwner> for String {
    fn from(owner: GithubOwner) -> String {
        owner.0
    }
}

impl GithubOwner {
    /// Get the owner name as a string slice.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<&str> for GithubOwner {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl fmt::Display for GithubOwner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// GitHub repository name (non-empty string).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct GithubRepo(String);

impl TryFrom<String> for GithubRepo {
    type Error = DomainError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        if s.is_empty() {
            return Err(DomainError::Empty {
                field: "github_repo",
            });
        }
        Ok(Self(s))
    }
}

impl From<GithubRepo> for String {
    fn from(repo: GithubRepo) -> String {
        repo.0
    }
}

impl GithubRepo {
    /// Get the repo name as a string slice.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<&str> for GithubRepo {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl fmt::Display for GithubRepo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// GitHub issue number (positive integer).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "u64", into = "u64")]
pub struct IssueNumber(u64);

impl TryFrom<u64> for IssueNumber {
    type Error = DomainError;

    fn try_from(n: u64) -> Result<Self, Self::Error> {
        if n == 0 {
            return Err(DomainError::Invalid {
                field: "issue_number",
                value: "0".to_string(),
            });
        }
        Ok(Self(n))
    }
}

impl TryFrom<String> for IssueNumber {
    type Error = DomainError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        let n = s.parse::<u64>().map_err(|_| DomainError::ParseError {
            field: "issue_number",
            value: s,
        })?;
        Self::try_from(n)
    }
}

impl From<IssueNumber> for u64 {
    fn from(num: IssueNumber) -> u64 {
        num.0
    }
}

impl IssueNumber {
    /// Get the issue number as a u64.
    pub fn as_u64(&self) -> u64 {
        self.0
    }
}

impl fmt::Display for IssueNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// ============================================================================
// Tool Permission
// ============================================================================

/// Tool execution permission for PreToolUse hooks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ToolPermission {
    /// Allow the tool to execute.
    Allow,
    /// Deny tool execution.
    Deny,
    /// Ask the user for permission.
    Ask,
}

impl fmt::Display for ToolPermission {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Allow => write!(f, "allow"),
            Self::Deny => write!(f, "deny"),
            Self::Ask => write!(f, "ask"),
        }
    }
}

impl TryFrom<String> for ToolPermission {
    type Error = DomainError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        match s.to_lowercase().as_str() {
            "allow" => Ok(Self::Allow),
            "deny" => Ok(Self::Deny),
            "ask" => Ok(Self::Ask),
            _ => Err(DomainError::Invalid {
                field: "tool_permission",
                value: s,
            }),
        }
    }
}

// ============================================================================
// Role
// ============================================================================

/// Agent role (dev, tl, pm).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Role {
    /// Developer role.
    Dev,
    /// Tech lead role.
    TL,
    /// Product manager role.
    PM,
}

impl Default for Role {
    fn default() -> Self {
        Self::Dev
    }
}

impl fmt::Display for Role {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Dev => write!(f, "dev"),
            Self::TL => write!(f, "tl"),
            Self::PM => write!(f, "pm"),
        }
    }
}

impl TryFrom<String> for Role {
    type Error = DomainError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        match s.to_lowercase().as_str() {
            "dev" => Ok(Self::Dev),
            "tl" => Ok(Self::TL),
            "pm" => Ok(Self::PM),
            _ => Err(DomainError::Invalid {
                field: "role",
                value: s,
            }),
        }
    }
}

// ============================================================================
// Path Types
// ============================================================================

use std::path::{Path, PathBuf};

/// Path validation errors.
#[derive(Debug, thiserror::Error)]
pub enum PathError {
    #[error("path must be absolute: {path}")]
    NotAbsolute { path: PathBuf },

    #[error("path does not exist: {path}")]
    NotFound { path: PathBuf },

    #[error("path is not a file: {path}")]
    NotFile { path: PathBuf },

    #[error("path does not have .wasm extension: {path}")]
    InvalidWasmExtension { path: PathBuf },
}

/// Absolute path (must be absolute).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AbsolutePath(PathBuf);

impl TryFrom<PathBuf> for AbsolutePath {
    type Error = PathError;

    fn try_from(p: PathBuf) -> Result<Self, Self::Error> {
        if !p.is_absolute() {
            return Err(PathError::NotAbsolute { path: p });
        }
        Ok(Self(p))
    }
}

impl From<AbsolutePath> for PathBuf {
    fn from(p: AbsolutePath) -> PathBuf {
        p.0
    }
}

impl AbsolutePath {
    /// Get the path as a Path reference.
    pub fn as_path(&self) -> &Path {
        &self.0
    }

    /// Convert to PathBuf (consumes self).
    pub fn into_path_buf(self) -> PathBuf {
        self.0
    }
}

impl AsRef<Path> for AbsolutePath {
    fn as_ref(&self) -> &Path {
        &self.0
    }
}

impl fmt::Display for AbsolutePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.display())
    }
}

/// WASM path (must exist, be a file, and have .wasm extension).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WasmPath(PathBuf);

impl TryFrom<PathBuf> for WasmPath {
    type Error = PathError;

    fn try_from(p: PathBuf) -> Result<Self, Self::Error> {
        if !p.exists() {
            return Err(PathError::NotFound { path: p });
        }
        if !p.is_file() {
            return Err(PathError::NotFile { path: p });
        }
        if p.extension().and_then(|e| e.to_str()) != Some("wasm") {
            return Err(PathError::InvalidWasmExtension { path: p });
        }
        Ok(Self(p))
    }
}

impl From<WasmPath> for PathBuf {
    fn from(p: WasmPath) -> PathBuf {
        p.0
    }
}

impl WasmPath {
    /// Get the path as a Path reference.
    pub fn as_path(&self) -> &Path {
        &self.0
    }

    /// Convert to PathBuf (consumes self).
    pub fn into_path_buf(self) -> PathBuf {
        self.0
    }
}

impl AsRef<Path> for WasmPath {
    fn as_ref(&self) -> &Path {
        &self.0
    }
}

impl fmt::Display for WasmPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.display())
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_session_id_validation() {
        // Valid
        let id = SessionId::try_from("session-123".to_string()).unwrap();
        assert_eq!(id.as_str(), "session-123");

        // Empty
        let result = SessionId::try_from("".to_string());
        assert!(matches!(result, Err(DomainError::Empty { .. })));
    }

    #[test]
    fn test_tool_name_validation() {
        // Valid
        let name = ToolName::try_from("Write".to_string()).unwrap();
        assert_eq!(name.as_str(), "Write");

        // Empty
        let result = ToolName::try_from("".to_string());
        assert!(matches!(result, Err(DomainError::Empty { .. })));
    }

    #[test]
    fn test_github_identifiers() {
        // Valid owner
        let owner = GithubOwner::try_from("anthropics".to_string()).unwrap();
        assert_eq!(owner.as_str(), "anthropics");

        // Valid repo
        let repo = GithubRepo::try_from("claude-code".to_string()).unwrap();
        assert_eq!(repo.as_str(), "claude-code");

        // Empty owner
        let result = GithubOwner::try_from("".to_string());
        assert!(matches!(result, Err(DomainError::Empty { .. })));

        // Empty repo
        let result = GithubRepo::try_from("".to_string());
        assert!(matches!(result, Err(DomainError::Empty { .. })));
    }

    #[test]
    fn test_issue_number_validation() {
        // Valid
        let num = IssueNumber::try_from(123u64).unwrap();
        assert_eq!(num.as_u64(), 123);

        // Zero
        let result = IssueNumber::try_from(0u64);
        assert!(matches!(result, Err(DomainError::Invalid { .. })));

        // From string
        let num = IssueNumber::try_from("456".to_string()).unwrap();
        assert_eq!(num.as_u64(), 456);

        // Invalid string
        let result = IssueNumber::try_from("not-a-number".to_string());
        assert!(matches!(result, Err(DomainError::ParseError { .. })));
    }

    #[test]
    fn test_tool_permission() {
        assert_eq!(
            ToolPermission::try_from("allow".to_string()).unwrap(),
            ToolPermission::Allow
        );
        assert_eq!(
            ToolPermission::try_from("deny".to_string()).unwrap(),
            ToolPermission::Deny
        );
        assert_eq!(
            ToolPermission::try_from("ask".to_string()).unwrap(),
            ToolPermission::Ask
        );

        // Case insensitive
        assert_eq!(
            ToolPermission::try_from("ALLOW".to_string()).unwrap(),
            ToolPermission::Allow
        );

        // Invalid
        let result = ToolPermission::try_from("invalid".to_string());
        assert!(matches!(result, Err(DomainError::Invalid { .. })));
    }

    #[test]
    fn test_role() {
        assert_eq!(Role::try_from("dev".to_string()).unwrap(), Role::Dev);
        assert_eq!(Role::try_from("tl".to_string()).unwrap(), Role::TL);
        assert_eq!(Role::try_from("pm".to_string()).unwrap(), Role::PM);

        // Case insensitive
        assert_eq!(Role::try_from("DEV".to_string()).unwrap(), Role::Dev);

        // Invalid
        let result = Role::try_from("invalid".to_string());
        assert!(matches!(result, Err(DomainError::Invalid { .. })));

        // Default
        assert_eq!(Role::default(), Role::Dev);
    }

    #[test]
    fn test_serde_roundtrip() {
        // SessionId
        let id = SessionId::try_from("test-session".to_string()).unwrap();
        let json = serde_json::to_string(&id).unwrap();
        let deserialized: SessionId = serde_json::from_str(&json).unwrap();
        assert_eq!(id, deserialized);

        // IssueNumber
        let num = IssueNumber::try_from(42u64).unwrap();
        let json = serde_json::to_string(&num).unwrap();
        let deserialized: IssueNumber = serde_json::from_str(&json).unwrap();
        assert_eq!(num, deserialized);

        // ToolPermission
        let permission = ToolPermission::Allow;
        let json = serde_json::to_string(&permission).unwrap();
        let deserialized: ToolPermission = serde_json::from_str(&json).unwrap();
        assert_eq!(permission, deserialized);
    }

    #[test]
    fn test_absolute_path() {
        // Valid absolute path
        let abs = AbsolutePath::try_from(PathBuf::from("/tmp/test")).unwrap();
        assert_eq!(abs.as_path(), Path::new("/tmp/test"));

        // Relative path should fail
        let result = AbsolutePath::try_from(PathBuf::from("relative/path"));
        assert!(matches!(result, Err(PathError::NotAbsolute { .. })));

        // Test as_ref
        let _: &Path = abs.as_ref();
    }

    #[test]
    fn test_wasm_path() {
        use std::fs;
        use tempfile::tempdir;

        let dir = tempdir().unwrap();

        // Create a valid .wasm file
        let wasm_file = dir.path().join("test.wasm");
        fs::write(&wasm_file, b"fake wasm").unwrap();

        let wasm = WasmPath::try_from(wasm_file.clone()).unwrap();
        assert_eq!(wasm.as_path(), wasm_file.as_path());

        // Non-existent file
        let missing = dir.path().join("missing.wasm");
        let result = WasmPath::try_from(missing);
        assert!(matches!(result, Err(PathError::NotFound { .. })));

        // Directory instead of file
        let result = WasmPath::try_from(dir.path().to_path_buf());
        assert!(matches!(result, Err(PathError::NotFile { .. })));

        // Wrong extension
        let wrong_ext = dir.path().join("test.txt");
        fs::write(&wrong_ext, b"not wasm").unwrap();
        let result = WasmPath::try_from(wrong_ext);
        assert!(matches!(result, Err(PathError::InvalidWasmExtension { .. })));
    }
}
