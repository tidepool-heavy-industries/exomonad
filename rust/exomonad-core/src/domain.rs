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
    Invalid { field: &'static str, value: String },

    /// Parse error for numeric types.
    #[error("parse error for {field}: {value}")]
    ParseError { field: &'static str, value: String },
}

// ============================================================================
// Validated String Macro
// ============================================================================

/// Generate a validated non-empty string newtype with standard impls.
///
/// Provides: TryFrom<String>, From<&str> (panics on empty), From<T> for String,
/// Display, as_str(), AsRef<str>, Borrow<str>, and serde support via try_from/into.
macro_rules! validated_string {
    ($(#[doc = $doc:expr])* $name:ident, $field:expr) => {
        $(#[doc = $doc])*
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
        #[serde(try_from = "String", into = "String")]
        pub struct $name(String);

        impl TryFrom<String> for $name {
            type Error = DomainError;

            fn try_from(s: String) -> Result<Self, Self::Error> {
                if s.is_empty() {
                    return Err(DomainError::Empty { field: $field });
                }
                Ok(Self(s))
            }
        }

        impl From<$name> for String {
            fn from(val: $name) -> String {
                val.0
            }
        }

        impl From<&str> for $name {
            /// Panics on empty input. Use `TryFrom<String>` for fallible conversion.
            fn from(s: &str) -> Self {
                assert!(!s.is_empty(), concat!($field, " must not be empty"));
                Self(s.to_string())
            }
        }

        impl $name {
            /// Get the value as a string slice.
            pub fn as_str(&self) -> &str {
                &self.0
            }
        }

        impl AsRef<str> for $name {
            fn as_ref(&self) -> &str {
                &self.0
            }
        }

        impl std::borrow::Borrow<str> for $name {
            fn borrow(&self) -> &str {
                &self.0
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", self.0)
            }
        }
    };
}

// ============================================================================
// String Newtypes
// ============================================================================

validated_string!(
    #[doc = "Session identifier (non-empty string)."]
    SessionId,
    "session_id"
);
validated_string!(
    #[doc = "Tool name identifier (non-empty string)."]
    ToolName,
    "tool_name"
);
validated_string!(
    #[doc = "GitHub repository owner (non-empty string)."]
    GithubOwner,
    "github_owner"
);
validated_string!(
    #[doc = "GitHub repository name (non-empty string)."]
    GithubRepo,
    "github_repo"
);

validated_string!(
    #[doc = "Git branch name (non-empty string)."]
    #[doc = "Semantically distinct from `Revision` which can also be a commit ID prefix."]
    BranchName,
    "branch_name"
);

validated_string!(
    #[doc = "Git revision specifier (branch name or commit ID prefix)."]
    #[doc = "Semantically distinct from `BranchName` — revisions resolve to commits via multiple strategies."]
    Revision,
    "revision"
);

validated_string!(
    #[doc = "Agent's birth-branch (git branch name as identity)."]
    #[doc = "Root TL is always `main`. Subtrees use dot-separated names like `main.feature-a`."]
    BirthBranch,
    "birth_branch"
);

impl BirthBranch {
    /// Root TL birth-branch (always "main").
    pub fn root() -> Self {
        Self("main".to_string())
    }

    /// Depth in the agent tree (0 = root TL on "main", 1 = first subtree, etc.).
    /// Depth equals the number of dot separators in the branch name.
    pub fn depth(&self) -> usize {
        self.0.chars().filter(|&c| c == '.').count()
    }

    /// Git branch to use as parent when creating child worktrees.
    /// Root ("main") returns "main", subtrees return themselves.
    pub fn as_parent_branch(&self) -> &str {
        &self.0
    }

    /// Derive a child birth-branch by appending a slug.
    pub fn child(&self, slug: &str) -> Self {
        Self(format!("{}.{}", self.0, slug))
    }

    /// Get the parent's birth-branch (one level up in dot hierarchy).
    /// Returns None for root ("main") since it has no parent.
    pub fn parent(&self) -> Option<Self> {
        if self.0 == "main" {
            None
        } else if let Some((parent, _)) = self.0.rsplit_once('.') {
            Some(Self(parent.to_string()))
        } else {
            // Single segment like "main" with no dots — this IS root
            None
        }
    }
}

validated_string!(
    #[doc = "Human-readable agent name/slug (e.g., `feature-a-claude`)."]
    AgentName,
    "agent_name"
);

impl AgentName {
    /// Whether this agent name looks like a Gemini worker (ends with "-gemini").
    pub fn is_gemini_worker(&self) -> bool {
        self.0.ends_with("-gemini")
    }
}

validated_string!(
    #[doc = "Claude Code session UUID for --fork-session context inheritance."]
    ClaudeSessionUuid,
    "claude_session_uuid"
);

#[cfg(test)]
impl SessionId {
    /// Create from &str (unchecked, for tests).
    pub fn from_str_unchecked(s: &str) -> Self {
        Self(s.to_string())
    }
}

#[cfg(test)]
impl BirthBranch {
    /// Create from &str (unchecked, for tests).
    pub fn from_str_unchecked(s: &str) -> Self {
        Self(s.to_string())
    }
}

#[cfg(test)]
impl AgentName {
    /// Create from &str (unchecked, for tests).
    pub fn from_str_unchecked(s: &str) -> Self {
        Self(s.to_string())
    }
}

#[cfg(test)]
impl ClaudeSessionUuid {
    /// Create from &str (unchecked, for tests).
    pub fn from_str_unchecked(s: &str) -> Self {
        Self(s.to_string())
    }
}

// ============================================================================
// GitHub Issue Number
// ============================================================================

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
// PR Number
// ============================================================================

/// GitHub pull request number (positive integer).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "u64", into = "u64")]
pub struct PRNumber(u64);

impl TryFrom<u64> for PRNumber {
    type Error = DomainError;

    fn try_from(n: u64) -> Result<Self, Self::Error> {
        if n == 0 {
            return Err(DomainError::Invalid {
                field: "pr_number",
                value: "0".to_string(),
            });
        }
        Ok(Self(n))
    }
}

impl From<PRNumber> for u64 {
    fn from(num: PRNumber) -> u64 {
        num.0
    }
}

impl PRNumber {
    /// Create a PRNumber from a u64 (panics on 0, use TryFrom for fallible).
    pub fn new(n: u64) -> Self {
        assert!(n > 0, "PR number must be positive");
        Self(n)
    }

    /// Get the PR number as a u64.
    pub fn as_u64(&self) -> u64 {
        self.0
    }
}

impl fmt::Display for PRNumber {
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

// ============================================================================
// Permission Mode
// ============================================================================

/// Claude Code permission mode.
///
/// Unknown values from future Claude Code versions deserialize to `Default`,
/// preserving wire compatibility.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Default)]
#[serde(rename_all = "camelCase")]
pub enum PermissionMode {
    /// Default permission behavior (also used as fallback for unknown modes).
    #[default]
    Default,
    /// Plan mode (higher gating).
    Plan,
    /// Accept edits without confirmation.
    AcceptEdits,
    /// Don't ask for permission.
    DontAsk,
    /// Bypass all permissions.
    BypassPermissions,
}

impl<'de> serde::Deserialize<'de> for PermissionMode {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Ok(match s.as_str() {
            "default" => Self::Default,
            "plan" => Self::Plan,
            "acceptEdits" => Self::AcceptEdits,
            "dontAsk" => Self::DontAsk,
            "bypassPermissions" => Self::BypassPermissions,
            _ => Self::Default,
        })
    }
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
// Filter State
// ============================================================================

/// State filter for GitHub issues and PRs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum FilterState {
    /// Open items only.
    Open,
    /// Closed items only.
    Closed,
    /// All items (open and closed).
    All,
}

// ============================================================================
// Role
// ============================================================================

/// Agent role (dev, tl, worker).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Role {
    /// Developer role.
    #[default]
    Dev,
    /// Tech lead role.
    TL,
    /// Worker role (ephemeral, notify_parent only).
    Worker,
}

impl fmt::Display for Role {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Dev => write!(f, "dev"),
            Self::TL => write!(f, "tl"),
            Self::Worker => write!(f, "worker"),
        }
    }
}

impl TryFrom<String> for Role {
    type Error = DomainError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        match s.to_lowercase().as_str() {
            "dev" => Ok(Self::Dev),
            "tl" => Ok(Self::TL),
            "worker" => Ok(Self::Worker),
            _ => Err(DomainError::Invalid {
                field: "role",
                value: s,
            }),
        }
    }
}

// ============================================================================
// GitHub States
// ============================================================================

/// State of an item (Issue/PR) - Open, Closed, or Unknown.
///
/// Deserializes case-insensitively to handle both lowercase API responses
/// and SCREAMING_SNAKE_CASE from GitHub's GraphQL API.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ItemState {
    /// Item is currently open.
    Open,
    /// Item is closed or merged.
    Closed,
    /// State could not be determined.
    Unknown,
}

impl fmt::Display for ItemState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Open => write!(f, "open"),
            Self::Closed => write!(f, "closed"),
            Self::Unknown => write!(f, "unknown"),
        }
    }
}

impl<'de> Deserialize<'de> for ItemState {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        match s.to_lowercase().as_str() {
            "open" => Ok(Self::Open),
            "closed" => Ok(Self::Closed),
            _ => Ok(Self::Unknown),
        }
    }
}

/// State of a GitHub Review.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum ReviewState {
    /// Review is pending.
    Pending,
    /// Review is approved.
    Approved,
    /// Changes were requested.
    ChangesRequested,
    /// Review was dismissed.
    Dismissed,
    /// Comment was left without explicit approval/request.
    Commented,
}

impl fmt::Display for ReviewState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            ReviewState::Pending => "PENDING",
            ReviewState::Approved => "APPROVED",
            ReviewState::ChangesRequested => "CHANGES_REQUESTED",
            ReviewState::Dismissed => "DISMISSED",
            ReviewState::Commented => "COMMENTED",
        };
        write!(f, "{}", s)
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

    #[error("I/O error for path {path}: {source}")]
    Io {
        path: PathBuf,
        source: std::io::Error,
    },
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
        assert_eq!(Role::try_from("worker".to_string()).unwrap(), Role::Worker);

        // Case insensitive
        assert_eq!(Role::try_from("DEV".to_string()).unwrap(), Role::Dev);

        // Invalid
        let result = Role::try_from("invalid".to_string());
        assert!(matches!(result, Err(DomainError::Invalid { .. })));

        // Default
        assert_eq!(Role::default(), Role::Dev);
    }

    #[test]
    fn test_item_state_case_insensitive() {
        // lowercase
        let s: ItemState = serde_json::from_str("\"open\"").unwrap();
        assert_eq!(s, ItemState::Open);

        // UPPERCASE
        let s: ItemState = serde_json::from_str("\"OPEN\"").unwrap();
        assert_eq!(s, ItemState::Open);

        let s: ItemState = serde_json::from_str("\"CLOSED\"").unwrap();
        assert_eq!(s, ItemState::Closed);

        // Unknown
        let s: ItemState = serde_json::from_str("\"something_else\"").unwrap();
        assert_eq!(s, ItemState::Unknown);
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
    fn test_birth_branch_depth() {
        assert_eq!(BirthBranch::root().depth(), 0);
        assert_eq!(BirthBranch::from("main").depth(), 0);
        assert_eq!(BirthBranch::from("main.feature-a").depth(), 1);
        assert_eq!(BirthBranch::from("main.feature-a.sub-task").depth(), 2);
    }

    #[test]
    fn test_birth_branch_parent() {
        assert_eq!(BirthBranch::root().parent(), None);
        assert_eq!(
            BirthBranch::from("main.feature-a").parent(),
            Some(BirthBranch::from("main"))
        );
        assert_eq!(
            BirthBranch::from("main.feature-a.sub-task").parent(),
            Some(BirthBranch::from("main.feature-a"))
        );
    }

    #[test]
    fn test_birth_branch_child() {
        let root = BirthBranch::root();
        let child = root.child("feature-a");
        assert_eq!(child.as_str(), "main.feature-a");
        assert_eq!(child.depth(), 1);

        let grandchild = child.child("sub-task");
        assert_eq!(grandchild.as_str(), "main.feature-a.sub-task");
        assert_eq!(grandchild.depth(), 2);
    }

    #[test]
    fn test_agent_name_is_gemini_worker() {
        assert!(AgentName::from("impl-gemini").is_gemini_worker());
        assert!(!AgentName::from("impl-claude").is_gemini_worker());
    }

    #[test]
    fn test_birth_branch_empty_rejected() {
        let result = BirthBranch::try_from("".to_string());
        assert!(matches!(result, Err(DomainError::Empty { .. })));
    }

    // =========================================================================
    // Enum serde tests
    // =========================================================================

    /// Helper: deserialize a string value into T.
    fn deser<T: serde::de::DeserializeOwned>(s: &str) -> T {
        serde_json::from_value(serde_json::Value::String(s.into())).unwrap()
    }

    #[test]
    fn test_permission_mode_deserialize_all_variants() {
        assert_eq!(deser::<PermissionMode>("default"), PermissionMode::Default);
        assert_eq!(deser::<PermissionMode>("plan"), PermissionMode::Plan);
        assert_eq!(
            deser::<PermissionMode>("acceptEdits"),
            PermissionMode::AcceptEdits
        );
        assert_eq!(deser::<PermissionMode>("dontAsk"), PermissionMode::DontAsk);
        assert_eq!(
            deser::<PermissionMode>("bypassPermissions"),
            PermissionMode::BypassPermissions
        );
    }

    #[test]
    fn test_permission_mode_unknown_falls_back_to_default() {
        assert_eq!(
            deser::<PermissionMode>("newModeFromFuture"),
            PermissionMode::Default
        );
        assert_eq!(deser::<PermissionMode>(""), PermissionMode::Default);
    }

    #[test]
    fn test_permission_mode_serialize_roundtrip() {
        for mode in [
            PermissionMode::Default,
            PermissionMode::Plan,
            PermissionMode::AcceptEdits,
            PermissionMode::DontAsk,
            PermissionMode::BypassPermissions,
        ] {
            let json = serde_json::to_string(&mode).unwrap();
            let back: PermissionMode = serde_json::from_str(&json).unwrap();
            assert_eq!(mode, back);
        }
    }

    #[test]
    fn test_filter_state_serde() {
        assert_eq!(deser::<FilterState>("open"), FilterState::Open);
        assert_eq!(deser::<FilterState>("closed"), FilterState::Closed);
        assert_eq!(deser::<FilterState>("all"), FilterState::All);

        for state in [FilterState::Open, FilterState::Closed, FilterState::All] {
            let json = serde_json::to_string(&state).unwrap();
            let back: FilterState = serde_json::from_str(&json).unwrap();
            assert_eq!(state, back);
        }
    }

    #[test]
    fn test_filter_state_invalid_rejected() {
        let result =
            serde_json::from_value::<FilterState>(serde_json::Value::String("invalid".into()));
        assert!(result.is_err());
    }

    // =========================================================================
    // BranchName, Revision tests
    // =========================================================================

    #[test]
    fn test_branch_name_validation() {
        let name = BranchName::try_from("main".to_string()).unwrap();
        assert_eq!(name.as_str(), "main");

        let name = BranchName::try_from("feature/foo-bar".to_string()).unwrap();
        assert_eq!(name.as_str(), "feature/foo-bar");

        let result = BranchName::try_from("".to_string());
        assert!(matches!(result, Err(DomainError::Empty { .. })));
    }

    #[test]
    fn test_branch_name_serde_roundtrip() {
        let name = BranchName::try_from("main.feature-a".to_string()).unwrap();
        let json = serde_json::to_string(&name).unwrap();
        let back: BranchName = serde_json::from_str(&json).unwrap();
        assert_eq!(name, back);
    }

    #[test]
    fn test_revision_validation() {
        let rev = Revision::try_from("abc123".to_string()).unwrap();
        assert_eq!(rev.as_str(), "abc123");

        let rev = Revision::try_from("main".to_string()).unwrap();
        assert_eq!(rev.as_str(), "main");

        let result = Revision::try_from("".to_string());
        assert!(matches!(result, Err(DomainError::Empty { .. })));
    }

    #[test]
    fn test_revision_serde_roundtrip() {
        let rev = Revision::try_from("deadbeef".to_string()).unwrap();
        let json = serde_json::to_string(&rev).unwrap();
        let back: Revision = serde_json::from_str(&json).unwrap();
        assert_eq!(rev, back);
    }

    // =========================================================================
    // PRNumber tests
    // =========================================================================

    #[test]
    fn test_pr_number_validation() {
        let pr = PRNumber::try_from(1u64).unwrap();
        assert_eq!(pr.as_u64(), 1);

        let pr = PRNumber::try_from(999u64).unwrap();
        assert_eq!(pr.as_u64(), 999);

        let result = PRNumber::try_from(0u64);
        assert!(matches!(result, Err(DomainError::Invalid { .. })));
    }

    #[test]
    fn test_pr_number_new() {
        let pr = PRNumber::new(42);
        assert_eq!(pr.as_u64(), 42);
    }

    #[test]
    #[should_panic(expected = "PR number must be positive")]
    fn test_pr_number_new_zero_panics() {
        PRNumber::new(0);
    }

    #[test]
    fn test_pr_number_display() {
        let pr = PRNumber::new(123);
        assert_eq!(format!("{}", pr), "123");
    }

    #[test]
    fn test_pr_number_into_u64() {
        let pr = PRNumber::new(77);
        let n: u64 = pr.into();
        assert_eq!(n, 77);
    }

    #[test]
    fn test_pr_number_serde_roundtrip() {
        let pr = PRNumber::try_from(42u64).unwrap();
        let json = serde_json::to_string(&pr).unwrap();
        assert_eq!(json, "42");
        let back: PRNumber = serde_json::from_str(&json).unwrap();
        assert_eq!(pr, back);
    }

    #[test]
    fn test_pr_number_serde_zero_rejected() {
        let result = serde_json::from_str::<PRNumber>("0");
        assert!(result.is_err());
    }
}
