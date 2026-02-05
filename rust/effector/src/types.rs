use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct CabalBuildResult {
    pub success: bool,
    pub errors: Vec<BuildError>,
    pub warnings: Vec<BuildWarning>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct BuildError {
    pub file: String,
    pub line: u32,
    pub column: u32,
    pub message: String,
    pub error_type: String, // "type-error", "parse-error", "scope-error", etc.
}

#[derive(Serialize, Deserialize, Debug)]
pub struct BuildWarning {
    pub file: String,
    pub line: u32,
    pub message: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct CabalTestResult {
    pub passed: u32,
    pub failed: u32,
    pub failures: Vec<TestFailure>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct TestFailure {
    pub suite: String,
    pub test_name: String,
    pub message: String,
    pub location: Option<String>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct GitStatusResult {
    pub branch: String,
    pub dirty: Vec<String>,  // modified/untracked files
    pub staged: Vec<String>, // staged files
    pub ahead: u32,
    pub behind: u32,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct GitDiffResult {
    pub files: Vec<DiffFile>,
    pub additions: u32,
    pub deletions: u32,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct DiffFile {
    pub path: String,
    pub status: String, // "added", "modified", "deleted"
    pub additions: u32,
    pub deletions: u32,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum PrState {
    Open,
    Closed,
    Merged,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum ReviewStatus {
    Pending,
    Approved,
    ChangesRequested,
    Commented,
    Dismissed,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct GhPrStatusResult {
    pub exists: bool,
    pub url: Option<String>,
    pub number: Option<u32>,
    pub state: Option<PrState>,              // "open", "closed", "merged"
    pub review_status: Option<ReviewStatus>, // "pending", "approved", "changes_requested"
    pub comments: Vec<PrComment>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct PrComment {
    pub author: String,
    pub body: String,
    pub path: Option<String>, // for review comments
    pub line: Option<u32>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct GhPrCreateResult {
    pub url: String,
    pub number: u32,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pr_state_serialization() {
        // Serialize
        let open = PrState::Open;
        let json = serde_json::to_string(&open).unwrap();
        assert_eq!(json, "\"open\"");

        // Roundtrip
        let deserialized: PrState = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized, PrState::Open);

        // All variants
        assert_eq!(serde_json::to_string(&PrState::Closed).unwrap(), "\"closed\"");
        assert_eq!(serde_json::to_string(&PrState::Merged).unwrap(), "\"merged\"");
    }

    #[test]
    fn test_review_status_serialization() {
        // Serialize
        let approved = ReviewStatus::Approved;
        let json = serde_json::to_string(&approved).unwrap();
        assert_eq!(json, "\"approved\"");

        // Roundtrip
        let deserialized: ReviewStatus = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized, ReviewStatus::Approved);

        // All variants
        assert_eq!(serde_json::to_string(&ReviewStatus::Pending).unwrap(), "\"pending\"");
        assert_eq!(serde_json::to_string(&ReviewStatus::ChangesRequested).unwrap(), "\"changes_requested\"");
        assert_eq!(serde_json::to_string(&ReviewStatus::Commented).unwrap(), "\"commented\"");
        assert_eq!(serde_json::to_string(&ReviewStatus::Dismissed).unwrap(), "\"dismissed\"");
    }

    #[test]
    fn test_cabal_build_result_roundtrip() {
        let result = CabalBuildResult {
            success: true,
            errors: vec![BuildError {
                file: "src/Lib.hs".to_string(),
                line: 10,
                column: 5,
                message: "Type error".to_string(),
                error_type: "type-error".to_string(),
            }],
            warnings: vec![BuildWarning {
                file: "src/Main.hs".to_string(),
                line: 20,
                message: "Unused import".to_string(),
            }],
        };

        let json = serde_json::to_string(&result).unwrap();
        let deserialized: CabalBuildResult = serde_json::from_str(&json).unwrap();

        assert_eq!(deserialized.success, true);
        assert_eq!(deserialized.errors.len(), 1);
        assert_eq!(deserialized.errors[0].file, "src/Lib.hs");
        assert_eq!(deserialized.warnings.len(), 1);
    }

    #[test]
    fn test_git_status_result_roundtrip() {
        let result = GitStatusResult {
            branch: "main".to_string(),
            dirty: vec!["file1.rs".to_string(), "file2.rs".to_string()],
            staged: vec!["staged.rs".to_string()],
            ahead: 3,
            behind: 1,
        };

        let json = serde_json::to_string(&result).unwrap();
        let deserialized: GitStatusResult = serde_json::from_str(&json).unwrap();

        assert_eq!(deserialized.branch, "main");
        assert_eq!(deserialized.dirty.len(), 2);
        assert_eq!(deserialized.staged.len(), 1);
        assert_eq!(deserialized.ahead, 3);
        assert_eq!(deserialized.behind, 1);
    }

    #[test]
    fn test_gh_pr_status_result_roundtrip() {
        let result = GhPrStatusResult {
            exists: true,
            url: Some("https://github.com/owner/repo/pull/123".to_string()),
            number: Some(123),
            state: Some(PrState::Open),
            review_status: Some(ReviewStatus::Approved),
            comments: vec![PrComment {
                author: "alice".to_string(),
                body: "LGTM".to_string(),
                path: Some("src/lib.rs".to_string()),
                line: Some(42),
            }],
        };

        let json = serde_json::to_string(&result).unwrap();
        let deserialized: GhPrStatusResult = serde_json::from_str(&json).unwrap();

        assert!(deserialized.exists);
        assert_eq!(deserialized.number, Some(123));
        assert_eq!(deserialized.state, Some(PrState::Open));
        assert_eq!(deserialized.review_status, Some(ReviewStatus::Approved));
        assert_eq!(deserialized.comments.len(), 1);
        assert_eq!(deserialized.comments[0].author, "alice");
    }
}
