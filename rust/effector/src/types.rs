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
