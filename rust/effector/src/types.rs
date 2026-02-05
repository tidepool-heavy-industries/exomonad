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

#[cfg(test)]
mod tests {
    use super::*;

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
}
