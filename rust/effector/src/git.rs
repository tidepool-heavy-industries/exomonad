use anyhow::Result;
use crate::types::{GitStatusResult, GitDiffResult};

pub fn status(_cwd: &str) -> Result<()> {
    let result = GitStatusResult {
        branch: "main".to_string(),
        dirty: vec![],
        staged: vec![],
        ahead: 0,
        behind: 0,
    };
    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}

pub fn diff(_cwd: &str, _staged: bool) -> Result<()> {
    let result = GitDiffResult {
        files: vec![],
        additions: 0,
        deletions: 0,
    };
    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}
