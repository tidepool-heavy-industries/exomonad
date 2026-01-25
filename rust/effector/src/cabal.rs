use anyhow::Result;
use crate::types::{CabalBuildResult, CabalTestResult};

pub fn build(_cwd: &str) -> Result<()> {
    let result = CabalBuildResult {
        success: true,
        errors: vec![],
        warnings: vec![],
    };
    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}

pub fn test(_cwd: &str, _package: Option<String>) -> Result<()> {
    let result = CabalTestResult {
        passed: 0,
        failed: 0,
        failures: vec![],
    };
    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}
