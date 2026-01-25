use anyhow::Result;
use crate::types::{GhPrStatusResult, GhPrCreateResult};

pub fn pr_status(_branch: Option<String>) -> Result<()> {
    let result = GhPrStatusResult {
        exists: false,
        url: None,
        number: None,
        state: None,
        review_status: None,
        comments: vec![],
    };
    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}

pub fn pr_create(_title: String, _body: String, _base: Option<String>) -> Result<()> {
    let result = GhPrCreateResult {
        url: "https://github.com/example/repo/pull/1".to_string(),
        number: 1,
    };
    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}
