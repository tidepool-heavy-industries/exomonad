use crate::types::{GhPrCreateResult, GhPrStatusResult, PrComment};
use anyhow::{Context, Result};
use duct::cmd;
use serde_json::Value;

pub fn pr_status(branch: Option<String>) -> Result<()> {
    let mut args = vec!["pr", "view"];

    let branch_str;
    if let Some(b) = &branch {
        branch_str = b.clone();
        args.push(&branch_str);
    }

    args.extend(&["--json", "number,url,state,reviewDecision,reviews,comments"]);

    let output = cmd("gh", args.as_slice())
        .unchecked()
        .stderr_to_stdout()
        .read()
        .context("Failed to execute 'gh' CLI. Is it installed?")?;

    if output.contains("no pull requests found")
        || output.contains("could not find any pull record")
    {
        let result = GhPrStatusResult {
            exists: false,
            url: None,
            number: None,
            state: None,
            review_status: None,
            comments: vec![],
        };
        println!("{}", serde_json::to_string(&result)?);
        return Ok(());
    }

    let json: Value = serde_json::from_str(&output).with_context(|| {
        format!(
            "Failed to parse 'gh' output as JSON. Raw output: {}",
            output
        )
    })?;

    let exists = true;
    let url = json["url"].as_str().map(|s| s.to_string());
    let number = json["number"].as_u64().map(|n| n as u32);
    let state = json["state"].as_str().map(|s| s.to_lowercase());

    let review_status = match json["reviewDecision"].as_str() {
        Some("REVIEW_REQUIRED") => Some("pending".to_string()),
        Some("APPROVED") => Some("approved".to_string()),
        Some("CHANGES_REQUESTED") => Some("changes_requested".to_string()),
        Some(other) => Some(other.to_lowercase()),
        None => None,
    };

    let mut comments = Vec::new();

    // Top level comments
    if let Some(top_comments) = json["comments"].as_array() {
        for c in top_comments {
            comments.push(PrComment {
                author: c["author"]["login"]
                    .as_str()
                    .unwrap_or("unknown")
                    .to_string(),
                body: c["body"].as_str().unwrap_or("").to_string(),
                path: None,
                line: None,
            });
        }
    }

    // Review comments (inline)
    if let Some(reviews) = json["reviews"].as_array() {
        for r in reviews {
            if let Some(review_comments) = r["comments"].as_array() {
                for c in review_comments {
                    comments.push(PrComment {
                        author: c["author"]["login"]
                            .as_str()
                            .unwrap_or("unknown")
                            .to_string(),
                        body: c["body"].as_str().unwrap_or("").to_string(),
                        path: c["path"].as_str().map(|s| s.to_string()),
                        line: c["line"].as_u64().map(|n| n as u32),
                    });
                }
            }
        }
    }

    let result = GhPrStatusResult {
        exists,
        url,
        number,
        state,
        review_status,
        comments,
    };

    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}

pub fn pr_create(title: String, body: String, base: Option<String>) -> Result<()> {
    let mut args = vec!["pr", "create", "--title", &title, "--body", &body];

    let base_str;
    if let Some(b) = &base {
        base_str = b.clone();
        args.extend(&["--base", &base_str]);
    }

    let stdout = cmd("gh", args.as_slice())
        .read()
        .context("Failed to execute 'gh' CLI or PR creation failed")?;
    // gh pr create output is just the URL of the created PR
    let url = stdout.clone();

    // We want the number too. We can try to extract it from the URL or run pr view.
    // URL format: https://github.com/owner/repo/pull/123
    let number = url
        .split('/')
        .next_back()
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(0);

    let result = GhPrCreateResult { url, number };

    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}
