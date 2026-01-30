use crate::types::{GhPrCreateResult, GhPrStatusResult, PrComment};
use anyhow::{anyhow, Context, Result};
use serde_json::Value;
use std::process::Command;

pub fn pr_status(branch: Option<String>) -> Result<()> {
    let mut cmd = Command::new("gh");
    cmd.arg("pr").arg("view");

    if let Some(b) = branch {
        cmd.arg(b);
    }

    cmd.arg("--json")
        .arg("number,url,state,reviewDecision,reviews,comments");

    let output = cmd
        .output()
        .context("Failed to execute 'gh' CLI. Is it installed?")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        if stderr.contains("no pull requests found")
            || stderr.contains("could not find any pull record")
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
        return Err(anyhow!("gh pr view failed: {}", stderr));
    }

    let json: Value = serde_json::from_slice(&output.stdout)?;

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
    let mut cmd = Command::new("gh");
    cmd.arg("pr")
        .arg("create")
        .arg("--title")
        .arg(title)
        .arg("--body")
        .arg(body);

    if let Some(b) = base {
        cmd.arg("--base").arg(b);
    }

    let output = cmd
        .output()
        .context("Failed to execute 'gh' CLI. Is it installed?")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(anyhow!("gh pr create failed: {}", stderr));
    }

    let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();
    // gh pr create output is just the URL of the created PR
    let url = stdout.clone();

    // We want the number too. We can try to extract it from the URL or run pr view.
    // URL format: https://github.com/owner/repo/pull/123
    let number = url
        .split('/')
        .last()
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(0);

    let result = GhPrCreateResult { url, number };

    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}
