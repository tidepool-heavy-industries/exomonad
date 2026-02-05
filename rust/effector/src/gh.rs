use crate::types::{GhPrCreateResult, GhPrStatusResult, PrComment, PrState, ReviewStatus};
use anyhow::{Context, Result};
use duct::cmd;
use serde_json::Value;

/// Check if the gh CLI output indicates no PR exists.
pub fn is_no_pr_output(output: &str) -> bool {
    output.contains("no pull requests found") || output.contains("could not find any pull record")
}

/// Parse gh pr view JSON output into a GhPrStatusResult.
/// Returns None if the JSON is invalid.
pub fn parse_pr_status_json(json: &Value) -> GhPrStatusResult {
    let url = json["url"].as_str().map(|s| s.to_string());
    let number = json["number"].as_u64().map(|n| n as u32);
    let state = json["state"].as_str().and_then(|s| match s {
        "OPEN" => Some(PrState::Open),
        "CLOSED" => Some(PrState::Closed),
        "MERGED" => Some(PrState::Merged),
        _ => None,
    });

    let review_status = match json["reviewDecision"].as_str() {
        Some("REVIEW_REQUIRED") => Some(ReviewStatus::Pending),
        Some("APPROVED") => Some(ReviewStatus::Approved),
        Some("CHANGES_REQUESTED") => Some(ReviewStatus::ChangesRequested),
        _ => None,
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

    GhPrStatusResult {
        exists: true,
        url,
        number,
        state,
        review_status,
        comments,
    }
}

/// Extract PR number from a GitHub PR URL.
/// URL format: https://github.com/owner/repo/pull/123
pub fn extract_pr_number_from_url(url: &str) -> Option<u32> {
    url.split('/').next_back().and_then(|s| s.parse().ok())
}

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

    if is_no_pr_output(&output) {
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

    let result = parse_pr_status_json(&json);
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
    let url = stdout.trim().to_string();

    let number = extract_pr_number_from_url(&url).unwrap_or(0);

    let result = GhPrCreateResult { url, number };

    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_is_no_pr_output_found() {
        assert!(is_no_pr_output("no pull requests found for branch"));
        assert!(is_no_pr_output("could not find any pull record"));
    }

    #[test]
    fn test_is_no_pr_output_has_pr() {
        assert!(!is_no_pr_output(r#"{"number":123,"url":"..."}"#));
    }

    #[test]
    fn test_pr_status_exists() {
        let json = json!({
            "number": 123,
            "url": "https://github.com/owner/repo/pull/123",
            "state": "OPEN",
            "reviewDecision": "APPROVED"
        });
        let result = parse_pr_status_json(&json);
        assert!(result.exists);
        assert_eq!(result.number, Some(123));
        assert_eq!(result.url, Some("https://github.com/owner/repo/pull/123".to_string()));
    }

    #[test]
    fn test_pr_status_open() {
        let json = json!({ "state": "OPEN" });
        let result = parse_pr_status_json(&json);
        assert_eq!(result.state, Some(PrState::Open));
    }

    #[test]
    fn test_pr_status_closed() {
        let json = json!({ "state": "CLOSED" });
        let result = parse_pr_status_json(&json);
        assert_eq!(result.state, Some(PrState::Closed));
    }

    #[test]
    fn test_pr_status_merged() {
        let json = json!({ "state": "MERGED" });
        let result = parse_pr_status_json(&json);
        assert_eq!(result.state, Some(PrState::Merged));
    }

    #[test]
    fn test_pr_status_approved() {
        let json = json!({ "reviewDecision": "APPROVED" });
        let result = parse_pr_status_json(&json);
        assert_eq!(result.review_status, Some(ReviewStatus::Approved));
    }

    #[test]
    fn test_pr_status_changes_requested() {
        let json = json!({ "reviewDecision": "CHANGES_REQUESTED" });
        let result = parse_pr_status_json(&json);
        assert_eq!(result.review_status, Some(ReviewStatus::ChangesRequested));
    }

    #[test]
    fn test_pr_status_pending() {
        let json = json!({ "reviewDecision": "REVIEW_REQUIRED" });
        let result = parse_pr_status_json(&json);
        assert_eq!(result.review_status, Some(ReviewStatus::Pending));
    }

    #[test]
    fn test_pr_status_with_comments() {
        let json = json!({
            "comments": [
                {
                    "author": {"login": "alice"},
                    "body": "LGTM"
                }
            ],
            "reviews": [
                {
                    "comments": [
                        {
                            "author": {"login": "bob"},
                            "body": "Fix this line",
                            "path": "src/lib.rs",
                            "line": 42
                        }
                    ]
                }
            ]
        });
        let result = parse_pr_status_json(&json);
        assert_eq!(result.comments.len(), 2);

        let alice_comment = result.comments.iter().find(|c| c.author == "alice").unwrap();
        assert_eq!(alice_comment.body, "LGTM");
        assert!(alice_comment.path.is_none());

        let bob_comment = result.comments.iter().find(|c| c.author == "bob").unwrap();
        assert_eq!(bob_comment.body, "Fix this line");
        assert_eq!(bob_comment.path, Some("src/lib.rs".to_string()));
        assert_eq!(bob_comment.line, Some(42));
    }

    #[test]
    fn test_extract_pr_number_from_url() {
        assert_eq!(
            extract_pr_number_from_url("https://github.com/owner/repo/pull/123"),
            Some(123)
        );
        assert_eq!(
            extract_pr_number_from_url("https://github.com/owner/repo/pull/1"),
            Some(1)
        );
        assert_eq!(
            extract_pr_number_from_url("https://github.com/owner/repo/pull/999999"),
            Some(999999)
        );
    }

    #[test]
    fn test_extract_pr_number_from_url_invalid() {
        assert_eq!(extract_pr_number_from_url("not a url"), None);
        assert_eq!(
            extract_pr_number_from_url("https://github.com/owner/repo/pull/abc"),
            None
        );
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn prop_parse_pr_status_json_never_panics(
            number in any::<Option<u64>>(),
            url in ".*",
            state in prop_oneof!["OPEN", "CLOSED", "MERGED", "UNKNOWN", ""],
        ) {
            let json = serde_json::json!({
                "number": number,
                "url": url,
                "state": state,
            });
            // Should never panic regardless of input
            let _ = parse_pr_status_json(&json);
        }

        #[test]
        fn prop_extract_pr_number_never_panics(url in ".*") {
            // Should never panic regardless of input
            let _ = extract_pr_number_from_url(&url);
        }
    }
}
