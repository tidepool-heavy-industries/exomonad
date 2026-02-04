// File PR service - creates/updates GitHub PRs using gh CLI
//
// Uses `gh pr create` / `gh pr view` for idempotent PR management.
// Returns immediately with PR URL and number.

use crate::common::{ErrorCode, FFIBoundary, HostResult};
use crate::services::git;
use anyhow::{Context, Result};
use duct::cmd;
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use extism_convert::Json;
use serde::{Deserialize, Serialize};
use tracing::{debug, info, warn};

use super::zellij_events;

// ============================================================================
// Types
// ============================================================================

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct FilePRInput {
    pub title: String,
    pub body: String,
}

impl FFIBoundary for FilePRInput {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FilePROutput {
    pub pr_url: String,
    pub pr_number: u64,
    pub head_branch: String,
    pub base_branch: String,
    pub created: bool, // true if newly created, false if already existed
}

impl FFIBoundary for FilePROutput {}

// ============================================================================
// Host Output Wrapper
// ============================================================================

fn map_error(e: anyhow::Error) -> HostResult<FilePROutput> {
    let msg = e.to_string();
    let code = if msg.contains("not on a branch")
        || msg.contains("not a git repository")
        || msg.contains("no remote")
        || msg.contains("No remote")
    {
        ErrorCode::GitError
    } else if msg.contains("gh auth") || msg.contains("not logged") {
        ErrorCode::NotAuthenticated
    } else if msg.contains("already exists") {
        ErrorCode::AlreadyExists
    } else {
        ErrorCode::InternalError
    };

    HostResult::error(msg, code, None, None)
}

// ============================================================================
// Implementation
// ============================================================================

/// Check if a PR already exists for the current branch
fn check_existing_pr() -> Result<Option<(String, u64, String, String)>> {
    info!("[FilePR] Checking for existing PR");

    let output = cmd!(
        "gh",
        "pr",
        "view",
        "--json",
        "url,number,headRefName,baseRefName"
    )
    .unchecked()
    .stderr_to_stdout()
    .read()
    .context("Failed to execute gh pr view")?;

    debug!("[FilePR] gh pr view output length: {}", output.len());

    // Check if this is an error (no PR found)
    if output.contains("no pull requests found") || output.contains("could not find") {
        debug!("[FilePR] No existing PR found");
        return Ok(None);
    }

    debug!("[FilePR] Existing PR found: {}", output.trim());

    #[derive(Deserialize)]
    struct PRView {
        url: String,
        number: u64,
        #[serde(rename = "headRefName")]
        head_ref_name: String,
        #[serde(rename = "baseRefName")]
        base_ref_name: String,
    }

    let pr: PRView =
        serde_json::from_str(&output).context("Failed to parse gh pr view JSON output")?;

    Ok(Some((
        pr.url,
        pr.number,
        pr.head_ref_name,
        pr.base_ref_name,
    )))
}

/// Create a new PR using gh CLI
fn create_pr(input: &FilePRInput) -> Result<FilePROutput> {
    info!("[FilePR] Creating new PR: {}", input.title);

    let head_branch = git::get_current_branch()?;
    info!("[FilePR] Current branch: {}", head_branch);

    let args = vec![
        "pr",
        "create",
        "--title",
        &input.title,
        "--body",
        &input.body,
    ];

    info!("[FilePR] Executing: gh {}", args.join(" "));

    let stdout = cmd("gh", args.as_slice())
        .read()
        .context("Failed to execute gh pr create")?;

    info!("[FilePR] gh pr create succeeded");

    // gh pr create outputs the PR URL on success
    let pr_url = stdout.trim().to_string();
    info!("[FilePR] Created PR: {}", pr_url);

    // Extract PR number from URL (format: https://github.com/owner/repo/pull/123)
    let pr_number = pr_url
        .rsplit('/')
        .next()
        .and_then(|s| s.parse::<u64>().ok())
        .context("Failed to extract PR number from URL")?;

    // Get base branch from the created PR
    let base_branch = get_pr_base_branch(pr_number)?;

    // Emit pr:filed event
    // Extract agent_id from branch name (format: gh-123/slug)
    if let Some(agent_id_str) = git::extract_agent_id(&head_branch) {
        match exomonad_ui_protocol::AgentId::try_from(agent_id_str) {
            Ok(agent_id) => {
                let event = exomonad_ui_protocol::AgentEvent::PrFiled {
                    agent_id,
                    pr_number,
                    timestamp: zellij_events::now_iso8601(),
                };
                if let Err(e) = zellij_events::emit_event(&event) {
                    warn!("Failed to emit pr:filed event: {}", e);
                }
            }
            Err(e) => {
                warn!(
                    "Invalid agent_id in branch '{}', skipping event: {}",
                    head_branch, e
                );
            }
        }
    }

    Ok(FilePROutput {
        pr_url,
        pr_number,
        head_branch,
        base_branch,
        created: true,
    })
}

/// Get base branch for a PR number
fn get_pr_base_branch(pr_number: u64) -> Result<String> {
    let pr_num_str = pr_number.to_string();
    let output = cmd!("gh", "pr", "view", &pr_num_str, "--json", "baseRefName")
        .unchecked()
        .read()
        .context("Failed to get PR base branch")?;

    // If the command failed (e.g., network issue), default to main
    if output.contains("error") || output.is_empty() {
        return Ok("main".to_string());
    }

    #[derive(Deserialize)]
    struct PRBase {
        #[serde(rename = "baseRefName")]
        base_ref_name: String,
    }

    let pr: PRBase = serde_json::from_str(&output).unwrap_or(PRBase {
        base_ref_name: "main".to_string(),
    });

    Ok(pr.base_ref_name)
}

/// Main file_pr implementation
pub fn file_pr(input: &FilePRInput) -> Result<FilePROutput> {
    info!("[FilePR] Starting file_pr operation");

    // First check if PR already exists for current branch
    if let Some((url, number, head, base)) = check_existing_pr()? {
        info!("[FilePR] PR already exists: {} (#{number})", url);
        return Ok(FilePROutput {
            pr_url: url,
            pr_number: number,
            head_branch: head,
            base_branch: base,
            created: false,
        });
    }

    // Create new PR
    create_pr(input)
}

// ============================================================================
// Host Functions
// ============================================================================

fn get_input<T: serde::de::DeserializeOwned>(
    plugin: &mut CurrentPlugin,
    val: Val,
) -> std::result::Result<T, Error> {
    let handle = plugin
        .memory_from_val(&val)
        .ok_or_else(|| Error::msg("Invalid memory handle in input"))?;
    let bytes = plugin.memory_bytes(handle)?;
    Ok(serde_json::from_slice(bytes)?)
}


pub fn register_host_functions() -> Vec<Function> {
    vec![Function::new(
        "file_pr",
        [ValType::I64],
        [ValType::I64],
        UserData::new(()),
        file_pr_host_fn,
    )
    .with_namespace("env")]
}

fn file_pr_host_fn(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    _user_data: UserData<()>,
) -> std::result::Result<(), Error> {
    let _span = tracing::info_span!("host_function", function = "file_pr").entered();
    let input: FilePRInput = get_input(plugin, inputs[0])?;
    tracing::info!(title = %input.title, "Filing PR");

    let result = file_pr(&input);

    match &result {
        Ok(pr) => {
            tracing::info!(success = true, pr_url = %pr.pr_url, created = pr.created, "Completed")
        }
        Err(e) => tracing::warn!(error = %e, "Failed"),
    }

    let output: HostResult<FilePROutput> = match result {
        Ok(val) => HostResult::Success(val),
        Err(e) => map_error(e),
    };

    plugin.memory_set_val(&mut outputs[0], Json(output))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_host_error_codes() {
        let err: HostResult<FilePROutput> = map_error(anyhow::anyhow!("not on a branch"));
        match err {
            HostResult::Error(e) => assert_eq!(e.code, ErrorCode::GitError),
            _ => panic!("Expected error"),
        }

        let err: HostResult<FilePROutput> = map_error(anyhow::anyhow!("gh auth login required"));
        match err {
            HostResult::Error(e) => assert_eq!(e.code, ErrorCode::NotAuthenticated),
            _ => panic!("Expected error"),
        }
    }
}
