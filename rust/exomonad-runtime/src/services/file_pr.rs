// File PR service - creates/updates GitHub PRs using gh CLI
//
// Uses `gh pr create` / `gh pr view` for idempotent PR management.
// Returns immediately with PR URL and number.

use anyhow::{Context, Result};
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use serde::{Deserialize, Serialize};
use std::process::Command;
use tracing::{debug, error, info, warn};

use super::{git, zellij_events};

// ============================================================================
// Types
// ============================================================================

#[derive(Debug, Clone, Deserialize)]
pub struct FilePRInput {
    pub title: String,
    pub body: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct FilePROutput {
    pub pr_url: String,
    pub pr_number: u64,
    pub head_branch: String,
    pub base_branch: String,
    pub created: bool, // true if newly created, false if already existed
}

// ============================================================================
// Host Output Wrapper
// ============================================================================

#[derive(Serialize)]
#[serde(tag = "kind", content = "payload")]
enum HostResult<T> {
    Success(T),
    Error(HostError),
}

#[derive(Serialize)]
struct HostError {
    message: String,
    code: String,
}

impl<T> From<Result<T>> for HostResult<T> {
    fn from(res: Result<T>) -> Self {
        match res {
            Ok(val) => HostResult::Success(val),
            Err(e) => {
                let msg = e.to_string();
                let code = if msg.contains("not on a branch")
                    || msg.contains("not a git repository")
                {
                    "not_git_repo"
                } else if msg.contains("no remote") || msg.contains("No remote") {
                    "no_remote"
                } else if msg.contains("gh auth") || msg.contains("not logged") {
                    "not_authenticated"
                } else if msg.contains("already exists") {
                    "pr_exists"
                } else {
                    "internal_error"
                };
                HostResult::Error(HostError {
                    message: msg,
                    code: code.to_string(),
                })
            }
        }
    }
}

// ============================================================================
// Implementation
// ============================================================================

/// Check if a PR already exists for the current branch
fn check_existing_pr() -> Result<Option<(String, u64, String, String)>> {
    info!("[FilePR] Checking for existing PR");

    let output = Command::new("gh")
        .args(["pr", "view", "--json", "url,number,headRefName,baseRefName"])
        .output()
        .context("Failed to execute gh pr view")?;

    debug!(
        "[FilePR] gh pr view exit code: {}",
        output.status.code().unwrap_or(-1)
    );

    if !output.status.success() {
        // No existing PR is not an error - just means we need to create one
        let stderr = String::from_utf8_lossy(&output.stderr);
        debug!("[FilePR] No existing PR found: {}", stderr.trim());
        return Ok(None);
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    debug!("[FilePR] Existing PR found: {}", stdout.trim());

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
        serde_json::from_str(&stdout).context("Failed to parse gh pr view JSON output")?;

    Ok(Some((pr.url, pr.number, pr.head_ref_name, pr.base_ref_name)))
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

    let output = Command::new("gh")
        .args(&args)
        .output()
        .context("Failed to execute gh pr create")?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    info!("[FilePR] gh pr create exit code: {:?}", output.status.code());
    if !stderr.is_empty() {
        debug!("[FilePR] stderr: {}", stderr.trim());
    }

    if !output.status.success() {
        error!("[FilePR] FAILED: {}", stderr.trim());
        anyhow::bail!("Failed to create PR: {}", stderr.trim());
    }

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
    if let Some(agent_id) = git::extract_agent_id(&head_branch) {
        let event = exomonad_ui_protocol::AgentEvent::PrFiled {
            agent_id,
            pr_number,
            timestamp: zellij_events::now_iso8601(),
        };
        if let Err(e) = zellij_events::emit_event(&event) {
            warn!("Failed to emit pr:filed event: {}", e);
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
    let output = Command::new("gh")
        .args([
            "pr",
            "view",
            &pr_number.to_string(),
            "--json",
            "baseRefName",
        ])
        .output()
        .context("Failed to get PR base branch")?;

    if !output.status.success() {
        // Default to main if we can't get it
        return Ok("main".to_string());
    }

    #[derive(Deserialize)]
    struct PRBase {
        #[serde(rename = "baseRefName")]
        base_ref_name: String,
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let pr: PRBase = serde_json::from_str(&stdout).unwrap_or(PRBase {
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

fn set_output<T: Serialize>(
    plugin: &mut CurrentPlugin,
    data: &T,
) -> std::result::Result<Val, Error> {
    let json = serde_json::to_vec(data)?;
    let handle = plugin.memory_new(json)?;
    Ok(plugin.memory_to_val(handle))
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
    let input: FilePRInput = get_input(plugin, inputs[0])?;

    let result = file_pr(&input);
    let output: HostResult<FilePROutput> = result.into();

    outputs[0] = set_output(plugin, &output)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_host_error_codes() {
        let err: HostResult<()> = Err(anyhow::anyhow!("not on a branch")).into();
        match err {
            HostResult::Error(e) => assert_eq!(e.code, "not_git_repo"),
            _ => panic!("Expected error"),
        }

        let err: HostResult<()> = Err(anyhow::anyhow!("gh auth login required")).into();
        match err {
            HostResult::Error(e) => assert_eq!(e.code, "not_authenticated"),
            _ => panic!("Expected error"),
        }
    }
}
