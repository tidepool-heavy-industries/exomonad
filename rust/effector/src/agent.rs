//! Coarse agent setup/teardown operations.
//!
//! Provides all-in-one filesystem operations for agent lifecycle:
//! - `setup`: Create worktree, write config files, symlinks, hook settings
//! - `teardown`: Remove worktree and prune references
//! - `fetch_origin`: Pre-spawn git fetch
//! - `list`: List agent worktrees as structured JSON
//! - `is_merged`: Check if a branch has been merged into main

use anyhow::{Context, Result};
use duct::cmd;
use serde::Serialize;
use std::os::unix::fs::symlink;
use std::path::Path;

// ============================================================================
// Types
// ============================================================================

#[derive(Debug, Serialize)]
pub struct SetupResult {
    pub worktree_path: String,
    pub branch_name: String,
    pub config_written: bool,
    pub mcp_json_written: bool,
    pub hook_settings_written: bool,
}

#[derive(Debug, Serialize)]
pub struct TeardownResult {
    pub removed: bool,
}

#[derive(Debug, Serialize)]
pub struct WorktreeInfo {
    pub name: String,
    pub path: String,
    pub branch: String,
    pub issue_id: Option<String>,
    pub slug: Option<String>,
    pub agent_type: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct IsMergedResult {
    pub merged: bool,
    pub branch: String,
}

/// Envelope for structured JSON output.
#[derive(Debug, Serialize)]
#[serde(tag = "kind")]
enum Output<T: Serialize> {
    #[serde(rename = "success")]
    Success { value: T },
    #[serde(rename = "error")]
    Error { code: String, message: String },
}

fn print_success<T: Serialize>(value: T) -> Result<()> {
    let output = Output::Success { value };
    println!("{}", serde_json::to_string(&output)?);
    Ok(())
}

fn print_error(code: &str, message: &str) -> Result<()> {
    let output: Output<()> = Output::Error {
        code: code.to_string(),
        message: message.to_string(),
    };
    println!("{}", serde_json::to_string(&output)?);
    Ok(())
}

// ============================================================================
// Commands
// ============================================================================

/// All-in-one agent workspace setup.
///
/// Creates git worktree, writes config files, creates symlinks,
/// and writes agent-specific hook configuration.
pub fn setup(
    project_dir: &str,
    worktree_path: &str,
    branch: &str,
    start_point: &str,
    role: &str,
    agent_type: &str,
    sidecar_path: &str,
) -> Result<()> {
    let project = Path::new(project_dir)
        .canonicalize()
        .context("Failed to resolve project directory")?;
    let wt_path = project.join(worktree_path);

    // 1. Create worktree
    if wt_path.exists() {
        eprintln!("[effector] Worktree already exists, reusing: {}", wt_path.display());
    } else {
        if let Some(parent) = wt_path.parent() {
            std::fs::create_dir_all(parent)
                .context("Failed to create worktree parent directory")?;
        }

        eprintln!("[effector] Creating worktree: git worktree add -b {} {} {}", branch, wt_path.display(), start_point);
        let result = cmd("git", &["worktree", "add", "-b", branch, &wt_path.to_string_lossy(), start_point])
            .dir(&project)
            .stderr_to_stdout()
            .unchecked()
            .read();

        match result {
            Ok(output) if output.contains("already exists") => {
                eprintln!("[effector] Branch already exists, creating worktree without -b");
                cmd("git", &["worktree", "add", &wt_path.to_string_lossy(), branch])
                    .dir(&project)
                    .run()
                    .context("git worktree add (fallback) failed")?;
            }
            Ok(_) => {}
            Err(e) => {
                return print_error("worktree_failed", &format!("git worktree add failed: {}", e));
            }
        }
    }

    // 2. Write .exomonad/config.toml
    let exomonad_dir = wt_path.join(".exomonad");
    std::fs::create_dir_all(&exomonad_dir)
        .context("Failed to create .exomonad directory")?;

    let config_content = format!(
        "# Agent config (auto-generated)\ndefault_role = \"{}\"\nproject_dir = \"../../..\"\n",
        role
    );
    std::fs::write(exomonad_dir.join("config.toml"), &config_content)
        .context("Failed to write config.toml")?;
    eprintln!("[effector] Wrote .exomonad/config.toml (default_role={})", role);

    // 3. Create symlinks for shared resources
    // Worktree at: {project}/.exomonad/worktrees/gh-xxx/
    // Symlinks at: {project}/.exomonad/worktrees/gh-xxx/.exomonad/{name}
    // Target:      ../../../{name} (up to gh-xxx, up to worktrees, up to .exomonad, then name)
    let symlinks = [
        ("wasm", "../../../wasm"),
        ("roles", "../../../roles"),
        ("lib", "../../../lib"),
        ("flake.nix", "../../../flake.nix"),
        ("flake.lock", "../../../flake.lock"),
    ];

    for (name, target) in symlinks {
        let link_path = exomonad_dir.join(name);
        let target_path = Path::new(target);

        // Remove existing non-symlink entries
        if link_path.exists() && !link_path.is_symlink() {
            if link_path.is_dir() {
                std::fs::remove_dir_all(&link_path).ok();
            } else if link_path.is_file() {
                std::fs::remove_file(&link_path).ok();
            }
        }

        if !link_path.exists() {
            if let Err(e) = symlink(target_path, &link_path) {
                eprintln!("[effector] Warning: Failed to create symlink {}: {}", name, e);
            } else {
                eprintln!("[effector] Created symlink: {} -> {}", name, target);
            }
        }
    }

    // 4. Write .gitignore
    let gitignore_content = "# Auto-generated for worktree\nresult\n";
    std::fs::write(exomonad_dir.join(".gitignore"), gitignore_content)
        .context("Failed to write .gitignore")?;

    // 5. Write .mcp.json
    let mcp_content = format!(
        r#"{{
  "mcpServers": {{
    "exomonad": {{
      "command": "{}",
      "args": ["mcp-stdio"]
    }}
  }}
}}"#,
        sidecar_path
    );
    std::fs::write(wt_path.join(".mcp.json"), &mcp_content)
        .context("Failed to write .mcp.json")?;
    eprintln!("[effector] Wrote .mcp.json");

    // 6. Write agent-specific hook configuration
    let hook_settings_written = match agent_type {
        "claude" => {
            let claude_dir = wt_path.join(".claude");
            std::fs::create_dir_all(&claude_dir)
                .context("Failed to create .claude directory")?;

            let settings_content = format!(
                r#"{{
  "enableAllProjectMcpServers": true,
  "hooks": {{
    "SubagentStop": [
      {{
        "hooks": [
          {{
            "type": "command",
            "command": "{} hook subagent-stop"
          }}
        ]
      }}
    ]
  }}
}}"#,
                sidecar_path
            );
            std::fs::write(claude_dir.join("settings.local.json"), &settings_content)
                .context("Failed to write claude settings")?;
            eprintln!("[effector] Wrote .claude/settings.local.json (SubagentStop hook)");
            true
        }
        "gemini" => {
            let gemini_dir = wt_path.join(".gemini");
            std::fs::create_dir_all(&gemini_dir)
                .context("Failed to create .gemini directory")?;

            let settings_content = format!(
                r#"{{
  "hooks": {{
    "AfterAgent": [
      {{
        "hooks": [
          {{
            "name": "stop-check",
            "type": "command",
            "command": "{} hook after-agent --runtime gemini",
            "timeout": 30000
          }}
        ]
      }}
    ]
  }}
}}"#,
                sidecar_path
            );
            std::fs::write(gemini_dir.join("settings.json"), &settings_content)
                .context("Failed to write gemini settings")?;
            eprintln!("[effector] Wrote .gemini/settings.json (AfterAgent hook)");
            true
        }
        other => {
            eprintln!("[effector] Warning: Unknown agent type '{}', skipping hook settings", other);
            false
        }
    };

    print_success(SetupResult {
        worktree_path: wt_path.to_string_lossy().to_string(),
        branch_name: branch.to_string(),
        config_written: true,
        mcp_json_written: true,
        hook_settings_written,
    })
}

/// Remove agent worktree and prune stale references.
pub fn teardown(project_dir: &str, worktree_path: &str, force: bool) -> Result<()> {
    let project = Path::new(project_dir)
        .canonicalize()
        .context("Failed to resolve project directory")?;
    let wt_path = project.join(worktree_path);

    if !wt_path.exists() {
        return print_error("not_found", &format!("Worktree does not exist: {}", wt_path.display()));
    }

    eprintln!("[effector] Removing worktree: {}", wt_path.display());

    let mut args = vec!["worktree", "remove"];
    if force {
        args.push("--force");
    }
    let wt_str = wt_path.to_string_lossy().to_string();
    args.push(&wt_str);

    let result = cmd("git", &args)
        .dir(&project)
        .stderr_to_stdout()
        .unchecked()
        .read()
        .context("Failed to run git worktree remove")?;

    // Check if it actually worked by seeing if directory is gone
    if wt_path.exists() {
        return print_error("remove_failed", &format!("git worktree remove failed: {}", result));
    }

    // Prune stale references
    eprintln!("[effector] Pruning stale worktree references");
    cmd("git", &["worktree", "prune"])
        .dir(&project)
        .unchecked()
        .run()
        .ok();

    print_success(TeardownResult { removed: true })
}

/// Fetch origin main (pre-spawn preparation).
pub fn fetch_origin(project_dir: &str) -> Result<()> {
    let project = Path::new(project_dir)
        .canonicalize()
        .context("Failed to resolve project directory")?;

    eprintln!("[effector] Running git fetch origin main");

    let result = cmd("git", &["fetch", "origin", "main"])
        .dir(&project)
        .stderr_to_stdout()
        .unchecked()
        .read();

    match result {
        Ok(output) => {
            eprintln!("[effector] git fetch completed: {}", output.trim());
            print_success(serde_json::json!({ "fetched": true }))
        }
        Err(e) => {
            print_error("fetch_failed", &format!("git fetch origin main failed: {}", e))
        }
    }
}

/// List agent worktrees as structured JSON.
pub fn list(project_dir: &str) -> Result<()> {
    let project = Path::new(project_dir)
        .canonicalize()
        .context("Failed to resolve project directory")?;

    let output = cmd("git", &["worktree", "list", "--porcelain"])
        .dir(&project)
        .read()
        .context("git worktree list failed")?;

    let mut worktrees = Vec::new();
    let mut current_path: Option<String> = None;
    let mut current_branch: Option<String> = None;

    for line in output.lines() {
        if let Some(path) = line.strip_prefix("worktree ") {
            current_path = Some(path.to_string());
        } else if let Some(branch) = line.strip_prefix("branch refs/heads/") {
            current_branch = Some(branch.to_string());
        } else if line.is_empty() {
            if let (Some(path), Some(branch)) = (current_path.take(), current_branch.take()) {
                let name = Path::new(&path)
                    .file_name()
                    .and_then(|n| n.to_str())
                    .unwrap_or("")
                    .to_string();

                if name.starts_with("gh-") {
                    let parsed = parse_worktree_name(&name);
                    worktrees.push(WorktreeInfo {
                        name: name.clone(),
                        path,
                        branch,
                        issue_id: parsed.as_ref().map(|p| p.issue_id.to_string()),
                        slug: parsed.as_ref().map(|p| p.slug.to_string()),
                        agent_type: parsed.as_ref().and_then(|p| p.agent_type.clone()),
                    });
                }
            } else {
                // Reset for next worktree entry
                current_path = None;
                current_branch = None;
            }
        }
    }

    print_success(worktrees)
}

/// Check if a branch has been merged into origin/main.
pub fn is_merged(project_dir: &str, branch: &str) -> Result<()> {
    let project = Path::new(project_dir)
        .canonicalize()
        .context("Failed to resolve project directory")?;

    let result = cmd("git", &["merge-base", "--is-ancestor", branch, "origin/main"])
        .dir(&project)
        .unchecked()
        .run()
        .context("Failed to run git merge-base")?;

    print_success(IsMergedResult {
        merged: result.status.success(),
        branch: branch.to_string(),
    })
}

// ============================================================================
// Helpers
// ============================================================================

struct ParsedName {
    issue_id: String,
    slug: String,
    agent_type: Option<String>,
}

/// Parse worktree name: gh-{issue_id}-{slug}-{agent_suffix}
fn parse_worktree_name(name: &str) -> Option<ParsedName> {
    let rest = name.strip_prefix("gh-")?;
    let (issue_id, rest) = rest.split_once('-')?;
    let (slug, agent_suffix) = rest.rsplit_once('-')?;

    let agent_type = match agent_suffix {
        "claude" | "gemini" => Some(agent_suffix.to_string()),
        _ => None,
    };

    Some(ParsedName {
        issue_id: issue_id.to_string(),
        slug: slug.to_string(),
        agent_type,
    })
}
