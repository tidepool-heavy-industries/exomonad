//! `exo doctor` — health-check + cleanup tool for node-mode workspaces.
//! Audits `.exo/worktrees/` and reclaims stale (merged) ones.

use anyhow::{Context, Result};
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WorktreeStatus {
    /// The current/main worktree. Never reclaimed.
    Current,
    /// Fully merged into the base branch. Safe to reclaim.
    Merged,
    /// Not yet merged into the base branch. Kept unless --include-unmerged.
    Unmerged,
}

impl std::fmt::Display for WorktreeStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WorktreeStatus::Current => write!(f, "CURRENT"),
            WorktreeStatus::Merged => write!(f, "MERGED"),
            WorktreeStatus::Unmerged => write!(f, "UNMERGED"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct WorktreeInfo {
    pub path: PathBuf,
    pub branch: String,
    pub head: String,
    pub status: WorktreeStatus,
}

/// Pure classification logic: given worktree facts, decide what to do.
pub fn classify(path: &Path, root_path: &Path, is_ancestor: bool) -> WorktreeStatus {
    if path == root_path {
        WorktreeStatus::Current
    } else if is_ancestor {
        WorktreeStatus::Merged
    } else {
        WorktreeStatus::Unmerged
    }
}

/// First 8 chars of a sha for display; shorter/empty input (e.g. an unborn HEAD) is returned as-is
/// rather than panicking on a byte-index slice.
fn short_sha(sha: &str) -> &str {
    let end = sha
        .char_indices()
        .nth(8)
        .map(|(i, _)| i)
        .unwrap_or(sha.len());
    &sha[..end]
}

pub async fn run(fix: bool, include_unmerged: bool) -> Result<()> {
    let root_path = get_project_root()?;
    let (base_branch, base_head) = get_base_info()?;

    println!(
        "Auditing .exo/worktrees/ against base branch '{}' ({})",
        base_branch,
        short_sha(&base_head)
    );
    println!("{:-<100}", "");
    println!(
        "{:<40} | {:<25} | {:<10} | {:<8}",
        "PATH", "BRANCH", "STATUS", "HEAD"
    );
    println!("{:-<100}", "");

    let mut worktrees = list_worktrees()?;
    let mut reclaimed_count = 0;
    let mut unmerged_count = 0;

    // Filter to only those under .exo/worktrees/ or the root itself
    worktrees
        .retain(|wt| wt.path == root_path || wt.path.starts_with(root_path.join(".exo/worktrees")));

    for wt in &mut worktrees {
        let is_ancestor = if wt.path == root_path {
            false
        } else {
            check_is_ancestor(&wt.head, &base_head)?
        };

        wt.status = classify(&wt.path, &root_path, is_ancestor);

        let relative_path = wt.path.strip_prefix(&root_path).unwrap_or(&wt.path);
        println!(
            "{:<40} | {:<25} | {:<10} | {:<8}",
            relative_path.display(),
            wt.branch,
            wt.status,
            short_sha(&wt.head)
        );

        if wt.status == WorktreeStatus::Merged {
            reclaimed_count += 1;
        } else if wt.status == WorktreeStatus::Unmerged {
            unmerged_count += 1;
        }
    }

    println!("{:-<100}", "");
    if reclaimed_count > 0 {
        println!("{} merged worktrees are reclaimable.", reclaimed_count);
    }
    if unmerged_count > 0 {
        println!("{} unmerged worktrees detected (skipped).", unmerged_count);
    }

    if !fix {
        if reclaimed_count > 0 {
            println!("\nRun 'exo doctor --fix' to reclaim merged worktrees.");
        } else {
            println!("\nEverything looks healthy. No merged worktrees to reclaim.");
        }
        return Ok(());
    }

    // Actually fix
    println!("\nReclaiming merged worktrees...");

    // Sort by depth DESC so nested children are removed before parents
    worktrees.sort_by(|a, b| {
        let da = a.path.components().count();
        let db = b.path.components().count();
        db.cmp(&da)
    });

    for wt in worktrees {
        let should_remove = match wt.status {
            WorktreeStatus::Merged => true,
            WorktreeStatus::Unmerged if include_unmerged => {
                println!(
                    "WARNING: Reclaiming UNMERGED worktree '{}' as requested.",
                    wt.branch
                );
                true
            }
            _ => false,
        };

        if should_remove {
            remove_worktree(&wt.path, &wt.branch)?;
        }
    }

    // Final prune
    match Command::new("git").args(["worktree", "prune"]).status() {
        Ok(status) if !status.success() => {
            eprintln!("    FAILED to prune worktrees (exit {status})");
        }
        Err(e) => eprintln!("    FAILED to run git worktree prune: {e}"),
        Ok(_) => {}
    }

    println!("\nCleanup complete.");
    Ok(())
}

fn get_project_root() -> Result<PathBuf> {
    let output = Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .output()
        .context("running git rev-parse --show-toplevel")?;

    if !output.status.success() {
        anyhow::bail!(
            "git rev-parse failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(PathBuf::from(
        String::from_utf8_lossy(&output.stdout).trim(),
    ))
}

fn get_base_info() -> Result<(String, String)> {
    // Branch name
    let branch_out = Command::new("git")
        .args(["rev-parse", "--abbrev-ref", "HEAD"])
        .output()
        .context("getting current branch name")?;
    if !branch_out.status.success() {
        anyhow::bail!(
            "git rev-parse --abbrev-ref HEAD failed: {}",
            String::from_utf8_lossy(&branch_out.stderr)
        );
    }
    let branch = String::from_utf8_lossy(&branch_out.stdout)
        .trim()
        .to_string();

    // HEAD sha
    let head_out = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()
        .context("getting current branch HEAD")?;
    if !head_out.status.success() {
        anyhow::bail!(
            "git rev-parse HEAD failed: {}",
            String::from_utf8_lossy(&head_out.stderr)
        );
    }
    let head = String::from_utf8_lossy(&head_out.stdout).trim().to_string();

    Ok((branch, head))
}

fn list_worktrees() -> Result<Vec<WorktreeInfo>> {
    let output = Command::new("git")
        .args(["worktree", "list", "--porcelain"])
        .output()
        .context("listing worktrees")?;

    if !output.status.success() {
        anyhow::bail!(
            "git worktree list failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut worktrees = Vec::new();
    let mut current_path = None;
    let mut current_head = None;
    let mut current_branch = None;

    for line in stdout.lines() {
        if line.is_empty() {
            if let (Some(path), Some(head)) = (current_path.take(), current_head.take()) {
                let branch = current_branch
                    .take()
                    .unwrap_or_else(|| "detached".to_string());
                worktrees.push(WorktreeInfo {
                    path,
                    branch,
                    head,
                    status: WorktreeStatus::Unmerged, // Default
                });
            }
            continue;
        }

        let mut parts = line.splitn(2, ' ');
        let key = parts.next().unwrap_or("");
        let val = parts.next().unwrap_or("");

        match key {
            "worktree" => current_path = Some(PathBuf::from(val)),
            "HEAD" => current_head = Some(val.to_string()),
            "branch" => {
                let branch_name = val.strip_prefix("refs/heads/").unwrap_or(val);
                current_branch = Some(branch_name.to_string());
            }
            _ => {}
        }
    }

    // Catch the last one if it didn't end with a newline
    if let (Some(path), Some(head)) = (current_path, current_head) {
        let branch = current_branch.unwrap_or_else(|| "detached".to_string());
        worktrees.push(WorktreeInfo {
            path,
            branch,
            head,
            status: WorktreeStatus::Unmerged,
        });
    }

    Ok(worktrees)
}

fn check_is_ancestor(head: &str, base: &str) -> Result<bool> {
    let status = Command::new("git")
        .args(["merge-base", "--is-ancestor", head, base])
        .status()
        .context("checking merge-base")?;

    Ok(status.success())
}

fn remove_worktree(path: &Path, branch: &str) -> Result<()> {
    println!("  Removing worktree: {}", path.display());
    let status = Command::new("git")
        .args(["worktree", "remove", "--force", &path.to_string_lossy()])
        .status()
        .context("removing worktree")?;

    if !status.success() {
        eprintln!("    FAILED to remove worktree at {}", path.display());
    }

    if branch != "detached" && branch != "main" && branch != "master" {
        println!("  Deleting branch: {}", branch);
        match Command::new("git").args(["branch", "-D", branch]).status() {
            Ok(status) if !status.success() => {
                eprintln!("    FAILED to delete branch {branch} (exit {status})");
            }
            Err(e) => eprintln!("    FAILED to run git branch -D {branch}: {e}"),
            Ok(_) => {}
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_classify() {
        let root = Path::new("/repo");
        let wt_root = Path::new("/repo");
        let wt_merged = Path::new("/repo/.exo/worktrees/a");
        let wt_unmerged = Path::new("/repo/.exo/worktrees/b");

        assert_eq!(classify(wt_root, root, false), WorktreeStatus::Current);
        assert_eq!(classify(wt_root, root, true), WorktreeStatus::Current);
        assert_eq!(classify(wt_merged, root, true), WorktreeStatus::Merged);
        assert_eq!(classify(wt_unmerged, root, false), WorktreeStatus::Unmerged);
    }
}
