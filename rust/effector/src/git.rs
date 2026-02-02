use crate::types::{DiffFile, GitDiffResult, GitStatusResult};
use anyhow::{anyhow, Result};
use std::collections::HashMap;
use std::process::Command;

fn run_git(cwd: &str, args: &[&str]) -> Result<String> {
    let output = Command::new("git").current_dir(cwd).args(args).output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(anyhow!("git command failed: {}", stderr));
    }

    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

pub fn status(cwd: &str) -> Result<()> {
    let output_res = run_git(cwd, &["status", "--porcelain=v2", "--branch"]);

    let mut branch = "none".to_string();
    let mut ahead = 0;
    let mut behind = 0;
    let mut dirty = Vec::new();
    let mut staged = Vec::new();

    if let Ok(output) = output_res {
        for line in output.lines() {
            if let Some(stripped) = line.strip_prefix("# branch.head ") {
                branch = stripped.to_string();
            } else if let Some(stripped) = line.strip_prefix("# branch.ab ") {
                let parts: Vec<&str> = stripped.split_whitespace().collect();
                if parts.len() >= 2 {
                    ahead = parts[0].trim_start_matches('+').parse().unwrap_or(0);
                    behind = parts[1].trim_start_matches('-').parse().unwrap_or(0);
                }
            } else if line.starts_with("1 ") || line.starts_with("2 ") {
                // 1 <XY> ... <path>
                // 2 <XY> ... <path> <orig_path>
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 2 {
                    let xy = parts[1];
                    let x = xy.chars().next().unwrap_or('.');
                    let y = xy.chars().nth(1).unwrap_or('.');
                    let path = parts[parts.len() - 1].to_string();

                    if x != '.' {
                        staged.push(path.clone());
                    }
                    if y != '.' {
                        dirty.push(path);
                    }
                }
            } else if let Some(stripped) = line.strip_prefix("? ") {
                let path = stripped.to_string();
                dirty.push(path);
            } else if line.starts_with("u ") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 9 {
                    let path = parts[8].to_string();
                    dirty.push(path);
                }
            }
        }
    }

    let result = GitStatusResult {
        branch,
        dirty,
        staged,
        ahead,
        behind,
    };
    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}

pub fn diff(cwd: &str, staged: bool) -> Result<()> {
    let mut args = vec!["diff", "--numstat"];
    if staged {
        args.push("--staged");
    }
    let numstat_output_res = run_git(cwd, &args);

    let mut status_args = vec!["diff", "--name-status"];
    if staged {
        status_args.push("--staged");
    }
    let name_status_output_res = run_git(cwd, &status_args);

    let mut files = Vec::new();
    let mut total_additions = 0;
    let mut total_deletions = 0;

    if let (Ok(numstat_output), Ok(name_status_output)) =
        (numstat_output_res, name_status_output_res)
    {
        let mut statuses = HashMap::new();
        for line in name_status_output.lines() {
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() >= 2 {
                let status_code = parts[0].chars().next().unwrap_or('M');
                let path = parts[parts.len() - 1];
                let status_str = match status_code {
                    'A' => "added",
                    'D' => "deleted",
                    _ => "modified",
                };
                statuses.insert(path.to_string(), status_str.to_string());
            }
        }

        for line in numstat_output.lines() {
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() >= 3 {
                let additions: u32 = parts[0].parse().unwrap_or(0);
                let deletions: u32 = parts[1].parse().unwrap_or(0);
                let path = parts[2].to_string();

                let status = statuses
                    .get(&path)
                    .cloned()
                    .unwrap_or_else(|| "modified".to_string());

                total_additions += additions;
                total_deletions += deletions;

                files.push(DiffFile {
                    path,
                    status,
                    additions,
                    deletions,
                });
            }
        }
    }

    let result = GitDiffResult {
        files,
        additions: total_additions,
        deletions: total_deletions,
    };
    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}

pub fn ls_files(cwd: &str, args: Vec<String>) -> Result<()> {
    let mut git_args = vec!["ls-files"];
    for arg in &args {
        git_args.push(arg);
    }
    let output = run_git(cwd, &git_args)?;
    let files: Vec<String> = output.lines().map(|s| s.to_string()).collect();
    println!("{}", serde_json::to_string(&files)?);
    Ok(())
}
