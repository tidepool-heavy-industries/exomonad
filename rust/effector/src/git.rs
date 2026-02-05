use crate::types::{DiffFile, GitDiffResult, GitStatusResult};
use anyhow::{Context, Result};
use duct::cmd;
use std::collections::HashMap;

fn run_git(cwd: &str, args: &[&str]) -> Result<String> {
    cmd("git", args)
        .dir(cwd)
        .read()
        .with_context(|| format!("git {} failed", args.first().unwrap_or(&"<unknown>")))
}

/// Parse git status --porcelain=v2 --branch output into a GitStatusResult.
/// Handles all line types: branch headers, ordinary changes, renames, untracked, and unmerged.
pub fn parse_status(output: &str) -> GitStatusResult {
    let mut branch = "none".to_string();
    let mut ahead = 0;
    let mut behind = 0;
    let mut dirty = Vec::new();
    let mut staged = Vec::new();

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

    GitStatusResult {
        branch,
        dirty,
        staged,
        ahead,
        behind,
    }
}

pub fn status(cwd: &str) -> Result<()> {
    let output = run_git(cwd, &["status", "--porcelain=v2", "--branch"]).unwrap_or_default();
    let result = parse_status(&output);
    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}

/// Parse git diff output (numstat + name-status) into a GitDiffResult.
/// numstat_output: output of `git diff --numstat`
/// name_status_output: output of `git diff --name-status`
pub fn parse_diff(numstat_output: &str, name_status_output: &str) -> GitDiffResult {
    let mut files = Vec::new();
    let mut total_additions = 0;
    let mut total_deletions = 0;

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
            // Binary files show "-" for additions/deletions
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

    GitDiffResult {
        files,
        additions: total_additions,
        deletions: total_deletions,
    }
}

pub fn diff(cwd: &str, staged: bool) -> Result<()> {
    let mut args = vec!["diff", "--numstat"];
    if staged {
        args.push("--staged");
    }
    let numstat_output = run_git(cwd, &args).unwrap_or_default();

    let mut status_args = vec!["diff", "--name-status"];
    if staged {
        status_args.push("--staged");
    }
    let name_status_output = run_git(cwd, &status_args).unwrap_or_default();

    let result = parse_diff(&numstat_output, &name_status_output);
    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}

/// Parse git ls-files output into a list of file paths.
pub fn parse_ls_files(output: &str) -> Vec<String> {
    output.lines().map(|s| s.to_string()).collect()
}

pub fn ls_files(cwd: &str, args: Vec<String>) -> Result<()> {
    let mut git_args = vec!["ls-files"];
    for arg in &args {
        git_args.push(arg);
    }
    let output = run_git(cwd, &git_args)?;
    let files = parse_ls_files(&output);
    println!("{}", serde_json::to_string(&files)?);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    // === Status parsing tests ===

    #[test]
    fn test_status_clean_repo() {
        let output = "# branch.head main\n# branch.ab +0 -0\n";
        let result = parse_status(output);
        assert_eq!(result.branch, "main");
        assert!(result.dirty.is_empty());
        assert!(result.staged.is_empty());
        assert_eq!(result.ahead, 0);
        assert_eq!(result.behind, 0);
    }

    #[test]
    fn test_status_dirty_files() {
        // Ordinary changed file: "1 <XY> ..." where Y != '.'
        let output = "# branch.head main\n1 .M N... 100644 100644 100644 abc123 def456 src/lib.rs\n";
        let result = parse_status(output);
        assert!(result.dirty.contains(&"src/lib.rs".to_string()));
        assert!(result.staged.is_empty());
    }

    #[test]
    fn test_status_staged_files() {
        // Staged file: "1 <XY> ..." where X != '.'
        let output = "# branch.head main\n1 M. N... 100644 100644 100644 abc123 def456 src/lib.rs\n";
        let result = parse_status(output);
        assert!(result.staged.contains(&"src/lib.rs".to_string()));
        assert!(result.dirty.is_empty());
    }

    #[test]
    fn test_status_staged_and_dirty() {
        // Both staged and dirty: "1 MM ..."
        let output = "# branch.head main\n1 MM N... 100644 100644 100644 abc123 def456 src/lib.rs\n";
        let result = parse_status(output);
        assert!(result.staged.contains(&"src/lib.rs".to_string()));
        assert!(result.dirty.contains(&"src/lib.rs".to_string()));
    }

    #[test]
    fn test_status_ahead_behind() {
        let output = "# branch.head feature\n# branch.ab +3 -1\n";
        let result = parse_status(output);
        assert_eq!(result.branch, "feature");
        assert_eq!(result.ahead, 3);
        assert_eq!(result.behind, 1);
    }

    #[test]
    fn test_status_untracked() {
        let output = "# branch.head main\n? untracked.txt\n? another/file.rs\n";
        let result = parse_status(output);
        assert!(result.dirty.contains(&"untracked.txt".to_string()));
        assert!(result.dirty.contains(&"another/file.rs".to_string()));
        assert!(result.staged.is_empty());
    }

    #[test]
    fn test_status_merge_conflict() {
        // Unmerged entry: "u <XY> <sub> <m1> <m2> <m3> <mW> <h1> <h2> <h3> <path>"
        // parts[8] = path in our parsing (starting from 0)
        // 0:u 1:UU 2:N... 3:100644 4:100644 5:100644 6:100644 7:abc 8:conflicted.rs
        let output = "# branch.head main\nu UU N... 100644 100644 100644 100644 abc conflicted.rs\n";
        let result = parse_status(output);
        assert!(result.dirty.contains(&"conflicted.rs".to_string()));
    }

    #[test]
    fn test_status_detached_head() {
        let output = "# branch.head (detached)\n# branch.ab +0 -0\n";
        let result = parse_status(output);
        assert_eq!(result.branch, "(detached)");
    }

    #[test]
    fn test_status_rename() {
        // Rename entry: "2 <XY> <sub> <mH> <mI> <mW> <hH> <hI> <X><score> <path>\t<origPath>"
        let output = "# branch.head main\n2 R. N... 100644 100644 100644 abc123 def456 R100 new.rs\told.rs\n";
        let result = parse_status(output);
        // For type 2 (rename), the path is old.rs (last whitespace-separated token)
        assert!(result.staged.contains(&"old.rs".to_string()));
    }

    #[test]
    fn test_status_empty_output() {
        let output = "";
        let result = parse_status(output);
        assert_eq!(result.branch, "none");
        assert!(result.dirty.is_empty());
        assert!(result.staged.is_empty());
    }

    // === Diff parsing tests ===

    #[test]
    fn test_diff_simple() {
        let numstat = "10\t5\tsrc/lib.rs\n3\t1\tsrc/main.rs\n";
        let name_status = "M\tsrc/lib.rs\nM\tsrc/main.rs\n";
        let result = parse_diff(numstat, name_status);

        assert_eq!(result.files.len(), 2);
        assert_eq!(result.additions, 13);
        assert_eq!(result.deletions, 6);

        let lib = result.files.iter().find(|f| f.path == "src/lib.rs").unwrap();
        assert_eq!(lib.additions, 10);
        assert_eq!(lib.deletions, 5);
        assert_eq!(lib.status, "modified");
    }

    #[test]
    fn test_diff_added_file() {
        let numstat = "50\t0\tnew_file.rs\n";
        let name_status = "A\tnew_file.rs\n";
        let result = parse_diff(numstat, name_status);

        assert_eq!(result.files.len(), 1);
        assert_eq!(result.files[0].status, "added");
        assert_eq!(result.files[0].additions, 50);
        assert_eq!(result.files[0].deletions, 0);
    }

    #[test]
    fn test_diff_deleted_file() {
        let numstat = "0\t100\told_file.rs\n";
        let name_status = "D\told_file.rs\n";
        let result = parse_diff(numstat, name_status);

        assert_eq!(result.files.len(), 1);
        assert_eq!(result.files[0].status, "deleted");
        assert_eq!(result.files[0].additions, 0);
        assert_eq!(result.files[0].deletions, 100);
    }

    #[test]
    fn test_diff_binary() {
        // Binary files show "-" for additions/deletions in numstat
        let numstat = "-\t-\timage.png\n";
        let name_status = "M\timage.png\n";
        let result = parse_diff(numstat, name_status);

        assert_eq!(result.files.len(), 1);
        assert_eq!(result.files[0].additions, 0);
        assert_eq!(result.files[0].deletions, 0);
    }

    #[test]
    fn test_diff_empty() {
        let result = parse_diff("", "");
        assert!(result.files.is_empty());
        assert_eq!(result.additions, 0);
        assert_eq!(result.deletions, 0);
    }

    // === ls-files parsing tests ===

    #[test]
    fn test_ls_files_basic() {
        let output = "src/lib.rs\nsrc/main.rs\nCargo.toml\n";
        let result = parse_ls_files(output);
        assert_eq!(result.len(), 3);
        assert!(result.contains(&"src/lib.rs".to_string()));
        assert!(result.contains(&"src/main.rs".to_string()));
        assert!(result.contains(&"Cargo.toml".to_string()));
    }

    #[test]
    fn test_ls_files_empty() {
        let result = parse_ls_files("");
        assert!(result.is_empty());
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn prop_parse_status_never_panics(input in ".*") {
            // Should never panic regardless of input
            let _ = parse_status(&input);
        }

        #[test]
        fn prop_parse_diff_never_panics(numstat in ".*", name_status in ".*") {
            // Should never panic regardless of input
            let _ = parse_diff(&numstat, &name_status);
        }

        #[test]
        fn prop_parse_ls_files_never_panics(input in ".*") {
            // Should never panic regardless of input
            let _ = parse_ls_files(&input);
        }
    }
}
