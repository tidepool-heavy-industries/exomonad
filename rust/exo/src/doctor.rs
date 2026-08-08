//! `exo doctor` — health-check + cleanup tool for node-mode workspaces.
//! Audits `.exo/worktrees/` and reclaims stale (merged) ones.
//!
//! `--fix` removal delegates the actual nested-worktree walk (discover → kill each nested
//! child's recorded pane → `git worktree remove` innermost-first) to
//! [`exo_runtime::Runtime::reclaim_worktree_tree`] — the SAME code path `Spawner::reclaim_worktree`
//! uses at merge-time. There is exactly one implementation of that walk; doctor only decides
//! *which* worktrees are reclaimable and reports outcomes.

use anyhow::{Context, Result};
use exo_caps::{
    fold_children, AgentName, Branch, ChildKind, ChildRecord, ChildState, NodePath, PaneId,
};
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
    // Built once, used by both the --fix reclaim loop and the (dry-run-safe) acknowledgment pass
    // below — construction is a pure struct literal, no IO (see `doctor_runtime`'s doc comment).
    let rt = doctor_runtime(&root_path);

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

    // Acknowledgment pass: a `Died` child with no worktree directory left on disk has nothing
    // left to reclaim — recording `Reaped` for it IS the acknowledgment (the ledger fold
    // self-heals `Died` -> `Reaped`, the same transition an ordinary reclaim already produces).
    // Runs in BOTH dry-run and --fix, so a plain `exo doctor` previews what --fix would do;
    // dry-run records nothing. Only reaches the root's own ledger (see `read_root_child_records`).
    let root_children = fold_children(&read_root_child_records(&root_path));
    let worktrees_root = root_path.join(".exo/worktrees");
    let mut acknowledged_count = 0;
    for (name, child) in &root_children {
        let worktree_exists = worktrees_root.join(name.as_str()).exists();
        if !should_acknowledge(&child.state, worktree_exists) {
            continue;
        }
        acknowledged_count += 1;
        if fix {
            if record_acknowledged(&rt, name).await {
                println!(
                    "  Acknowledged dead child with no worktree left: {}",
                    name.as_str()
                );
            }
        } else {
            println!(
                "  Would acknowledge dead child with no worktree left: {}",
                name.as_str()
            );
        }
    }
    if acknowledged_count > 0 {
        println!(
            "{} dead children with no worktree left {}.",
            acknowledged_count,
            if fix {
                "acknowledged"
            } else {
                "would be acknowledged"
            }
        );
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

    // Sort by depth ASC so an outer worktree is visited (and reclaimed, nested subtree and
    // all) before any of its nested children are considered.
    worktrees.sort_by_key(|wt| wt.path.components().count());

    let mut reclaimed_paths: Vec<PathBuf> = vec![];
    let mut reclaimed_reaped_count = 0;

    for wt in &worktrees {
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

        if !should_remove {
            continue;
        }

        // Already swallowed by an outer worktree's reclaim (its own nested-worktree walk) —
        // don't attempt a second, redundant `git worktree remove` on a dir that's already gone.
        if reclaimed_paths
            .iter()
            .any(|r| wt.path != *r && wt.path.starts_with(r))
        {
            println!(
                "  {} was reclaimed as part of its enclosing worktree",
                wt.path.display()
            );
            continue;
        }

        println!("  Removing worktree: {}", wt.path.display());
        let name = wt
            .path
            .file_name()
            .unwrap_or_default()
            .to_string_lossy()
            .to_string();
        match AgentName::new(name) {
            Ok(agent_name) => match rt.reclaim_worktree_tree(&agent_name, &wt.path).await {
                Ok(()) => {
                    reclaimed_paths.push(wt.path.clone());
                    // Direct append, not `record_reaped_if_active`: a doctor-reclaimed child is
                    // usually already `Died` (terminal), which that helper's guard skips. The
                    // fold's later-record-wins rule makes the extra `Reaped` the acknowledgment.
                    if record_acknowledged(&rt, &agent_name).await {
                        reclaimed_reaped_count += 1;
                    }
                }
                Err(e) => eprintln!(
                    "    FAILED to remove worktree at {}: {e}",
                    wt.path.display()
                ),
            },
            Err(e) => eprintln!(
                "    FAILED to remove worktree at {}: invalid agent name: {e}",
                wt.path.display()
            ),
        }
    }

    if reclaimed_reaped_count > 0 {
        println!(
            "{} reclaimed worktrees recorded as Reaped.",
            reclaimed_reaped_count
        );
    }

    // Branch cleanup for everything actually reclaimed above (root or swallowed-nested).
    for wt in &worktrees {
        if reclaimed_paths.iter().any(|r| wt.path.starts_with(r)) {
            delete_branch(&wt.branch);
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

/// Read + tolerantly parse the project root's own child ledger (`.exo/children.jsonl`) directly
/// off disk. Doctor has no `Fs` cap and `exo_runtime::Runtime::read_child_records` is crate-private
/// (ledger reads are not part of the tool-facing cap surface), so this reads the plain file itself
/// — mirroring `exo_runtime`'s own tolerant-parse discipline (a malformed line is skipped and
/// logged, never fatal to the rest of the ledger). A missing ledger means no children yet.
fn read_root_child_records(root_path: &Path) -> Vec<ChildRecord> {
    let path = root_path.join(".exo/children.jsonl");
    let data = match std::fs::read(&path) {
        Ok(d) => d,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Vec::new(),
        Err(e) => {
            eprintln!("    FAILED to read {}: {e}", path.display());
            return Vec::new();
        }
    };
    let mut records = Vec::new();
    for line in data.split(|&b| b == b'\n') {
        if line.is_empty() {
            continue;
        }
        match serde_json::from_slice::<ChildRecord>(line) {
            Ok(r) => records.push(r),
            Err(e) => eprintln!(
                "    skipping malformed children.jsonl line at {}: {e}",
                path.display()
            ),
        }
    }
    records
}

/// Pure decision for the acknowledgment pass: a `Died` child with no worktree directory left on
/// disk has nothing left to reclaim — recording `Reaped` for it IS the acknowledgment (the ledger
/// fold self-heals `Died` -> `Reaped`). A `Died` child whose worktree still exists is left alone
/// here — it's only acknowledged via an actual reclaim, above.
fn should_acknowledge(state: &ChildState, worktree_exists: bool) -> bool {
    matches!(state, ChildState::Died) && !worktree_exists
}

fn check_is_ancestor(head: &str, base: &str) -> Result<bool> {
    let status = Command::new("git")
        .args(["merge-base", "--is-ancestor", head, base])
        .status()
        .context("checking merge-base")?;

    Ok(status.success())
}

/// Construct a minimal root-rooted [`exo_runtime::Runtime`] purely to reach
/// [`exo_runtime::Runtime::reclaim_worktree_tree`] — doctor has no birth identity of its own (it's
/// a foreground CLI, not a spawned node), so every field beyond `working_dir` is a
/// behavior-preserving placeholder: `reclaim_worktree_tree` never reads `node_path`/`branch`/
/// `run_id`/`own_pane`, and `tmux_session` is unused by `Tmux::kill_pane` (it targets a `%N` pane
/// id directly, not a session-qualified path).
fn doctor_runtime(root_path: &Path) -> exo_runtime::Runtime {
    exo_runtime::Runtime::new(
        NodePath::new(vec![AgentName::new("doctor".into()).expect("static name")])
            .expect("non-empty"),
        Branch::new("doctor".into()).expect("static name is a valid ref"),
        root_path.to_path_buf(),
        None,
        "doctor".into(),
        std::env::var("EXOMONAD_TMUX_SESSION").unwrap_or_default(),
        PaneId::new("%0".into()).expect("static pane id"),
        ChildKind::Worktree,
    )
}

/// Record `Reaped` for `child` by direct ledger append — the acknowledgment transition.
/// Doctor already knows the child's folded state (`Died`, or just physically reclaimed), so it
/// must NOT go through `record_reaped_if_active`: that helper's not-yet-terminal guard exists
/// for the runtime teardown path and refuses exactly the `Died -> Reaped` transition doctor
/// performs. Returns whether the record actually landed — callers gate their success print on it.
async fn record_acknowledged(rt: &exo_runtime::Runtime, child: &AgentName) -> bool {
    let rec = ChildRecord::Reaped {
        child: child.clone(),
        at: None,
    };
    match rt.append_child_record(&rec).await {
        Ok(()) => true,
        Err(e) => {
            eprintln!(
                "    FAILED to record Reaped for {}: {e} — tombstone stays Died; re-run doctor --fix",
                child.as_str()
            );
            false
        }
    }
}

fn delete_branch(branch: &str) {
    if branch == "detached" || branch == "main" || branch == "master" {
        return;
    }
    println!("  Deleting branch: {}", branch);
    match Command::new("git").args(["branch", "-D", branch]).status() {
        Ok(status) if !status.success() => {
            eprintln!("    FAILED to delete branch {branch} (exit {status})");
        }
        Err(e) => eprintln!("    FAILED to run git branch -D {branch}: {e}"),
        Ok(_) => {}
    }
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

    #[test]
    fn should_acknowledge_dead_with_no_worktree() {
        assert!(should_acknowledge(&ChildState::Died, false));
    }

    #[test]
    fn should_not_acknowledge_dead_with_worktree_present() {
        // Still reclaimable via the normal worktree-removal path — not doctor's acknowledgment
        // pass, which only fires once there is nothing left to reclaim.
        assert!(!should_acknowledge(&ChildState::Died, true));
    }

    #[test]
    fn should_not_acknowledge_live_or_already_terminal() {
        assert!(!should_acknowledge(&ChildState::Live, false));
        assert!(!should_acknowledge(&ChildState::Reaped, false));
        assert!(!should_acknowledge(
            &ChildState::Submitted {
                sha: "deadbeef".into(),
                reviewed: false
            },
            false
        ));
    }

    #[tokio::test]
    async fn record_acknowledged_persists_over_died_state() {
        // The transition doctor exists to perform: Died -> Reaped must actually LAND in the
        // ledger (record_reaped_if_active's not-yet-terminal guard would silently skip it —
        // the bug this test pins).
        let dir = std::env::temp_dir().join(format!(
            "exo-doctor-test-ack-{}-{}",
            std::process::id(),
            line!()
        ));
        std::fs::create_dir_all(dir.join(".exo")).unwrap();
        std::fs::write(
            dir.join(".exo/children.jsonl"),
            concat!(
                r#"{"record":"spawned","child":"x","kind":"worktree","pane":"%9","inbox":"/tmp/x.jsonl","model_label":null}"#,
                "\n",
                r#"{"record":"died","child":"x","pane":"%9"}"#,
                "\n"
            ),
        )
        .unwrap();

        let rt = doctor_runtime(&dir);
        let child = AgentName::new("x".into()).unwrap();
        assert!(record_acknowledged(&rt, &child).await);

        let folded = fold_children(&read_root_child_records(&dir));
        assert_eq!(folded.get(&child).unwrap().state, ChildState::Reaped);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn read_root_child_records_missing_ledger_is_empty() {
        let dir = std::env::temp_dir().join(format!(
            "exo-doctor-test-missing-{}-{}",
            std::process::id(),
            line!()
        ));
        assert!(read_root_child_records(&dir).is_empty());
    }

    #[test]
    fn read_root_child_records_parses_and_skips_malformed_lines() {
        let dir = std::env::temp_dir().join(format!(
            "exo-doctor-test-parse-{}-{}",
            std::process::id(),
            line!()
        ));
        std::fs::create_dir_all(dir.join(".exo")).unwrap();
        let record = ChildRecord::Died {
            child: AgentName::new("leaf-1".into()).unwrap(),
            pane: PaneId::new("%9".into()).unwrap(),
            at: None,
        };
        let mut contents = serde_json::to_string(&record).unwrap();
        contents.push('\n');
        contents.push_str("not valid json\n");
        std::fs::write(dir.join(".exo/children.jsonl"), contents).unwrap();

        let records = read_root_child_records(&dir);
        assert_eq!(records, vec![record]);

        std::fs::remove_dir_all(&dir).ok();
    }
}
