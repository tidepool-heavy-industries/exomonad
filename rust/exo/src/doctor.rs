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
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, SystemTime};

/// How long a run's home-dir state (`~/.claude/exo/{inboxes,status,papers}/{run_id}/`) may go
/// without a fresh mtime before `exo doctor` calls it dead. The sidecar's status publisher writes
/// a fresh status file every 5s while a node is alive (`exo_runtime::Runtime::status_snapshot`'s
/// periodic caller in `exo-node`'s `run_node`), so hours of silence means the run ended, not that
/// it's merely idle between turns.
const STALE_RUN_THRESHOLD: Duration = Duration::from_secs(6 * 60 * 60);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WorktreeStatus {
    /// The current/main worktree. Never reclaimed.
    Current,
    /// Fully merged into the base branch. Safe to reclaim.
    Merged,
    /// Not yet merged into the base branch. Kept unless --include-unmerged.
    Unmerged,
    /// A working agent per the children ledger (non-terminal state). NEVER reclaimed by doctor.
    Live,
}

impl std::fmt::Display for WorktreeStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WorktreeStatus::Current => write!(f, "CURRENT"),
            WorktreeStatus::Merged => write!(f, "MERGED"),
            WorktreeStatus::Unmerged => write!(f, "UNMERGED"),
            WorktreeStatus::Live => write!(f, "LIVE"),
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
///
/// `is_live` comes from the children ledger fold: a child whose state is non-terminal
/// (Live/Submitted) is a WORKING AGENT regardless of git ancestry — a freshly-spawned child's
/// branch sits at the fork point with no commits yet, so the ancestor check alone reads it as
/// "merged" and `--fix` would destroy a live agent's worktree. Live wins over everything except
/// Current, and is never reclaimable (not even with `--include-unmerged` — tearing down a live
/// agent is `merge`/`dismiss_worker`/shutdown's job, not doctor's).
pub fn classify(path: &Path, root_path: &Path, is_ancestor: bool, is_live: bool) -> WorktreeStatus {
    if path == root_path {
        WorktreeStatus::Current
    } else if is_live {
        WorktreeStatus::Live
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
    let mut live_count = 0;

    // The ledger fold is the live-agent authority: ancestry alone misreads a freshly-spawned
    // child (branch at fork point, no commits) as "merged". Folded once, reused by the
    // acknowledgment pass below.
    let root_children = fold_children(&read_root_child_records(&root_path));
    let live_names: BTreeSet<&str> = root_children
        .iter()
        .filter(|(_, c)| !c.state.is_terminal())
        .map(|(n, _)| n.as_str())
        .collect();

    // Filter to only those under .exo/worktrees/ or the root itself
    worktrees
        .retain(|wt| wt.path == root_path || wt.path.starts_with(root_path.join(".exo/worktrees")));

    for wt in &mut worktrees {
        let is_ancestor = if wt.path == root_path {
            false
        } else {
            check_is_ancestor(&wt.head, &base_head)?
        };

        let is_live = wt
            .path
            .file_name()
            .map(|n| live_names.contains(n.to_string_lossy().as_ref()))
            .unwrap_or(false);
        wt.status = classify(&wt.path, &root_path, is_ancestor, is_live);

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
        } else if wt.status == WorktreeStatus::Live {
            live_count += 1;
        }
    }

    println!("{:-<100}", "");
    if reclaimed_count > 0 {
        println!("{} merged worktrees are reclaimable.", reclaimed_count);
    }
    if unmerged_count > 0 {
        println!("{} unmerged worktrees detected (skipped).", unmerged_count);
    }
    if live_count > 0 {
        println!(
            "{} LIVE children (working agents per the ledger) — never reclaimed by doctor.",
            live_count
        );
    }

    // Acknowledgment pass: a `Died` child with no worktree directory left on disk has nothing
    // left to reclaim — recording `Reaped` for it IS the acknowledgment (the ledger fold
    // self-heals `Died` -> `Reaped`, the same transition an ordinary reclaim already produces).
    // Runs in BOTH dry-run and --fix, so a plain `exo doctor` previews what --fix would do;
    // dry-run records nothing. Only reaches the root's own ledger (see `read_root_child_records`).
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

    // Run-artifact GC: home-dir run state and repo-local tmux-paste spill files accumulate
    // forever (nothing else deletes them — see `exo-node/src/inbound.rs`'s spill-file comment and
    // `exo-node/src/dispatch.rs`'s `.exo/tmp/inbox-{pid}-*.md` writer). Runs in BOTH dry-run and
    // --fix, same as the acknowledgment pass above.
    println!();
    let home = home_dir();
    let current_run_id = std::env::var("EXOMONAD_SWARM_RUN_ID").ok();
    let now = SystemTime::now();
    println!("Run-artifact GC ({}):", home.join(".claude/exo").display());
    let dead_runs = classify_dead_runs(&home, current_run_id.as_deref(), now);
    if dead_runs.is_empty() {
        println!("  No dead runs found.");
    } else {
        let mut freed = 0u64;
        for info in &dead_runs {
            let age = info
                .newest_mtime
                .and_then(|t| now.duration_since(t).ok())
                .map(|d| format!("{}h old", d.as_secs() / 3600))
                .unwrap_or_else(|| "no files found".to_string());
            println!(
                "  {} — {} dir(s), {} bytes, {}",
                info.id,
                info.dirs.len(),
                info.total_size(),
                age
            );
            if fix {
                for (d, sz) in &info.dirs {
                    match std::fs::remove_dir_all(d) {
                        Ok(()) => freed += sz,
                        Err(e) => eprintln!("    FAILED to remove {}: {e}", d.display()),
                    }
                }
            }
        }
        if fix {
            println!(
                "{} dead run(s) removed, {} bytes freed.",
                dead_runs.len(),
                freed
            );
        } else {
            let total: u64 = dead_runs.iter().map(RunGcInfo::total_size).sum();
            println!(
                "{} dead run(s), {} bytes would be freed.",
                dead_runs.len(),
                total
            );
        }
    }

    let spill_dir = root_path.join(".exo/tmp");
    println!("Spill files ({}):", spill_dir.display());
    let dead_spills = classify_dead_spill_files(&spill_dir);
    if dead_spills.is_empty() {
        println!("  No dead spill files found.");
    } else {
        let mut freed = 0u64;
        for f in &dead_spills {
            println!("  {} — {} bytes", f.path.display(), f.size);
            if fix {
                match std::fs::remove_file(&f.path) {
                    Ok(()) => freed += f.size,
                    Err(e) => eprintln!("    FAILED to remove {}: {e}", f.path.display()),
                }
            }
        }
        if fix {
            println!(
                "{} dead spill file(s) removed, {} bytes freed.",
                dead_spills.len(),
                freed
            );
        } else {
            let total: u64 = dead_spills.iter().map(|f| f.size).sum();
            println!(
                "{} dead spill file(s), {} bytes would be freed.",
                dead_spills.len(),
                total
            );
        }
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

/// `$HOME`, defaulting to `.` on an unset environment — mirrors `exo_runtime::runtime::home()`'s
/// fallback so a misconfigured environment degrades to "nothing found under `.`" rather than
/// panicking.
fn home_dir() -> PathBuf {
    std::env::var("HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("."))
}

/// One run id's home-dir footprint: the existing dirs among `{inboxes,status,papers}/{run_id}`,
/// each paired with its total size, plus the mtime that decides liveness (see [`run_gc_info`]).
struct RunGcInfo {
    id: String,
    dirs: Vec<(PathBuf, u64)>,
    newest_mtime: Option<SystemTime>,
}

impl RunGcInfo {
    fn total_size(&self) -> u64 {
        self.dirs.iter().map(|(_, size)| size).sum()
    }
}

/// Pure classification: is a run dead? The current run is never dead, regardless of `newest_mtime`
/// — this is the absolute floor the rest of the pass builds on. `newest_mtime` is `None` only when
/// the run's directories exist but hold no files at all, which is classified dead the same as any
/// other silence (there is no heartbeat to point to).
fn run_is_dead(newest_mtime: Option<SystemTime>, now: SystemTime, is_current: bool) -> bool {
    if is_current {
        return false;
    }
    match newest_mtime {
        None => true,
        Some(t) => now.duration_since(t).unwrap_or(Duration::ZERO) > STALE_RUN_THRESHOLD,
    }
}

/// Recursively walk `dir`, returning its total file size and the newest mtime seen among its
/// files. Unreadable entries are skipped, not fatal — discovery here is best-effort, matching the
/// rest of doctor's read side; only a --fix removal is loud on failure.
fn walk_dir_stats(dir: &Path) -> (u64, Option<SystemTime>) {
    let mut total = 0u64;
    let mut newest: Option<SystemTime> = None;
    let mut stack = vec![dir.to_path_buf()];
    while let Some(d) = stack.pop() {
        let entries = match std::fs::read_dir(&d) {
            Ok(e) => e,
            Err(_) => continue,
        };
        for entry in entries.flatten() {
            let Ok(meta) = entry.metadata() else {
                continue;
            };
            if meta.is_dir() {
                stack.push(entry.path());
                continue;
            }
            total += meta.len();
            if let Ok(mtime) = meta.modified() {
                newest = Some(newest.map_or(mtime, |n: SystemTime| n.max(mtime)));
            }
        }
    }
    (total, newest)
}

/// Every run id appearing as a subdirectory of `{home}/.claude/exo/{inboxes,status,papers}`.
fn discover_run_ids(home: &Path) -> BTreeSet<String> {
    let base = home.join(".claude/exo");
    ["inboxes", "status", "papers"]
        .iter()
        .flat_map(|sub| std::fs::read_dir(base.join(sub)).into_iter().flatten())
        .flatten()
        .filter(|entry| entry.file_type().map(|t| t.is_dir()).unwrap_or(false))
        .filter_map(|entry| entry.file_name().to_str().map(str::to_string))
        .collect()
}

/// Gather one run id's footprint. Liveness is anchored to the `status/` dir's own mtimes when that
/// dir exists (the sidecar's 5s heartbeat lives there); a run with no status dir at all (dead
/// before ever heartbeating, or from a build that predates status publishing) falls back to the
/// newest mtime anywhere under its `inboxes`/`papers` dirs — see the doctor.rs module boundary in
/// the spec this implements.
fn run_gc_info(home: &Path, run_id: &str) -> RunGcInfo {
    let base = home.join(".claude/exo");
    let status_dir = base.join("status").join(run_id);
    let inbox_dir = base.join("inboxes").join(run_id);
    let papers_dir = base.join("papers").join(run_id);
    let has_status = status_dir.exists();

    let mut dirs = Vec::new();
    let mut newest_mtime: Option<SystemTime> = None;
    for d in [&status_dir, &inbox_dir, &papers_dir] {
        if !d.exists() {
            continue;
        }
        let (size, mtime) = walk_dir_stats(d);
        dirs.push((d.clone(), size));
        if !has_status || d == &status_dir {
            newest_mtime = match (newest_mtime, mtime) {
                (Some(a), Some(b)) => Some(a.max(b)),
                (a, None) => a,
                (None, b) => b,
            };
        }
    }

    RunGcInfo {
        id: run_id.to_string(),
        dirs,
        newest_mtime,
    }
}

/// Every dead run under `home`, per [`run_is_dead`]. Live runs (including the current one) are
/// omitted entirely — nothing about them is reported or touched.
fn classify_dead_runs(
    home: &Path,
    current_run_id: Option<&str>,
    now: SystemTime,
) -> Vec<RunGcInfo> {
    discover_run_ids(home)
        .into_iter()
        .map(|id| run_gc_info(home, &id))
        .filter(|info| {
            let is_current = current_run_id == Some(info.id.as_str());
            run_is_dead(info.newest_mtime, now, is_current)
        })
        .collect()
}

/// One dead repo-local tmux-paste spill file.
struct DeadSpillFile {
    path: PathBuf,
    size: u64,
}

/// Parse `pid` out of a spill filename of the form `inbox-{pid}-{id}.md` (written by
/// `exo-node`'s `dispatch::prepare_tmux_payload`). Anything that doesn't match this exact shape —
/// including a non-numeric pid — returns `None`, so a malformed or unrelated filename is left
/// alone rather than guessed at.
fn spill_pid_from_name(name: &str) -> Option<u32> {
    let rest = name.strip_prefix("inbox-")?.strip_suffix(".md")?;
    let (pid_str, _id) = rest.split_once('-')?;
    pid_str.parse().ok()
}

/// Is `pid` a live process? Linux-only (`/proc`), matching the rest of this codebase's process
/// probing (e.g. `exo-scry`'s `/proc`-based process-tree walk).
fn pid_alive(pid: u32) -> bool {
    Path::new(&format!("/proc/{pid}")).exists()
}

/// Every dead spill file directly inside `tmp_dir` (non-recursive — spill files are written flat
/// into `.exo/tmp/`). A missing `tmp_dir` yields no dead files, not an error.
fn classify_dead_spill_files(tmp_dir: &Path) -> Vec<DeadSpillFile> {
    let Ok(entries) = std::fs::read_dir(tmp_dir) else {
        return Vec::new();
    };
    entries
        .flatten()
        .filter_map(|entry| {
            let name = entry.file_name();
            let pid = spill_pid_from_name(name.to_str()?)?;
            if pid_alive(pid) {
                return None;
            }
            let size = entry.metadata().ok()?.len();
            Some(DeadSpillFile {
                path: entry.path(),
                size,
            })
        })
        .collect()
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

        assert_eq!(
            classify(wt_root, root, false, false),
            WorktreeStatus::Current
        );
        assert_eq!(classify(wt_root, root, true, false), WorktreeStatus::Current);
        assert_eq!(
            classify(wt_merged, root, true, false),
            WorktreeStatus::Merged
        );
        assert_eq!(
            classify(wt_unmerged, root, false, false),
            WorktreeStatus::Unmerged
        );
        // A live child (per the ledger) is NEVER reclaimable, even when its branch sits at the
        // fork point and ancestry alone would read it as merged — the bug this arm pins.
        assert_eq!(classify(wt_merged, root, true, true), WorktreeStatus::Live);
        assert_eq!(
            classify(wt_unmerged, root, false, true),
            WorktreeStatus::Live
        );
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

    fn secs_ago(now: SystemTime, secs: u64) -> SystemTime {
        now - Duration::from_secs(secs)
    }

    #[test]
    fn run_is_dead_current_run_never_dead() {
        let now = SystemTime::now();
        // Even wildly stale, the current run is exempt.
        assert!(!run_is_dead(Some(secs_ago(now, 999_999)), now, true));
        assert!(!run_is_dead(None, now, true));
    }

    #[test]
    fn run_is_dead_threshold_boundary() {
        let now = SystemTime::now();
        assert!(!run_is_dead(
            Some(secs_ago(now, STALE_RUN_THRESHOLD.as_secs() - 1)),
            now,
            false
        ));
        assert!(run_is_dead(
            Some(secs_ago(now, STALE_RUN_THRESHOLD.as_secs() + 1)),
            now,
            false
        ));
    }

    #[test]
    fn run_is_dead_no_files_is_dead_unless_current() {
        let now = SystemTime::now();
        assert!(run_is_dead(None, now, false));
        assert!(!run_is_dead(None, now, true));
    }

    #[test]
    fn spill_pid_from_name_parses_well_formed() {
        assert_eq!(spill_pid_from_name("inbox-1234-5.md"), Some(1234));
        assert_eq!(spill_pid_from_name("inbox-1-0.md"), Some(1));
    }

    #[test]
    fn spill_pid_from_name_rejects_malformed() {
        assert_eq!(spill_pid_from_name("inbox-abc-5.md"), None);
        assert_eq!(spill_pid_from_name("inbox-1234.md"), None);
        assert_eq!(spill_pid_from_name("notes.md"), None);
        assert_eq!(spill_pid_from_name("inbox-1234-5.txt"), None);
        assert_eq!(spill_pid_from_name(""), None);
    }

    #[test]
    fn pid_alive_true_for_self() {
        assert!(pid_alive(std::process::id()));
    }

    #[test]
    fn pid_alive_false_for_unlikely_pid() {
        // PIDs are 32-bit on Linux but the kernel caps pid_max well below u32::MAX; this value
        // will never be a real running process.
        assert!(!pid_alive(u32::MAX));
    }

    /// Build a fake `~/.claude/exo` layout under a tempdir and assert `classify_dead_runs` finds
    /// exactly the dead one — the current run and a fresh (recently-heartbeating) run both survive.
    #[test]
    fn classify_dead_runs_hermetic_tempdir() {
        let home = std::env::temp_dir().join(format!(
            "exo-doctor-test-runs-{}-{}",
            std::process::id(),
            line!()
        ));
        std::fs::remove_dir_all(&home).ok();
        let now = SystemTime::now();

        let write_status = |run_id: &str, secs_old: u64| {
            let dir = home.join(".claude/exo/status").join(run_id);
            std::fs::create_dir_all(&dir).unwrap();
            let file = dir.join("pane-1.json");
            std::fs::write(&file, b"{}").unwrap();
            std::fs::File::options()
                .write(true)
                .open(&file)
                .unwrap()
                .set_modified(secs_ago(now, secs_old))
                .unwrap();
        };

        write_status("dead-run", STALE_RUN_THRESHOLD.as_secs() + 3600);
        write_status("fresh-run", 30);
        write_status("current-run", STALE_RUN_THRESHOLD.as_secs() + 3600);

        let dead = classify_dead_runs(&home, Some("current-run"), now);
        assert_eq!(dead.len(), 1);
        assert_eq!(dead[0].id, "dead-run");
        assert!(home.join(".claude/exo/status/fresh-run").exists());
        assert!(home.join(".claude/exo/status/current-run").exists());

        std::fs::remove_dir_all(&home).ok();
    }

    #[test]
    fn classify_dead_runs_no_status_dir_falls_back_to_inbox_papers_mtime() {
        let home = std::env::temp_dir().join(format!(
            "exo-doctor-test-runs-nostatus-{}-{}",
            std::process::id(),
            line!()
        ));
        std::fs::remove_dir_all(&home).ok();
        let now = SystemTime::now();

        let inbox_dir = home.join(".claude/exo/inboxes/orphan-run");
        std::fs::create_dir_all(&inbox_dir).unwrap();
        let inbox_file = inbox_dir.join("pane-1.jsonl");
        std::fs::write(&inbox_file, b"{}").unwrap();
        std::fs::File::options()
            .write(true)
            .open(&inbox_file)
            .unwrap()
            .set_modified(secs_ago(now, STALE_RUN_THRESHOLD.as_secs() + 3600))
            .unwrap();

        let dead = classify_dead_runs(&home, None, now);
        assert_eq!(dead.len(), 1);
        assert_eq!(dead[0].id, "orphan-run");

        std::fs::remove_dir_all(&home).ok();
    }

    #[test]
    fn classify_dead_spill_files_hermetic_tempdir() {
        let tmp = std::env::temp_dir().join(format!(
            "exo-doctor-test-spill-{}-{}",
            std::process::id(),
            line!()
        ));
        std::fs::remove_dir_all(&tmp).ok();
        std::fs::create_dir_all(&tmp).unwrap();

        // Dead: pid u32::MAX will never be alive.
        std::fs::write(tmp.join("inbox-4294967295-0.md"), b"dead spill").unwrap();
        // Alive: this test process's own pid.
        std::fs::write(
            tmp.join(format!("inbox-{}-1.md", std::process::id())),
            b"live spill",
        )
        .unwrap();
        // Not a spill file at all — must be skipped, never deleted.
        std::fs::write(tmp.join("notes.md"), b"unrelated").unwrap();

        let dead = classify_dead_spill_files(&tmp);
        assert_eq!(dead.len(), 1);
        assert!(dead[0].path.ends_with("inbox-4294967295-0.md"));

        std::fs::remove_dir_all(&tmp).ok();
    }
}
