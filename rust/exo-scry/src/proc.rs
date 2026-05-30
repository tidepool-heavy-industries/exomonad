//! Process-tree navigation over `/proc` via `procfs` (Linux fast-path).
//!
//! The portable version of this layer would sit behind a `ProcessTree` trait
//! backed by `sysinfo`; today this is the Linux implementation.

use crate::error::{Result, ScryError};
use crate::identity::Pid;
use std::collections::{HashMap, HashSet, VecDeque};

/// Cap the ancestry/subtree walks so a pathological ppid cycle can't loop.
const MAX_WALK: usize = 64;

/// Is this process a Claude Code instance? CC's binary surfaces as comm
/// `.claude-unwrapped` (kernel truncates comm to 15 chars → `.claude-unwrapp`)
/// or a cmdline whose argv0 contains `claude-unwrapped`/`claude-code`.
fn is_claude(comm: &str, cmdline: &[String]) -> bool {
    if comm.starts_with(".claude") || comm == "claude" {
        return true;
    }
    cmdline
        .first()
        .map(|argv0| argv0.contains("claude-unwrapped") || argv0.contains("claude-code"))
        .unwrap_or(false)
}

struct ProcInfo {
    ppid: i32,
    comm: String,
    cmdline: Vec<String>,
}

fn read_proc(pid: i32) -> Result<ProcInfo> {
    let p = procfs::process::Process::new(pid).map_err(|e| ScryError::from_proc(pid, e))?;
    let stat = p.stat().map_err(|e| ScryError::from_proc(pid, e))?;
    // cmdline is legitimately empty for kernel threads/zombies — not fatal.
    let cmdline = p.cmdline().unwrap_or_default();
    Ok(ProcInfo {
        ppid: stat.ppid,
        comm: stat.comm,
        cmdline,
    })
}

/// Walk *up* the parent chain from `start` to the nearest Claude Code ancestor.
/// Used for `--self` and `--pid` (a tool/shell's owning session is its ancestor).
pub fn find_claude_ancestor(start: i32) -> Result<Pid> {
    let mut pid = start;
    let mut walked = 0;
    while walked < MAX_WALK {
        let info = match read_proc(pid) {
            Ok(i) => i,
            // The chain raced out from under us mid-walk — report progress.
            Err(ScryError::ProcessGone(_)) if pid != start => {
                return Err(ScryError::NoClaudeProcess {
                    start,
                    direction: "ancestry",
                    walked,
                })
            }
            Err(e) => return Err(e),
        };
        walked += 1;
        if is_claude(&info.comm, &info.cmdline) {
            return Ok(Pid(pid));
        }
        // Reached init (ppid 0/1) or a self-parenting root without a hit.
        if info.ppid <= 1 || info.ppid == pid {
            break;
        }
        pid = info.ppid;
    }
    Err(ScryError::NoClaudeProcess {
        start,
        direction: "ancestry",
        walked,
    })
}

/// Walk *down* from `root` (e.g. a tmux pane's shell) to the Claude Code
/// process running inside it. Used for `--pane`. Returns the nearest Claude
/// descendant (the session, not any nested subagent).
pub fn find_claude_descendant(root: i32) -> Result<Pid> {
    let all = procfs::process::all_processes().map_err(|e| ScryError::from_proc(root, e))?;
    let mut children: HashMap<i32, Vec<i32>> = HashMap::new();
    let mut claude: HashSet<i32> = HashSet::new();
    for pr in all {
        let Ok(pr) = pr else { continue };
        let pid = pr.pid;
        let Ok(stat) = pr.stat() else { continue };
        children.entry(stat.ppid).or_default().push(pid);
        let cmdline = pr.cmdline().unwrap_or_default();
        if is_claude(&stat.comm, &cmdline) {
            claude.insert(pid);
        }
    }
    let mut queue = VecDeque::from([root]);
    let mut seen = HashSet::new();
    while let Some(pid) = queue.pop_front() {
        if !seen.insert(pid) {
            continue;
        }
        if pid != root && claude.contains(&pid) {
            return Ok(Pid(pid));
        }
        if let Some(kids) = children.get(&pid) {
            queue.extend(kids);
        }
    }
    Err(ScryError::NoClaudeProcess {
        start: root,
        direction: "subtree",
        walked: seen.len(),
    })
}

/// The current process id.
pub fn self_pid() -> i32 {
    std::process::id() as i32
}

/// Ground-truth liveness for a tmux-backed member: does its pane still exist and
/// run a Claude process? This is observed, not claimed — the config's `isActive`
/// flag goes stale (it stays `true` when a pane is killed without a clean
/// shutdown). A vanished pane, or a pane with no Claude in it (e.g. a spawn whose
/// launch command was corrupted), both read as not-live.
pub fn pane_has_live_claude(pane: &str) -> bool {
    if pane.is_empty() {
        return false;
    }
    match crate::tmux::pane_pid(pane) {
        Ok(pid) => find_claude_descendant(pid).is_ok(),
        Err(_) => false,
    }
}

/// The working directory of a process (`/proc/{pid}/cwd`). On macOS the portable
/// equivalent is `proc_pidinfo(PROC_PIDVNODEPATHINFO)`.
pub fn process_cwd(pid: i32) -> Result<std::path::PathBuf> {
    match std::fs::read_link(format!("/proc/{pid}/cwd")) {
        Ok(p) => Ok(p),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Err(ScryError::ProcessGone(pid)),
        Err(e) if e.kind() == std::io::ErrorKind::PermissionDenied => {
            Err(ScryError::PermissionDenied(pid))
        }
        Err(e) => Err(ScryError::Io(e)),
    }
}

/// Every live Claude Code process whose cwd equals `cwd`. More than one means
/// the transcript signal can't map a cwd to a single session (the caller should
/// fall back to the per-process inotify path on Linux).
pub fn claude_pids_with_cwd(cwd: &std::path::Path) -> Result<Vec<i32>> {
    let all = procfs::process::all_processes()
        .map_err(|e| ScryError::ProcUnavailable(format!("enumerating processes: {e}")))?;
    let mut pids = Vec::new();
    for pr in all {
        let Ok(pr) = pr else { continue };
        let pid = pr.pid;
        let Ok(stat) = pr.stat() else { continue };
        let cmdline = pr.cmdline().unwrap_or_default();
        if !is_claude(&stat.comm, &cmdline) {
            continue;
        }
        // cwd reads race against exit; a vanished/forbidden proc just isn't a match.
        if process_cwd(pid).is_ok_and(|c| c == cwd) {
            pids.push(pid);
        }
    }
    Ok(pids)
}

#[cfg(test)]
mod tests {
    use super::is_claude;

    #[test]
    fn recognizes_claude_by_comm() {
        assert!(is_claude(".claude-unwrapp", &[]));
        assert!(is_claude("claude", &[]));
    }

    #[test]
    fn recognizes_claude_by_argv0() {
        assert!(is_claude(
            "node",
            &["/nix/store/x/bin/.claude-unwrapped".into()]
        ));
    }

    #[test]
    fn rejects_non_claude() {
        assert!(!is_claude("zsh", &["/bin/zsh".into()]));
        assert!(!is_claude("exomonad", &["exomonad".into(), "serve".into()]));
    }
}
