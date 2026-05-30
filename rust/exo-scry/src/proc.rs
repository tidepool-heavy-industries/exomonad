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
    for hops in 0..MAX_WALK {
        let info = match read_proc(pid) {
            Ok(i) => i,
            // The chain raced out from under us mid-walk — report progress.
            Err(ScryError::ProcessGone(_)) if pid != start => {
                return Err(ScryError::NoClaudeProcess {
                    start,
                    direction: "ancestry",
                    walked: hops,
                })
            }
            Err(e) => return Err(e),
        };
        if is_claude(&info.comm, &info.cmdline) {
            return Ok(Pid(pid));
        }
        if info.ppid <= 1 || info.ppid == pid {
            break;
        }
        pid = info.ppid;
    }
    Err(ScryError::NoClaudeProcess {
        start,
        direction: "ancestry",
        walked: MAX_WALK,
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
