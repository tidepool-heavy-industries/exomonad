//! Parse the kernel's inotify watch bookkeeping out of `/proc/{pid}/fdinfo/*`.
//!
//! No crate types these lines reliably, and crucially the `sdev:` field uses a
//! kernel encoding that does NOT equal `stat`'s `st_dev`. So we deliberately
//! collect only the watched *inode* numbers and disambiguate later by matching
//! them against the inodes of known candidate directories (within one
//! filesystem an inode is unique, so a hit is unambiguous).

use crate::error::{Result, ScryError};
use std::collections::HashSet;
use std::io::{BufRead, BufReader};

/// Every inode number this process holds an inotify watch on (union across all
/// of its inotify fds).
pub fn watched_inodes(pid: i32) -> Result<HashSet<u64>> {
    let dir = format!("/proc/{pid}/fdinfo");
    let entries = match std::fs::read_dir(&dir) {
        Ok(e) => e,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Err(ScryError::ProcessGone(pid)),
        Err(e) if e.kind() == std::io::ErrorKind::PermissionDenied => {
            return Err(ScryError::PermissionDenied(pid))
        }
        Err(e) => return Err(ScryError::Io(e)),
    };

    let mut inodes = HashSet::new();
    for entry in entries {
        let Ok(entry) = entry else { continue };
        // fds churn constantly; a vanished/unreadable one is normal, not fatal.
        let Ok(file) = std::fs::File::open(entry.path()) else {
            continue;
        };
        for line in BufReader::new(file).lines() {
            let Ok(line) = line else { break };
            if let Some(ino) = parse_inotify_inode(&line) {
                inodes.insert(ino);
            }
        }
    }
    Ok(inodes)
}

/// Parse a watched inode out of an fdinfo line of the form:
/// `inotify wd:1 ino:abcd sdev:800012 mask:... ...`  (`ino` is hex).
fn parse_inotify_inode(line: &str) -> Option<u64> {
    let rest = line.strip_prefix("inotify ")?;
    let tok = rest.split_whitespace().find_map(|t| t.strip_prefix("ino:"))?;
    u64::from_str_radix(tok, 16).ok()
}

#[cfg(test)]
mod tests {
    use super::parse_inotify_inode;

    #[test]
    fn parses_hex_inode() {
        let line = "inotify wd:3 ino:1a2b3c sdev:800012 mask:3c6 ignored_mask:0";
        assert_eq!(parse_inotify_inode(line), Some(0x1a2b3c));
    }

    #[test]
    fn ignores_non_inotify_lines() {
        assert_eq!(parse_inotify_inode("pos:\t0"), None);
        assert_eq!(parse_inotify_inode("flags:\t02000002"), None);
    }
}
