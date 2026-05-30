//! Reverse-map a set of inode numbers to a directory under a root, by scanning
//! the root's immediate children and comparing `st_ino`. This sidesteps the
//! inotify `sdev` encoding mismatch entirely: within one filesystem, distinct
//! dirs have distinct inodes, so an inode hit uniquely identifies the dir.

use std::collections::HashSet;
use std::os::unix::fs::MetadataExt;
use std::path::{Path, PathBuf};

/// First immediate child directory of `root` whose inode is in `inodes`.
/// Returns `Ok(None)` if `root` doesn't exist or nothing matches.
pub fn match_dir_by_inode(root: &Path, inodes: &HashSet<u64>) -> std::io::Result<Option<PathBuf>> {
    let entries = match std::fs::read_dir(root) {
        Ok(e) => e,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(e) => return Err(e),
    };
    for entry in entries {
        let entry = entry?;
        let Ok(meta) = entry.metadata() else { continue };
        if meta.is_dir() && inodes.contains(&meta.ino()) {
            return Ok(Some(entry.path()));
        }
    }
    Ok(None)
}
