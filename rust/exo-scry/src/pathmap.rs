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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn matches_dir_by_its_real_inode() {
        let root = std::env::temp_dir().join(format!("exo-scry-pathmap-{}", std::process::id()));
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(&root).unwrap();

        let child = root.join("child_dir");
        fs::create_dir_all(&child).unwrap();

        let inode = fs::metadata(&child).unwrap().ino();
        let mut inodes = HashSet::new();
        inodes.insert(inode);

        let matched = match_dir_by_inode(&root, &inodes).unwrap();
        assert!(matched.is_some());
        assert_eq!(matched.unwrap().file_name().unwrap(), "child_dir");

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn no_match_returns_none() {
        let root =
            std::env::temp_dir().join(format!("exo-scry-pathmap-nomatch-{}", std::process::id()));
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(&root).unwrap();

        let mut inodes = HashSet::new();
        inodes.insert(999_999_999); // Unlikely inode

        let matched = match_dir_by_inode(&root, &inodes).unwrap();
        assert!(matched.is_none());

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn missing_root_is_none() {
        let root = PathBuf::from("/tmp/nonexistent-path-12345");
        let mut inodes = HashSet::new();
        inodes.insert(1);
        let matched = match_dir_by_inode(&root, &inodes).unwrap();
        assert!(matched.is_none());
    }
}
