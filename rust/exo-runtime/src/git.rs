//! `impl Git for Runtime` — local git operations.
//!
//! **Leaf R1.** Adapt exomonad-core `GitService` (`services/git.rs`). Use
//! `tokio::process::Command` (or `spawn_blocking` around the existing sync executor) —
//! NEVER block the tokio executor inside an `async fn`.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{Branch, Git, GitError};
use std::path::Path;
use tokio::process::Command;

#[async_trait]
impl Git for Runtime {
    async fn current_branch(&self) -> Result<Branch, GitError> {
        let output = self.git(&["rev-parse", "--abbrev-ref", "HEAD"]).await?;
        let s = String::from_utf8_lossy(&output.stdout).trim().to_string();
        Branch::new(s).map_err(|e| GitError::Failed {
            op: "current_branch",
            detail: e.to_string(),
        })
    }

    async fn head_sha(&self) -> Result<String, GitError> {
        let output = self.git(&["rev-parse", "HEAD"]).await?;
        Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
    }

    async fn merge_base(&self, refish: &str) -> Result<Option<String>, GitError> {
        // Run the command directly (not via `self.git`) so a non-zero exit — `refish` doesn't
        // resolve, or HEAD and `refish` share no history — is `Ok(None)` (caller falls back to
        // another base), not an error.
        let output = Command::new("git")
            .current_dir(self.working_dir())
            .args(["merge-base", "HEAD", refish])
            .output()
            .await?;
        if !output.status.success() {
            return Ok(None);
        }
        let sha = String::from_utf8_lossy(&output.stdout).trim().to_string();
        Ok((!sha.is_empty()).then_some(sha))
    }

    async fn is_clean(&self) -> Result<bool, GitError> {
        let output = self.git(&["status", "--porcelain"]).await?;
        Ok(String::from_utf8_lossy(&output.stdout).trim().is_empty())
    }

    async fn fetch(&self) -> Result<(), GitError> {
        self.git(&["fetch"]).await?;
        Ok(())
    }

    async fn merge(&self, branch: &Branch) -> Result<(), GitError> {
        // Local fold of a child's branch into this node's branch; non-interactive so a clean
        // merge commit doesn't block on $EDITOR.
        match self.git(&["merge", "--no-edit", branch.as_str()]).await {
            Ok(_) => Ok(()),
            Err(e) => {
                // A conflict (or other failure) leaves the worktree half-merged (MERGE_HEAD +
                // markers), which would then wedge this node's own stop clean-gate. Restore a
                // clean tree before surfacing the error so the parent stays un-wedged and can
                // re-decompose. Best-effort: ignore the abort's own result.
                let _ = self.git(&["merge", "--abort"]).await;
                Err(match e {
                    GitError::Failed { detail, .. } => GitError::Failed {
                        op: "merge",
                        detail,
                    },
                    other => other,
                })
            }
        }
    }

    async fn worktree_add(&self, branch: &Branch, at: &Path) -> Result<(), GitError> {
        self.git(&[
            "worktree",
            "add",
            "-b",
            branch.as_str(),
            &at.to_string_lossy(),
        ])
        .await?;
        Ok(())
    }

    async fn worktree_remove(&self, at: &Path) -> Result<(), GitError> {
        self.git(&["worktree", "remove", &at.to_string_lossy()])
            .await?;
        Ok(())
    }
}

impl Runtime {
    async fn git(&self, args: &[&str]) -> Result<std::process::Output, GitError> {
        let output = Command::new("git")
            .current_dir(self.working_dir())
            .args(args)
            .output()
            .await?;

        if !output.status.success() {
            return Err(GitError::Failed {
                op: "git",
                detail: String::from_utf8_lossy(&output.stderr).trim().to_string(),
            });
        }

        Ok(output)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use exo_caps::{AgentName, NodePath, PaneId};
    use std::process::Command;
    use tempfile::tempdir;

    fn run_git(dir: &std::path::Path, args: &[&str]) {
        let ok = Command::new("git")
            .current_dir(dir)
            .args(args)
            .status()
            .unwrap()
            .success();
        assert!(ok, "git {:?} failed", args);
    }

    fn runtime_at(dir: &std::path::Path) -> Runtime {
        Runtime::new(
            NodePath::new(vec![AgentName::new("root".into()).unwrap()]).unwrap(),
            Branch::new("main".into()).unwrap(),
            dir.to_path_buf(),
            None,
            "run".into(),
            "session".into(),
            PaneId::new("%1".into()).unwrap(),
        )
    }

    fn init_repo(dir: &std::path::Path) {
        run_git(dir, &["init", "-q", "-b", "main"]);
        run_git(dir, &["config", "user.email", "t@t"]);
        run_git(dir, &["config", "user.name", "t"]);
        std::fs::write(dir.join("f.txt"), "base\n").unwrap();
        run_git(dir, &["add", "f.txt"]);
        run_git(dir, &["commit", "-q", "-m", "base"]);
    }

    #[tokio::test]
    async fn merge_conflict_aborts_and_leaves_clean_tree() {
        let dir = tempdir().unwrap();
        let p = dir.path();
        init_repo(p);
        // feature: conflicting change
        run_git(p, &["checkout", "-q", "-b", "feature"]);
        std::fs::write(p.join("f.txt"), "feature\n").unwrap();
        run_git(p, &["commit", "-q", "-am", "feature"]);
        // main: conflicting change
        run_git(p, &["checkout", "-q", "main"]);
        std::fs::write(p.join("f.txt"), "main\n").unwrap();
        run_git(p, &["commit", "-q", "-am", "main"]);

        let rt = runtime_at(p);
        match rt.merge(&Branch::new("feature".into()).unwrap()).await {
            Err(GitError::Failed { op, .. }) => assert_eq!(op, "merge"),
            other => panic!("expected a labelled merge failure, got {:?}", other),
        }
        // The conflict was --abort'd: the worktree is clean again (stop-gate won't wedge).
        assert!(
            rt.is_clean().await.unwrap(),
            "worktree should be clean after an aborted conflicting merge"
        );
    }

    #[tokio::test]
    async fn merge_succeeds_brings_in_branch() {
        let dir = tempdir().unwrap();
        let p = dir.path();
        init_repo(p);
        run_git(p, &["checkout", "-q", "-b", "feature"]);
        std::fs::write(p.join("g.txt"), "new\n").unwrap();
        run_git(p, &["add", "g.txt"]);
        run_git(p, &["commit", "-q", "-m", "feature"]);
        run_git(p, &["checkout", "-q", "main"]);

        let rt = runtime_at(p);
        rt.merge(&Branch::new("feature".into()).unwrap())
            .await
            .unwrap();
        assert!(
            p.join("g.txt").exists(),
            "merge should fold in feature's file"
        );
        assert!(rt.is_clean().await.unwrap());
    }
}
